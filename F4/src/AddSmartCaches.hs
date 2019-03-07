{-# LANGUAGE RecordWildCards #-}

module AddSmartCaches where

import           Data.Ix
import           Data.List
import           Data.List.Index
import qualified Data.Map                    as DMap
import           Data.Maybe
import           Data.Tuple
import           Debug.Trace
import           GHC.Exts
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Pipeline
import           SmartCacheParameterAnalysis
import           Text.Printf
import           Utils

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
insertSmartCaches ::
     [Kernel] -> IO [(Kernel, Maybe (PipelineItem SharedPipelineData))]
insertSmartCaches = mapM buildSmartCacheForKernel

-- Take a kernel and using SmartCacheParameterAnalysis get the details
-- of any smart caches required by StencilStream inputs to the kernel.
-- Then amalgamate the details of the smart caches required by individual
-- streams to form one smart cache that can be place before the kernel
-- and will then provide it with the appropriate input streams
buildSmartCacheForKernel ::
     Kernel -> IO (Kernel, Maybe (PipelineItem SharedPipelineData))
buildSmartCacheForKernel k = do
  print "------------------------------------------"
 -- print k
 -- mapM_ print smartCacheItems
 -- print "======== Padded items below =============="
 -- mapM_ print paddedSmartCacheItems
  print withInputStreamsUpdated
  print "------------------------------------------"
  return
    ( withInputStreamsUpdated
    , if null smartCacheItems
        then Nothing
        else Just smartCache)
  where
    requiredStencilStreams =
      filter (\s -> isTransit s || isStencil s) $ inputs k
    -- TODO This posibly needs to be more advanced when deciding which
    -- streams to add to the smart cache as non stencil streams which do
    -- not come from memory will need to be kept in sync with the smart
    -- cache output and therefore will need to be buffered by the smart cache.
    -- However, for now, I think the logic in AddTransitStreams handles this
    -- so that all non stencil streams that need to remain in sync are converted
    -- from Stream to StencilStream with one point (0, 0).
    -- Update: it is now slightly more advanced :S
    streamDimensionsOrders =
      map (length . getStreamDimensions) requiredStencilStreams
    numberOfStreamDimensions =
      if all (== head streamDimensionsOrders) streamDimensionsOrders
        then head streamDimensionsOrders
        else error "Streams of different dimension orders present in kernel"
    smartCacheItems =
      map
        (buildSmartCacheItem k numberOfStreamDimensions)
        requiredStencilStreams
    paddedSmartCacheItems = padCacheItems smartCacheItems
    withInputStreamsUpdated =
      updateKernelInputStreams allSmartCacheOutputStreams k
    allSmartItemInputStreams = map inputStream paddedSmartCacheItems
    allSmartCacheOutputStreams = concatMap buildStream smartCacheItems
    cacheSize = (size . head) paddedSmartCacheItems
    name = kernelName k ++ "_smart_cache"
    smartCache =
      SmartCache
        { inputStreams = allSmartItemInputStreams
        , outputStreams = allSmartCacheOutputStreams
        , smartCacheSize = cacheSize
        , name = name
        , cacheLines = paddedSmartCacheItems
        , nextStage = NullItem
        , sharedData = NullPipeLineData
        }

-- take a SmartCacheItem and generate output stream objects for it
buildStream :: SmartCacheItem -> [Stream Anno]
buildStream SmartCacheTransitItem {..} = [Stream name valueType dims]
  where
    (TransitStream name valueType dims) = inputStream
buildStream SmartCacheItem {..} =
  map
    (\(name, _) -> Stream name inputValueType inputDimensions)
    outputStreamNamesAndBufferIndex
  where
    (Stream _ inputValueType inputDimensions) = inputStream

-- Update the input streams of the kernel to no longer use stencil streams
-- and instead use the output streams from the smart cache
updateKernelInputStreams :: [Stream Anno] -> Kernel -> Kernel
updateKernelInputStreams smartCacheOutputStreams kernel =
  kernel {inputs = nonStencilInputs ++ smartCacheOutputStreams}
  where
    nonStencilInputs =
      filter (\s -> (not . isTransit) s && (not . isStencil) s) $ inputs kernel

-- All the buffers in a smart cache must be the same size e.g. the size of the
-- largest buffer in order to prevent the processing pipeline deadlocking.
-- This function takes generated SmartCacheItems and pads any that are smaller
-- than the largest buffer thats going to be in the smart cache. It also updates
-- the buffer indices to valid in line with the newly padded size
padCacheItems :: [SmartCacheItem] -> [SmartCacheItem]
padCacheItems inputs = map updateCacheItem inputs
  where
    inputSorted = sortBy (\x y -> size y `compare` size x) inputs
    largestSize = size $ head inputSorted
    updateCacheItem :: SmartCacheItem -> SmartCacheItem
    updateCacheItem SmartCacheTransitItem {..} =
      SmartCacheItem
        { size = largestSize
        , inputStream = Stream name inputValueType inputDims
        , outputStreamNamesAndBufferIndex = [(name, largestSize)]
        }
      where
        (TransitStream name inputValueType inputDims) = inputStream
    updateCacheItem org@SmartCacheItem {..} =
      if sizeDiff > 0
        then updated
        else org
      where
        sizeDiff = largestSize - size
        updated =
          org
            { size = largestSize
            , outputStreamNamesAndBufferIndex =
                map
                  (\(name, idx) -> (name, idx + sizeDiff))
                  outputStreamNamesAndBufferIndex
            }

-- Using the stencil points match the results from SmartCacheParameterAnalysis with
-- the generated output variable names. The smart cache buffer will be a standard
-- Fortran array with 1 based indexing. This means the values from startToPointDistances
-- in the SmartCacheDetails structure can be used directly as the indices.
buildSmartCacheItem :: Kernel -> Int -> Stream Anno -> SmartCacheItem
buildSmartCacheItem _ _ transStream@TransitStream {} =
  SmartCacheTransitItem {inputStream = transStream, size = 1}
buildSmartCacheItem kernel streamDimensionOrder inStream =
  SmartCacheItem
    { size = requiredBufferSize
    , inputStream = convertStencilStream inStream
    , outputStreamNamesAndBufferIndex = pointsAndVarNames
    }
  where
    outputVars = getSmartCacheOutputVars kernel inStream
    SmartCacheDetailsForStream {..} =
      calculateSmartCacheDetailsForStream
        (defaultIterationOrder streamDimensionOrder)
        inStream
    pointToStreamNameMap = DMap.fromList outputVars
    pointsAndVarNames =
      foldl
        (\acc (point, buffIndex) ->
           (pointToStreamNameMap DMap.! map Offset point, buffIndex) : acc)
        []
        ((startIndex, 1) : startToPointDistances)

-- build variable names for output streams based on stencil points
getSmartCacheOutputVars :: Kernel -> Stream Anno -> [([StencilIndex], String)]
getSmartCacheOutputVars kern (StencilStream name _ dims sten) =
  getOutputVariableNames loopVarPos sten
  where
    loopVarPos = getLoopVarPositions name kern

-- 1) Get the loop vars used to index an array
-- 2) Order them by the order they are used to access the array e.g. eta(j, k) -> [j, k]
-- 3) Using the Stencil used to access that array generate the output variables
-- a smart cache buffering that array needs to emit
getOutputVariableNames ::
     [(String, Maybe Int)] -> Stencil Anno -> [([StencilIndex], String)]
getOutputVariableNames loopVarsAndPosition (Stencil _ _ _ stenIndices (VarName _ name)) =
  outputVariables
  where
    loopVarPosMap =
      DMap.fromList $
      map (swap . (\(f, s) -> (f, fromJust s))) $
      filter (isJust . snd) loopVarsAndPosition
    outputVariables =
      map (\si -> (si, snd $ foldl buildVarName (0, name) si)) stenIndices
    buildVarName :: (Int, String) -> StencilIndex -> (Int, String)
    buildVarName (pos, outVarName) cur =
      case cur of
        Offset _ ->
          ( pos + 1
          , outVarName ++
            "_" ++ (loopVarPosMap DMap.! pos) ++ convertStenIdx cur)
        _ -> (pos + 1, outVarName ++ "_" ++ convertStenIdx cur)
    convertStenIdx (Offset val)
      | val == 0 = ""
      | val < 0 = "m" ++ (show . abs) val
      | val > 0 = "p" ++ show val
    convertStenIdx (Constant val) = show val

-- Get the position loopvars are used in the kernel body in a
-- specific stencil. At this point we have already check that
-- loop var usage is consistent across arrays in a kernel thanks to
-- validateIndexingAndMakeUnique in AddrnelLoopGuards.hs
getLoopVarPositions :: String -> Kernel -> [(String, Maybe Int)]
getLoopVarPositions stencilArrayName Kernel {..} =
  map (\(_, loopV, mPos) -> (loopV, mPos)) $
  getLoopIndexPosition loopVars arrayAccess
  where
    stencilArray =
      filter
        (\arr ->
           let (VarName _ name) = varName arr
            in name == stencilArrayName) $
      map arrayFromDecl $ getArrayDecls body
    arrayAccess = head $ getArrayReads stencilArray (getSubBody body)
