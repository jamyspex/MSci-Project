{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module AddSmartCaches where

import           CommandLineProcessor
import           Data.Ix
import           Data.List
import           Data.List.Index
import qualified Data.Map                    as DMap
import           Data.Maybe
import qualified Data.Set                    as Set
import           Data.Tuple
import           Data.Tuple.Utils
import           Debug.Trace
import           GHC.Exts
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Safe
import           SmartCacheParameterAnalysis
import           Text.Printf
import           Utils

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
insertSmartCaches ::
     F4Opts
  -> [Kernel]
  -> IO [(Kernel, Maybe (PipelineItem SharedPipelineData))]
insertSmartCaches options kernels =
  mapM (buildSmartCacheForKernel options) kernelsAndMemoryStreamable
  where
    kernelsAndMemoryStreamable = findStreamsThatCanComeFromMemoryReader kernels

-- Now that transit streams have been added we know that all input streams that
-- are not matched with an output stream from the kernel before can come from
-- memory. So iterate through the pipeline and find any mismatched streams that
-- that we now know can be streamed directly from memory (e.g. they are not produced
-- or updated within the pipeline) and form tuples of (kernel, set of stream names
-- it can stream from memory)
findStreamsThatCanComeFromMemoryReader :: [Kernel] -> [(Kernel, Set.Set String)]
findStreamsThatCanComeFromMemoryReader kernels =
  snd $ foldl go (Set.empty, []) kernels
  where
    go ::
         (Set.Set String, [(Kernel, Set.Set String)])
      -> Kernel
      -> (Set.Set String, [(Kernel, Set.Set String)])
    go (availableFromLast, kernelsAndMemoryReadableStreams) currentKernel =
      ( availableFromThisKernel
      , kernelsAndMemoryReadableStreams ++
        [(currentKernel, inputsThatCanComeFromMemory)])
      where
        availableFromThisKernel =
          Set.fromList $ map getStreamName $ outputs currentKernel
        previousKernelOutputStreams =
          if null kernelsAndMemoryReadableStreams
            then []
            else (outputs . fst . last) kernelsAndMemoryReadableStreams
        requiredInputStreamNames = map getStreamName $ inputs currentKernel
        inputsThatCanComeFromMemory =
          Set.fromList $
          concatMap
            (\sn -> [sn | Set.notMember sn availableFromLast])
            requiredInputStreamNames

-- Take a kernel and using SmartCacheParameterAnalysis get the details
-- of any smart caches required by StencilStream inputs to the kernel.
-- Then amalgamate the details of the smart caches required by individual
-- streams to form one smart cache that can be place before the kernel
-- and will then provide it with the appropriate input streams
buildSmartCacheForKernel ::
     F4Opts
  -> (Kernel, Set.Set String)
  -> IO (Kernel, Maybe (PipelineItem SharedPipelineData))
buildSmartCacheForKernel opts (k, streamsThatCanComeFromMem) = do
  print "------------------------------------------"
  print withInputStreamsUpdated
  print "------------------------------------------"
  return
    ( withInputStreamsUpdated
    , if null smartCacheItems
        then Nothing
        else Just smartCache)
  where
    hasStencilStreams = any isStencil $ inputs k
    stencilStreamInputs = filter isStencil $ inputs k
    allStencilStreamsComeFromMem =
      all
        (\s -> getStreamName s `Set.member` streamsThatCanComeFromMem)
        stencilStreamInputs
    streamsToGoThroughSmartCache =
      filter
        (\s
        -- If all stencil streams come from memory then transit and normal streams
        -- do not need to go through the smart cache as they do not need to be
        -- kept in sync with the stencil streams as they come from a memory reader
        -- which only outputs one stream each therefore there is no deadlock issue.
          ->
           (not allStencilStreamsComeFromMem &&
            (getStreamName s `Set.notMember` streamsThatCanComeFromMem) ||
            (hasStencilStreams && isTransit s))
        -- Stencil streams always need to go through the smart cache in order
        -- have their different components generated.
            ||
           isStencil s) $
      inputs k
    streamDimensionsOrders =
      map (length . getStreamDimensions) streamsToGoThroughSmartCache
    numberOfStreamDimensions = maximum streamDimensionsOrders
    iterationOrder = defaultIterationOrder numberOfStreamDimensions
    smartCacheItems =
      map
        (buildSmartCacheItem iterationOrder k numberOfStreamDimensions)
        streamsToGoThroughSmartCache
    paddedSmartCacheItems = padCacheItems smartCacheItems
    withInputStreamsUpdated =
      updateKernelInputStreams allSmartCacheOutputStreams k
    allSmartItemInputStreams = map inputStream paddedSmartCacheItems
    allSmartCacheOutputStreams = concatMap buildStream smartCacheItems
    cacheSize = (size . headNote "line 66") paddedSmartCacheItems
    name = kernelName k ++ "_smart_cache"
    smartCache =
      SmartCache
        { inputStreams = allSmartItemInputStreams
        , outputStreams = allSmartCacheOutputStreams
        , smartCacheSize = cacheSize
        , name = name
        , cacheLines = paddedSmartCacheItems
        , nextStage = NullItem
        , readPipes = []
        , writtenPipes = []
        , sharedData = NullPipeLineData
        , stageNumber = order k
        }

-- The criteria for adding a smart cache is that
-- a kernel requires ANY stencil stream inputs that can not come
-- from memory. So check if all the input stencil stream can come from
-- memory because if they can then only they need to go through the
-- smart cache rather than all the input streams to the kernel
-- TODO This posibly needs to be more advanced when deciding which
-- streams to add to the smart cache as non stencil streams which do
-- not come from memory will need to be kept in sync with the smart
-- cache output and therefore will need to be buffered by the smart cache.
-- However, for now, I think the logic in AddTransitStreams handles this
-- so that all non stencil streams that need to remain in sync are converted
-- from Stream to StencilStream with one point (0, 0).
-- Update: it is now slightly more advanced :S
-- take a SmartCacheItem and generate output stream objects for it
buildStream :: SmartCacheItem -> [Stream Anno]
buildStream SmartCacheTransitItem {..} = [Stream name arrayName valueType dims]
  where
    (name, arrayName, valueType, dims) =
      ( getStreamName inputStream
      , getArrayNameFromStream inputStream
      , getStreamType inputStream
      , getStreamDimensions inputStream)
buildStream SmartCacheItem {..} =
  map
    (\(name, _, _) -> Stream name arrayName inputValueType inputDimensions)
    outputStreamNamesAndBufferIndex
  where
    (Stream _ arrayName inputValueType inputDimensions) = inputStream

-- Update the input streams of the kernel to no longer use stencil streams
-- and instead use the output streams from the smart cache
updateKernelInputStreams :: [Stream Anno] -> Kernel -> Kernel
updateKernelInputStreams smartCacheOutputStreams kernel =
  kernel {inputs = nonStencilInputs ++ smartCacheOutputStreams}
  where
    nonStencilInputs =
      filter (\s -> (not . isTransit) s && (not . isStencil) s) $ inputs kernel

stencilItemsOnly =
  filter
    (\case
       SmartCacheItem {} -> True
       _ -> False)

-- All the buffers in a smart cache must be the same size e.g. the size of the
-- largest buffer in order to prevent the processing pipeline deadlocking.
-- This function takes generated SmartCacheItems and pads any that are smaller
-- than the largest buffer thats going to be in the smart cache. It also updates
-- the buffer indices to valid in line with the newly padded size
padCacheItems :: [SmartCacheItem] -> [SmartCacheItem]
padCacheItems inputs = map updateCacheItem inputs
    -- inputSorted = sortBy (\x y -> size y `compare` size x) inputs
    -- largestSize = size $ headNote "line 108" inputSorted
  where
    stencilItems = stencilItemsOnly inputs
    maxPosOffset = maximum $ map maxPositiveOffset stencilItems
    maxNegOffset = maximum $ map maxNegativeOffset stencilItems
    largestSize = maximum $ map size stencilItems
    largestOriginOffset =
      snd3 $
      maximumBy (\(_, o1, _) (_, o2, _) -> o1 `compare` o2) $
      filter (\(_, _, originType) -> isOrigin originType) $
      concatMap outputStreamNamesAndBufferIndex stencilItems
    updateCacheItem :: SmartCacheItem -> SmartCacheItem
    updateCacheItem item@SmartCacheTransitItem {..} =
      SmartCacheItem
        { size = largestSize
        , inputStream = Stream name arrayName inputValueType inputDims
        , maxNegativeOffset = maxNegOffset
        , maxPositiveOffset = maxPosOffset
        , outputStreamNamesAndBufferIndex =
            [(name, largestSize - maxPosOffset, RealOrigin)]
        }
      where
        (name, arrayName, inputValueType, inputDims) =
          ( getStreamName inputStream
          , getArrayNameFromStream inputStream
          , getStreamType inputStream
          , getStreamDimensions inputStream)
    updateCacheItem org@SmartCacheItem {..} =
      if diffOffset > 0
        then updated
        else org
      where
        sizeDiff = largestSize - size
        originOffset =
          snd3 $
          headNote
            ("outputStreamNamesAndBufferIndex = " ++
             show outputStreamNamesAndBufferIndex) $
          filter
            (\(_, _, originType) -> isOrigin originType)
            outputStreamNamesAndBufferIndex
        diffOffset = largestOriginOffset - originOffset
        withOriginRemovedIfSynthetic =
          filter
            (\(_, _, originType) -> (not . isSyntheticOrigin) originType)
            outputStreamNamesAndBufferIndex
        updated =
          org
            { size = largestSize
            , outputStreamNamesAndBufferIndex =
                map
                  (\(name, idx, originType) ->
                     (name, idx + diffOffset, originType) -- idx + sizeDiff))
                   )
                  outputStreamNamesAndBufferIndex
            }

isSyntheticOrigin ot =
  case ot of
    SyntheticOrigin -> True
    _               -> False

isOrigin ot =
  case ot of
    RealOrigin      -> True
    SyntheticOrigin -> True
    NotOrigin       -> False

-- Using the stencil points match the results from SmartCacheParameterAnalysis with
-- the generated output variable names. The smart cache buffer will be a standard
-- Fortran array with 1 based indexing. This means the values from startToPointDistances
-- in the SmartCacheDetails structure can be used directly as the indices.
buildSmartCacheItem :: [Int] -> Kernel -> Int -> Stream Anno -> SmartCacheItem
buildSmartCacheItem _ _ _ stream@Stream {} =
  SmartCacheTransitItem {inputStream = stream, size = 1}
buildSmartCacheItem _ _ _ transStream@TransitStream {} =
  SmartCacheTransitItem {inputStream = transStream, size = 1}
buildSmartCacheItem iterationOrder kernel streamDimensionOrder inStream =
  SmartCacheItem
    { size = requiredBufferSize
    , inputStream = convertStencilStream inStream
    , maxPositiveOffset = maxPosOffset
    , maxNegativeOffset = maxNegOffset
    , outputStreamNamesAndBufferIndex = pointsAndVarNames
    }
  where
    outputVars = getSmartCacheOutputVars kernel inStream
    s@SmartCacheDetailsForStream {..} =
      calculateSmartCacheDetailsForStream (Just kernel) iterationOrder inStream
    pointToStreamNameMap = DMap.fromList outputVars
    pointsAndVarNames =
      foldl
        (\acc (point, buffIndex) ->
           ( synthenticOriginPointNameWorkaround
               (originType point)
               (map Offset point)
               pointToStreamNameMap --  pointToStreamNameMap DMap.! map Offset point
           , buffIndex
           , originType point) :
           acc)
        []
        pointsAndDistances
    originType point =
      if point == originPoint
        then if centralPointIsSynthetic
               then SyntheticOrigin
               else RealOrigin
        else NotOrigin
    originPoint = replicate (length $ getStreamDimensions inStream) 0
    pointsAndDistances =
      (startIndex, 1) :
      trace
        ("Smart cache details of stream:\n" ++ show inStream ++ "\n" ++ show s)
        startToPointDistances

-- god this is getting hairy
-- If its a synthetic origin then it won't have appeared as a stencil index
-- therefore there won't be a stream name for it in the map
-- This doesn't matter because its only used in padding calculation and then
-- discarded so just return an empty string for the stream name if it is
-- an artifical origin point and trust it all works out in the end
synthenticOriginPointNameWorkaround ::
     StencilOriginType
  -> [StencilIndex]
  -> DMap.Map [StencilIndex] String
  -> String
synthenticOriginPointNameWorkaround SyntheticOrigin stencilIdx map = ""
synthenticOriginPointNameWorkaround _ stencilIdx map = map DMap.! stencilIdx

-- build variable names for output streams based on stencil points
getSmartCacheOutputVars :: Kernel -> Stream Anno -> [([StencilIndex], String)]
getSmartCacheOutputVars kern (StencilStream _ arrayName _ dims sten) =
  getOutputVariableNames loopVarPos sten
  where
    loopVarPos = getLoopVarPositions arrayName kern

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
getLoopVarPositions stencilArrayName kern@Kernel {..} =
  map (\(_, loopV, mPos) -> (loopV, mPos)) $
  getLoopIndexPosition loopVars arrayAccess
  where
    stencilArray =
      filter
        (\arr ->
           let (VarName _ name) = arrayVarName arr
            in name == stencilArrayName) $
      map arrayFromDecl $ getArrayDecls body
    arrayAccess =
      headNote "line 221" $ getAllArrayAccesses stencilArray (getSubBody body)
