{-# LANGUAGE RecordWildCards #-}

module AddSmartCaches where

import           Data.Ix
import           Data.List
import           Data.List.Index
import qualified Data.Map                      as DMap
import           Data.Maybe
import           Data.Tuple
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           SmartCacheParameterAnalysis
import           Text.Printf
import           Utils

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
insertSmartCaches :: [Kernel] -> IO ()
insertSmartCaches = mapM_ buildSmartCacheForKernel

-- Take a kernel and using SmartCacheParameterAnalysis get the details
-- of any smart caches required by StencilStream inputs to the kernel.
-- Then amalgamate the details of the smart caches required by individual
-- streams to form one smart cache that can be place before the kernel
-- and will then provide it with the appropriate input streams
buildSmartCacheForKernel :: Kernel -> IO () -- (rnel, Pipeline)
buildSmartCacheForKernel k = do
  print k
  mapM_ print smartCacheItems
 where
  requiredStencilStreams = filter isStencil $ inputs k
  streamDimensionsOrders =
    map (length . getStreamDimensions) requiredStencilStreams
  numberOfStreamDimensions =
    if all (== head streamDimensionsOrders) streamDimensionsOrders
      then head streamDimensionsOrders
      else error "Streams of different dimension orders present in kernel"
  smartCacheItems = sortOn size $ map
    (buildSmartCacheItem k numberOfStreamDimensions)
    requiredStencilStreams
  largestSmartCacheRequired = head smartCacheItems


data SmartCacheItem = SmartCacheItem
  {
    size                            :: Int,
    inputStream                     :: Stream Anno,
    outputStreamNamesAndBufferIndex :: [(String, Int)]
  }

instance Show SmartCacheItem where
  show SmartCacheItem{..} = "-------------------------------\n" ++
    "Smart cache item\n" ++
    "Input stream: " ++ name ++ "\n" ++
    "Buffer size: " ++ show size ++ "\n" ++
    "Stream Dimensions: " ++ show dims ++ "\n" ++
    "Output Streams:\n" ++
    concatMap (\(name, bufIdx) -> "\t" ++ name ++ " from buffer index = " ++ show bufIdx ++ "\n") outputStreamNamesAndBufferIndex ++
    "-------------------------------\n"
    where
      (StencilStream name _ dims _)= inputStream

-- Using the stencil points match the results from SmartCacheParameterAnalysis with
-- the generated output variable names. The smart cache buffer will be a standard
-- Fortran array with 1 based indexing. This means the values from startToPointDistances
-- in the SmartCacheDetails structure can be used directly as the indices.
buildSmartCacheItem :: Kernel -> Int -> Stream Anno -> SmartCacheItem
buildSmartCacheItem kernel streamDimensionOrder inStream = SmartCacheItem
  { size                            = requiredBufferSize
  , inputStream                     = inStream
  , outputStreamNamesAndBufferIndex = pointsAndVarNames
  }
 where
  outputVars                      = getSmartCacheOutputVars kernel inStream
  SmartCacheDetailsForStream {..} = calculateSmartCacheDetailsForStream
    (defaultIterationOrder streamDimensionOrder)
    inStream
  pointToStreamNameMap = DMap.fromList outputVars
  pointsAndVarNames    = foldl
    (\acc (point, buffIndex) ->
      (pointToStreamNameMap DMap.! map Offset point, buffIndex) : acc
    )
    []
    ((startIndex, 1) : startToPointDistances)

isStencil stream = case stream of
  Stream{}        -> False
  StencilStream{} -> True

-- build variable names for output streams based on stencil points
getSmartCacheOutputVars :: Kernel -> Stream Anno -> [([StencilIndex], String)]
getSmartCacheOutputVars kern (StencilStream name _ dims sten) =
  getOutputVariableNames loopVarPos sten
  where loopVarPos = getLoopVarPositions name kern

-- 1) Get the loop vars used to index an array
-- 2) Order them by the order they are used to access the array e.g. eta(j, k) -> [j, k]
-- 3) Using the Stencil used to access that array generate the output variables
-- a smart cache buffering that array needs to emit
getOutputVariableNames
  :: [(String, Maybe Int)] -> Stencil Anno -> [([StencilIndex], String)]
getOutputVariableNames loopVarsAndPosition (Stencil _ _ _ stenIndices (VarName _ name))
  = outputVariables
 where
  loopVarPosMap =
    DMap.fromList $ map (swap . (\(f, s) -> (f, fromJust s))) $ filter
      (isJust . snd)
      loopVarsAndPosition
  outputVariables =
    map (\si -> (si, snd $ foldl buildVarName (0, name) si)) stenIndices
  buildVarName :: (Int, String) -> StencilIndex -> (Int, String)
  buildVarName (pos, outVarName) cur = case cur of
    Offset _ ->
      ( pos + 1
      , outVarName ++ "_" ++ (loopVarPosMap DMap.! pos) ++ convertStenIdx cur
      )
    _ -> (pos + 1, outVarName ++ "_" ++ convertStenIdx cur)
  convertStenIdx (Offset val) | val == 0 = ""
                              | val < 0  = "m" ++ (show . abs) val
                              | val > 0  = "p" ++ show val
  convertStenIdx (Constant val) = show val

-- Get the position loopvars are used in the kernel body in a
-- specific stencil. At this point we have already check that
-- loop var usage is consistent across arrays in a kernel thanks to
-- validateIndexingAndMakeUnique in AddrnelLoopGuards.hs
getLoopVarPositions :: String -> Kernel -> [(String, Maybe Int)]
getLoopVarPositions stencilArrayName Kernel {..} =
  map (\(_, loopV, mPos) -> (loopV, mPos))
    $ getLoopIndexPosition loopVars arrayAccess
 where
  stencilArray
    = filter
        (\arr -> let (VarName _ name) = varName arr in name == stencilArrayName
        )
      $ map arrayFromDecl
      $ getArrayDecls body
  arrayAccess = head $ getArrayReads stencilArray (getSubBody body)
