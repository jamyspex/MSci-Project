{-# Language RecordWildCards, TupleSections #-}

module ScalarizeKernels where

import           Data.Generics
import           LanguageFortranTools
import           Language.Fortran
import           AddMemoryAccessKernels
import           Utils
import           Pipeline
import qualified Data.Map                      as DMap
import qualified Data.Set                      as Set

-- This module is going to walk through the pipeline finding the pipes
-- that are required. 
-- Then it wil add code to the kernels to read the pipes and assign the
-- value read to a local variable 
-- Then it will subsitute the new local variable in place of any previous 
-- array read expressions. 

scalarize :: [PipelineStage] -> IO [PipelineStage]
scalarize pipeline = do
  getRequiredPipesAndInsertAccessCode pipeline
  return pipeline


-- This function folds over the pipeline finding the pipes that 
-- are required at by each stage. It also builds up a list of all 
-- the pipes it has assumed exist so that they can be emitted when 
-- it is time to emit the device code. 
-- To make sure we detect all the pipes in the pipeline the following 
-- steps have to be taken
-- 	1) look for pipes output from memory readers
-- 	2) look for pipes input to memory writers
-- 	3) look for pipes output from smart caches
-- 	4) look for pipes input to smart caches
-- 	5) look for pipes input to the kernel directly
getRequiredPipesAndInsertAccessCode
  :: [PipelineStage] -> IO ([PipelineStage], [Pipe])
getRequiredPipesAndInsertAccessCode pipeline = do
  mapM_ print requiredPipes
  return requiredPipes
 where
  requiredPipes = foldl go ([], []) pipeline
  go :: ([PipelineStage], [Pipe]) -> PipelineStage -> ([PipelineStage], [Pipe])
  go (newPipeline, pipes) (kernel, smartCache, memAccess) = ([], pipes ++ temp)
   where
    (previousKernel, _, _) = last newPipeline
    previousKernelName     = kernelName previousKernel
    memoryAccessPipeStreamPairs =
      concatMap (getMemoryAccessPipes (kernelName kernel)) memAccess
    smartCachePipeStreamPairs = maybe
      []
      (getSmartCachePipes previousKernelName (kernelName kernel))
      smartCache
    processedStreams = Set.fromList $ map
      (getStreamName . fst)
      (smartCachePipeStreamPairs ++ memoryAccessPipeStreamPairs)
    -- kernelPipeStreamPairs =
    --   getKernelInputPipes previousKernelName processedStreams kernel
    requiredPipeStreamPairs =
      memoryAccessPipeStreamPairs ++ smartCachePipeStreamPairs
    --    ++ kernelPipeStreamPairs
    temp = map (\(Stream _ valueType _, name) -> Pipe name valueType)
               requiredPipeStreamPairs

-- TODO if you find you need stuff later on that you had in Kernel
-- this is probably where you need to change
convertKernelToPipelineItem :: Kernel -> PipelineItem SharedPipelineData
convertKernelToPipelineItem k@Kernel {..} = case kernelType k of
  MapKernel -> Map
    { inputStreams  = inputs
    , outputStreams = outputs
    , name          = kernelName
    , fortran       = body
    , nextStage     = NullStage
    , sharedData    = NullPipeLineData
    }
  ReduceKernel -> Reduce
    { inputStreams  = inputs
    , outputStreams = outputs
    , reductionVars = outputReductionVars
    , name          = kernelName
    , fortran       = body
    , nextStage     = NullStage
    , sharedData    = NullPipeLineData
    }

data KernelType = MapKernel | ReduceKernel

kernelType :: Kernel -> KernelType
kernelType kernel = if valid
  then case (openCLMapCount, openCLReduceCount) of
    (1, 0) -> MapKernel
    (0, 1) -> ReduceKernel
  else error "more than one map or fold in kernel"
 where
  valid =
    (openCLMapCount + openCLReduceCount == 1)
      && openCLMapCount
      >= 0
      && openCLReduceCount
      >= 0
  openCLMapCount = length $ everything (++) (mkQ [] mapQuery) (body kernel)
  openCLReduceCount =
    length $ everything (++) (mkQ [] reduceQuery) (body kernel)
  reduceQuery fortran = case fortran of
    r@OpenCLReduce{} -> [r]
    _                -> []
  mapQuery fortran = case fortran of
    m@OpenCLMap{} -> [m]
    _             -> []

buildPipeName from to streamName =
  from ++ "_" ++ to ++ "_" ++ streamName ++ "pipe"

buildStreamPipePair from to stream@(Stream name _ _) =
  (stream, buildPipeName from to name)

-- For a pipeline stage create a map from stream name to 
-- (source, stream) so that when getting pipes we know 
-- where each stream came from and can name them appropriately
getAvailableStreamsAndSource
  :: Maybe PipelineStage
  -> PipelineStage
  -> DMap.Map String (PipelineItem SharedPipelineData, Stream Anno)
getAvailableStreamsAndSource (Just previousStage) currentStage = DMap.empty
 where
  (previousKernel, _                , _               ) = previousStage
  (currentKernel , currentSmartCache, currentMemAccess) = currentStage
  availableStreamsAndSources =
    getAvailableStreamsAndSource currentSmartCache
      ++ concatMap getAvailableStreamsFromSource currentMemAccess

getAvailableStreamsAndSource Nothing currentStage = DMap.empty

-- for a pipelineItem get all the possible outputstreams
getAvailableStreamsFromSource
  :: PipelineItem SharedPipelineData
  -> [(PipelineItem SharedPipelineData, Stream Anno)]
getAvailableStreamsFromSource mr@MemoryReader {..} =
  map (\(_, s) -> (mr, s)) memToOutputStreams
getAvailableStreamsFromSource smartcache@SmartCache {..} =
  map (smartcache, ) outputStreams
getAvailableStreamsFromSource MemoryWriter{} = []

getKernelInputPipes
  :: String -> Set.Set String -> Kernel -> [(Stream Anno, String)]
getKernelInputPipes previousKernelName smartCacheAndMemoryReaderOutputs kernel
  = map (buildStreamPipePair previousKernelName (kernelName kernel))
        directInputs
 where
  directInputs =
    filter
        (\(Stream name _ _) ->
          Set.notMember name smartCacheAndMemoryReaderOutputs
        )
      $ inputs kernel


getSmartCachePipes
  :: String
  -> String
  -> PipelineItem SharedPipelineData
  -> [(Stream Anno, String)]
getSmartCachePipes previousKernelName nextKernelName SmartCache {..} =
  inputPipes ++ outputPipes
 where
  inputPipes  = map (buildStreamPipePair previousKernelName name) inputStreams
  outputPipes = map (buildStreamPipePair name nextKernelName) outputStreams

getMemoryAccessPipes
  :: String -> PipelineItem SharedPipelineData -> [(Stream Anno, String)]
getMemoryAccessPipes kernelName MemoryReader {..} = map
  (\(_, stream) -> buildStreamPipePair name kernelName stream)
  memToOutputStreams
getMemoryAccessPipes kernelName MemoryWriter {..} = map
  (\(stream, _) -> buildStreamPipePair kernelName name stream)
  inputStreamsToMem






