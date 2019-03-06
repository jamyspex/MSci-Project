module ScalarizeKernels where

import qualified Data.Set                      as Set

-- This module is going to walk through the pipeline finding the pipes
-- that are required. 
-- Then it wil add code to the kernels to read the pipes and assign the
-- value read to a local variable 
-- Then it will subsitute the new local variable in place of any previous 
-- array read expressions. 

scalarize :: [PipelineStage] -> [PipelineStage]
scalarize pipeline = pipeline


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
  :: [PipelineStage] -> ([PipelineStage], [Pipe])
getRequiredPipesAndInsertAccessCode pipeline = ([], [])
 where
  go :: ([PipelineStage], [Pipe]) -> PipelineStage -> ([PipelineStage], [Pipe])
  go (newPipeline, pipes) (kenerel, smartCache, memAcces) = ([], [])
   where
    requiredPipes      = []
    previousKernelName = (name . last) newPipeline
    smartCacheInputs   = maybe [] inputStreams smartCache
    smartCacheOutputs  = maybe [] outputStreams smartCache
    smartCacheName     = name smartCache
    memoryAccessPipes  = concatMap getMemoryAccessPipes memAcces

buildPipeName from to streamName =
  from ++ "_" ++ to ++ "_" ++ streamName ++ "pipe"

buildStreamPipePair from to stream@(Stream name _ _) =
  (stream, buildPipeName from to name)

getKernelInputPipes
  :: String -> Set.Set String -> Kernel -> [(Stream Anno, String)]
getKernelInputPipes previousKernelName smartCacheAndMemoryReaderOutputs kernel
  = map (buildStreamPipePair previousKernelName (name kernel)) directInputs
 where
  directInputs =
    filter
        (\(Stream name _ _) ->
          not smartCacheAndMemoryReaderOutputs `contains` name
        )
      $ inputs kernel


getSmartCachePipes
  :: String -> String -> Maybe PipelineStage -> [(Stream Anno, String)]
getSmartCachePipes previousKernelName nextKernelName (Just SmartCache {..}) =
  inputPipes ++ outputPipes
 where
  inputPipes  = map (buildStreamPipePair previousKernelName name) inputStreams
  outputPipes = map (buildStreamPipePair name nextKernelName) outputStreams
getSmartCachePipes _ (Nothing _) = []

getMemoryAccessPipes :: String -> PipelineStage -> [(Stream Anno, String)]
getMemoryAccessPipes kernelName MemoryReader {..} = map
  (\(_, stream) -> buildStreamPipePair name kernelName stream)
  memToOutputStreams
getMemoryAccessPipes kernelName MemoryWriter {..} = map
  (\(stream, _) -> buildStreamPipePair kernelName name stream)
  inputStreamsToMem






