module AddMemoryAccessKernels where

import Data.Map
import           Pipeline
import           Utils

--  The way we're going to work this is take the last instance of an output stream to the where ever
--  it is next required. If an output stream does not exist prior to that point in the pipeline
--  generate a memory reader.
--
-- TODO
-- For now I'm going to set the size of the smartcaches to the size of the largest one in the
-- pipeline to prevent deadlocks in situations like shown below. I say for now but I don't think
-- there is anyway to actually get round this issue. The best we could do is to minimise the
-- number of smart caches that have their size increased by analysing the dependency graph.
-- Hmm actually I'm just going to leave it for now and then once I've wired all the pipes up decide
-- how much work it would be to implement the dependency analysis as can easily implement the "scale
-- 'em all up" fall back at that point as well.
--
--                --------------
--                |  Kernel 1  |
--                --------------
--          stream 1 |       |  stream 2
--                   |       ---------|
--                   V                |
--              -------------------   |    This would deadlock because in order for Kernel
-- size = 1005  |  Smart Cache 1  |   |    2 to start processing 1005 items from stream 1 need to be
--              -------------------   |    written to Smart Cache 1. However that is impossible
--                   |                |    because Smart Cache 2 is only able to buffer 503
--                   |                |    values of stream 2. Thinking about it this is exactly
--                   V                |    the same issue that means cache lines within a smart
--                --------------      |    cache have to all be the same size as the largest one.
--                |  Kernel 2  |      |
--                --------------      |
--                   |                |
--                   |       |---------
--                   V       V
--              -------------------
--  size = 503  |  Smart Cache 2  |
--              -------------------
--                   |       |
--                   |       |
--                   V       V
--                --------------
--                |  Kernel 3  |
--                --------------
-- 
--
--

type PipelineStage = (Kernel, Pipeline SharedPipelineData, [Pipeline SharedPipelineData])

-- Add memory reader kernels
addMemoryReaders ::
     [(Kernel, Pipeline SharedPipelineData)]
  -> IO (PipelineStage) 
addMemoryReaders kernelsAndSmartCaches =
  where
    sorted = sortBy (\(k1, _) (k2, _) -> order k1 `compare` order k2) kernelsAndSmartCaches
    availableOutputStreams = DMap.empty

foldOverPipeline :: (DMap.Map String String, [PipelineStage]) -> Int -> (DMap.Map String String, [PipelineStage])
foldOverPipeline (availableStreamMap, pipeline) currentStage = 
  where

    
    
