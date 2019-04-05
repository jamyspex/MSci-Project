{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AddMemoryAccessKernels where

import           Data.Foldable
import           Data.Ix
import           Data.List            as List
import           Data.Maybe
import           Data.Sequence        as Seq
import qualified Data.Set             as Set
import           Debug.Trace
import           LanguageFortranTools
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
showPipelineStage (kernel, smartCache, memReaders) =
  "--------------  PIPELINE STAGE  ---------------\n" ++
  show kernel ++
  show smartCache ++
  concatMap show memReaders ++
  "----------------------------------------------\n"

addMemoryAccesses ::
     [(Kernel, Maybe (PipelineItem SharedPipelineData))] -> IO [PipelineStage]
addMemoryAccesses kernelsAndSmartCaches = do
  mapM_ (putStrLn . showPipelineStage) withMemoryWriter
  return withMemoryWriter
  where
    kernelsConverted =
      map
        (\(k, sc) -> (convertKernelToPipelineItem k, sc))
        kernelsAndSmartCaches
    withMemoryReaders = addMemoryReaders kernelsConverted
    withMemoryWriter = addMemoryWriter withMemoryReaders

-- Add memory reader kernels
addMemoryReaders ::
     [(PipelineItem SharedPipelineData, Maybe (PipelineItem SharedPipelineData))]
  -> [PipelineStage]
addMemoryReaders kernelsAndSmartCaches = pipeline
  where
    sorted =
      List.sortBy
        (\(k1, _) (k2, _) -> stageNumber k1 `compare` stageNumber k2)
        kernelsAndSmartCaches
    initialPipelineStages = map (\(k, sc) -> (k, sc, [])) sorted
    availableStreams = Set.empty
    (_, withMemReaders) =
      foldl
        foldOverPipeline
        (availableStreams, Seq.fromList initialPipelineStages)
        (range (0, List.length initialPipelineStages))
    pipeline = toList withMemReaders

-- Iterate over the pipeline stages and using a set of the currently available streams.
-- A stream is available if it has been output by a kernel and has not been used as input to another kernel.
foldOverPipeline ::
     (Set.Set String, Seq.Seq PipelineStage)
  -> Int
  -> (Set.Set String, Seq.Seq PipelineStage)
foldOverPipeline (availableStreams, pipeline) currentStageIdx =
  (withKernelOutputStreams, updatedPipeline)
  where
    updatedPipeline =
      Seq.update
        currentStageIdx
        (kernel, smartCache, requiredMemoryReaders)
        pipeline
    (kernel, smartCache, _) = pipeline `Seq.index` currentStageIdx
    requiredInputStreams = getRequiredInputStreams kernel smartCache
    requiredMemoryReaders = concatMap buildMemoryReader requiredInputStreams
    withConsumedStreamsRemoved =
      foldl
        (\set stream -> Set.delete (getArrayNameFromStream stream) set)
        availableStreams
        requiredInputStreams
    withKernelOutputStreams =
      foldl
        (\set stream -> Set.insert (getArrayNameFromStream stream) set)
        withConsumedStreamsRemoved
        (outputStreams kernel)
      -- (Stream name _ _ _) -> Set.insert name set)
    buildMemoryReader :: Stream Anno -> [PipelineItem SharedPipelineData]
    buildMemoryReader stream@Stream {} = checkForAvailablityAndBuild stream
    -- if streamAvailable
    --   then []
    --   else
    --     [ MemoryReader
    --         { memToOutputStreams = [ ( FPGAMemArray streamName dimensions
    --                                  , stream
    --                                  )
    --                                ]
    --         , nextStage          = NullItem
    --         , name = name kernel ++ "_" ++ streamName ++ "_" ++ "reader"
    --         , readPipes          = []
    --         , writtenPipes       = []
    --         , sharedData         = NullPipeLineData
    --         }
    --     ]
    -- where streamAvailable = Set.member streamName availableStreams
    buildMemoryReader stream@(StencilStream streamName arrayName valueType dimensions _) =
      trace
        ("WARNING: emitting memory reader for unresolved stencil stream: " ++
         streamName)
        checkForAvailablityAndBuild
        (Stream streamName arrayName valueType dimensions)
    checkForAvailablityAndBuild stream@(Stream streamName arrayName valueType dimensions) =
      if streamAvailable
        then []
        else [ MemoryReader
                 { memToOutputStreams =
                     [(FPGAMemArray streamName dimensions, stream)]
                 , nextStage = NullItem
                 , name = name kernel ++ "_" ++ streamName ++ "_" ++ "reader"
                 , readPipes = []
                 , writtenPipes = []
                 , sharedData = NullPipeLineData
                 }
             ]
      where
        streamAvailable = Set.member arrayName availableStreams

-- used to signify the position a stream is required at.
-- Allows streams to enter and exit smartcache with the same name
data StreamPosition
  = SmartCacheIn
  | KernelIn
  deriving (Eq, Ord)

-- used to work out which of the input streams to a pipeline stage need to come
-- from a memory reader
getRequiredInputStreams ::
     PipelineItem SharedPipelineData
  -> Maybe (PipelineItem SharedPipelineData)
  -> [Stream Anno]
getRequiredInputStreams kernel smartCache =
  map snd $ Set.toList withSmartCacheOutputsRemoved
  where
    smartCacheInputStreams =
      maybe [] (map (SmartCacheIn, ) . inputStreams) smartCache
    smartCacheOutputStreams = maybe [] outputStreams smartCache
    allInputs = smartCacheInputStreams ++ map (KernelIn, ) (inputStreams kernel)
    inputSet = Set.fromList allInputs
  -- delete streams from smart cache to kernel as they don't require mem reader
    withSmartCacheOutputsRemoved =
      foldl
        (\set streamName -> Set.delete (KernelIn, streamName) set)
        inputSet
        smartCacheOutputStreams

-- at the minute this is simply gonna look for the last stage
-- and then add a memory writer for each of the output streams
addMemoryWriter :: [PipelineStage] -> [PipelineStage]
addMemoryWriter pipeline = init pipeline ++ [updatedFinalStage]
  where
    sortedPipeline =
      List.sortBy
        (\(kernel1, _, _) (kernel2, _, _) ->
           stageNumber kernel1 `compare` stageNumber kernel2)
        pipeline
    (lastKernel, smartcache, memoryReaders) = last sortedPipeline
    memoryWriter =
      buildMemoryWriter (name lastKernel) (outputStreams lastKernel)
    updatedFinalStage = (lastKernel, smartcache, memoryWriter : memoryReaders)

buildMemoryWriter :: String -> [Stream Anno] -> PipelineItem SharedPipelineData
buildMemoryWriter kernelName streams =
  MemoryWriter
    { inputStreamsToMem =
        map
          (\s@(Stream name _ valueType dims) -> (s, FPGAMemArray name dims))
          streams
    , name = kernelName ++ "_output_writer"
    , readPipes = []
    , writtenPipes = []
    , sharedData = NullPipeLineData
    }
