{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module ScalarizeKernels where

import           AddMemoryAccessKernels
import           Data.Generics
import           Data.List.Unique
import qualified Data.Map               as DMap
import qualified Data.Set               as Set
import           Language.Fortran
import           LanguageFortranTools
import           Pipeline
import           Utils

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
getRequiredPipesAndInsertAccessCode ::
     [PipelineStage] -> IO ([PipelineStage], [Pipe])
getRequiredPipesAndInsertAccessCode pipeline = do
  mapM_ print requiredPipes
  return requiredPipes
  where
    requiredPipes = foldl go ([], []) pipeline
    go ::
         ([PipelineStage], [Pipe]) -> PipelineStage -> ([PipelineStage], [Pipe])
    go (newPipeline, pipes) currentStage = ([], pipes ++ newPipes)
      where
        previousStage =
          if null newPipeline
            then Nothing
            else Just $ last newPipeline
        availableStreams =
          getAvailableStreamsAndSource previousStage currentStage
        (currentKernel, currentSmartCache, currentMemAccess) = currentStage
        currentKernelName = name currentKernel
        smartCacheStreams = maybe [] getRequiredStreams currentSmartCache
        streamsRequiringPipesAndDestination =
          concatMap getRequiredStreams currentMemAccess ++
          smartCacheStreams ++ getRequiredStreams currentKernel
        newPipes = map getPipeName streamsRequiringPipesAndDestination
        getPipeName :: (Stream Anno, PipelineItem SharedPipelineData) -> Pipe
        getPipeName (stream, dest) =
          if valid
            then buildPipe sourceName destName stream
            else error "Stream has different name in map"
          where
            valid = getStreamName stream == getStreamName streamFromMap
            streamName = getStreamName stream
            (source, streamFromMap) = availableStreams DMap.! streamName
            sourceName = name source
            destName = name dest

buildPipe from to stream =
  Pipe (from ++ "_" ++ to ++ "_" ++ name ++ "pipe") valueType
  where
    (Stream name valueType _) = stream

-- For a pipeline stage create a map from stream name to
-- (source, stream) so that when getting pipes we know
-- where each stream came from and can name them appropriately
getAvailableStreamsAndSource ::
     Maybe PipelineStage
  -> PipelineStage
  -> DMap.Map String (PipelineItem SharedPipelineData, Stream Anno)
getAvailableStreamsAndSource Nothing currentStage = DMap.fromList mapItems
  where
    (_, currentSmartCache, currentMemAccess) = currentStage
    availableStreamsAndSources =
      maybe [] getAvailableStreamsFromSource currentSmartCache ++
      concatMap getAvailableStreamsFromSource currentMemAccess
    mapItems = makeMapItems availableStreamsAndSources
getAvailableStreamsAndSource (Just previousStage) currentStage =
  DMap.fromList mapItems
  where
    (previousKernel, _, _) = previousStage
    (_, currentSmartCache, currentMemAccess) = currentStage
    availableStreamsAndSources =
      getAvailableStreamsFromSource previousKernel ++
      maybe [] getAvailableStreamsFromSource currentSmartCache ++
      concatMap getAvailableStreamsFromSource currentMemAccess
    mapItems = makeMapItems availableStreamsAndSources

makeMapItems ::
     [(PipelineItem SharedPipelineData, Stream Anno)]
  -> [(String, (PipelineItem SharedPipelineData, Stream Anno))]
makeMapItems sourceAndStreams =
  if allUnique streamNamesOnly
    then mapItems
    else error "Duplicate streams available"
  where
    mapItems =
      map
        (\(source, s@(Stream name _ _)) -> (name, (source, s)))
        sourceAndStreams
    streamNamesOnly = map fst mapItems

-- for a pipelineItem get all the possible outputstreams
getAvailableStreamsFromSource ::
     PipelineItem SharedPipelineData
  -> [(PipelineItem SharedPipelineData, Stream Anno)]
getAvailableStreamsFromSource mr@MemoryReader {..} =
  map (\(_, s) -> (mr, s)) memToOutputStreams
getAvailableStreamsFromSource smartcache@SmartCache {..} =
  map (smartcache, ) outputStreams
getAvailableStreamsFromSource MemoryWriter {} = []
getAvailableStreamsFromSource k@Map {..} = map (k, ) outputStreams
getAvailableStreamsFromSource k@Reduce {..} = map (k, ) outputStreams

-- getKernelInputPipes ::
--      String -> Set.Set String -> Kernel -> [(Stream Anno, String)]
-- getKernelInputPipes previousKernelName smartCacheAndMemoryReaderOutputs kernel =
--   map (buildStreamPipePair previousKernelName (kernelName kernel)) directInputs
--   where
--     directInputs =
--       filter
--         (\(Stream name _ _) ->
--            Set.notMember name smartCacheAndMemoryReaderOutputs) $
--       inputs kernel
getRequiredStreams ::
     PipelineItem SharedPipelineData
  -> [(Stream Anno, PipelineItem SharedPipelineData)]
getRequiredStreams item@Map {..} = map (, item) inputStreams
getRequiredStreams item@Reduce {..} = map (, item) inputStreams
getRequiredStreams item@SmartCache {..} = map (, item) inputStreams
getRequiredStreams item@MemoryReader {..} =
  map (\(_, s) -> (s, item)) memToOutputStreams
getRequiredStreams item@MemoryWriter {..} =
  map (\(s, _) -> (s, item)) inputStreamsToMem
-- getMemoryAccessPipes :: String -> PipelineItem SharedPipelineData -> [(Stream Anno, String)]
-- getMemoryAccessPipes kernelName MemoryReader {..} =
--   map
--     (\(_, stream) -> buildStreamPipePair name kernelName stream)
--     memToOutputStreams
-- getMemoryAccessPipes kernelName MemoryWriter {..} =
--   map
--     (\(stream, _) -> buildStreamPipePair kernelName name stream)
--     inputStreamsToMem
