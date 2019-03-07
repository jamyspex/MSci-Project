{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module ScalarizeKernels where

import           AddMemoryAccessKernels
import           Data.Generics
import           Data.List
import           Data.List.Split
import qualified Data.Map                      as DMap
import qualified Data.Set                      as Set
import           Debug.Trace
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

pipeToGraphEdge :: Pipe -> String
pipeToGraphEdge (Pipe name _) =
  from ++ " -> " ++ to ++ " [ label=\"" ++ stream_name ++ "\" ]"
  where [from, to, stream_name, _] = splitOn "__" name

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
  mapM_ (\p -> putStrLn $ show p ++ "\n") requiredPipes
  putStrLn "Graph Viz data:\n"
  putStrLn "digraph G {\n"
  mapM_ (\p -> putStrLn $ "\t" ++ pipeToGraphEdge p) requiredPipes
  putStrLn "}\n"
  return result
 where

  (_, requiredPipes) = result
  result             = foldl go ([], []) pipeline
  go :: ([PipelineStage], [Pipe]) -> PipelineStage -> ([PipelineStage], [Pipe])
  go (newPipeline, pipes) currentStage =
     -- TODO Feel like the fold might fuck up the ordering of newpipeline but we'll see
    (newPipeline ++ [currentStage], pipes ++ newPipes)
   where
    previousStage =
      if null newPipeline then Nothing else Just $ last newPipeline
    availableStreams = getAvailableStreamsAndSource previousStage currentStage
    (currentKernel, currentSmartCache, currentMemAccess) = currentStage
    streamsRequiringPipesAndDestination =
      concatMap getRequiredStreams currentMemAccess
        ++ maybe [] getRequiredStreams currentSmartCache
        ++ getRequiredStreams
             (trace ("current kernel = " ++ name currentKernel ++ "\n")
                    currentKernel
             )
    newPipes = map getPipeName streamsRequiringPipesAndDestination
    getPipeName :: (Stream Anno, PipelineItem SharedPipelineData) -> Pipe
    getPipeName (stream, dest) = if valid
      then buildPipe sourceName destName stream
      else error "Stream has different name in map"
     where
      valid =
        -- trace ("available stream map: \n" ++ showMap availableStreams)
        getStreamName stream == getStreamName streamFromMap
      streamName = getStreamName stream
      (source, streamFromMap) =
        pickSource dest
          $      availableStreams
          DMap.! trace ("lookup: " ++ streamName) streamName
      sourceName = name source
      destName   = name dest


showMap
  :: DMap.Map String [(PipelineItem SharedPipelineData, Stream Anno)] -> String
showMap map = foldl
  (\string key ->
    string
      ++ key
      ++ " --> \n"
      ++ concatMap
           (\(source, stream) ->
             "\t" ++ name source ++ " " ++ getStreamName stream ++ "\n"
           )
           (map DMap.! key)
      ++ "\n"
  )
  ""
  (DMap.keys map)


-- When connecting up
--      forwardStreamsOnly
--        :: [(Stream Anno, PipelineItem SharedPipelineData)]
--        -> [(Stream Anno, PipelineItem SharedPipelineData)]
--      forwardStreamsOnly streamsAndDestinations

-- this function uses the list of values from the map to decide which
-- stream source to use based on where the destination. As explained
-- below.
pickSource
  :: PipelineItem SharedPipelineData
  -> [(PipelineItem SharedPipelineData, Stream Anno)]
  -> (PipelineItem SharedPipelineData, Stream Anno)
pickSource dest [source@(s, _)] =
  trace ("one choice for dest = " ++ name dest ++ " source = " ++ name s) source
pickSource dest@SmartCache{} sources = trace
  (  "options = \n"
  ++ concatMap (\(s, _) -> "\t" ++ name s ++ "\n") sources
  ++ "picked = "
  ++ name pname
  )
  picked
 where
  picked@(pname, _) =
    minimumBy (\(pi1, _) (pi2, _) -> stageNumber pi1 `compare` stageNumber pi2)
      $ filter
          (\(s, _) -> case s of
            Map{}    -> True
            Reduce{} -> True
            _        -> False
          )
          (removeDestFromSources dest sources)
pickSource dest@MemoryWriter{} sources =
  head
    $ filter
        (\(s, _) -> case s of
          Map{}    -> True
          Reduce{} -> True
          _        -> False
        )
    $ trace ("memory writer dest = " ++ name dest)
            (removeDestFromSources dest sources)
pickSource dest sources =
  maximumBy (\(s1, _) (s2, _) -> compareKernelOpts s1 s2)
    -- $ filter
    --     (\(s, _) -> case s of
    --       SmartCache{}   -> True
    --       MemoryReader{} -> True
    --       Map{}          -> True
    --       Reduce{}       -> True
    --       _              -> False
    --     )
                                                          $ trace
    ("map/reduce dest = " ++ name dest)
    (removeDestFromSources dest sources)

removeDestFromSources dest = filter (\(s, _) -> name s /= name dest)

-- The functions below are used to rank other pipeline items
-- in order that they should be selected as the source for a stream
-- when they is more than one possible option. All the functions
-- are used to find the most "desirable" stream source from a list
-- of all available streams. The available streams are:
--      1) Outputs from compute kernel in previous pipeline stage
--      2) Outputs from smartcaches
--      3) Outputs from memory readers
--      4) Outputs from compute kernel in curren pipeline stage
-- This list is then filter to remove outputs from the current
-- stage e.g. if the destination being connected up is a smartcache
-- the outputs from that smart cache are removed to prevent loop
-- backs.
--
-- This one is for ranking sources for map/fold kernel inputs
compareKernelOpts SmartCache{} _            = Prelude.GT
compareKernelOpts _            SmartCache{} = Prelude.LT
compareKernelOpts Map{}        Reduce{}     = Prelude.EQ
compareKernelOpts Reduce{}     Map{}        = Prelude.EQ
compareKernelOpts Map{}        _            = Prelude.GT
compareKernelOpts _            Map{}        = Prelude.LT
compareKernelOpts Reduce{}     _            = Prelude.GT
compareKernelOpts _            Reduce{}     = Prelude.LT
compareKernelOpts _            _            = Prelude.EQ

-- Compare input source options from perspective of a memory writer
-- e.g. always select outputs from the previous computer kernel.
-- If we can't get outputs find from a compute kernel throw and error.
compareMemWriterOpts Map{}    Reduce{} = Prelude.EQ
compareMemWriterOpts Reduce{} Map{}    = Prelude.EQ
compareMemWriterOpts Map{}    _        = Prelude.GT
compareMemWriterOpts _        Map{}    = Prelude.LT
compareMemWriterOpts Reduce{} _        = Prelude.GT
compareMemWriterOpts _        Reduce{} = Prelude.LT
compareMemWriterOpts _ _ =
  error "No suitable streams found for memory writer inputs"

-- When smartCache is the destination we need a tie break between
-- the previous pipeline stage compute kernels outputs and the current
-- pipeline stage outputs. We obivously don't want to select the outputs
-- of the current compute kernel to put back into the smart into the
-- smartcache as an input so select the streams from the previous compute
-- kernel by selecting the kernel with the lowest stageNumber as the source.
compareSmartCacheOpts m@Map{} r@Reduce{} =
  stageNumber r `compare` stageNumber m
compareSmartCacheOpts r@Reduce{} m@Map{} =
  stageNumber m `compare` stageNumber r
compareSmartCacheOpts Map{}    _        = Prelude.GT
compareSmartCacheOpts _        Map{}    = Prelude.LT
compareSmartCacheOpts Reduce{} _        = Prelude.GT
compareSmartCacheOpts _        Reduce{} = Prelude.LT
compareSmartCacheOpts _        _        = Prelude.EQ

buildPipe from to stream = Pipe
  (from ++ "__" ++ to ++ "__" ++ name ++ "__pipe")
  valueType
  where (Stream name valueType _) = stream

-- For a pipeline stage create a map from stream name to
-- (source, stream) so that when getting pipes we know
-- where each stream came from and can name them appropriately
--
-- So this function gets a bit implicit and difficult to follow
-- without a bit more explaination. The filtering of availableStreamsAndSources
-- is nescesary because transit streams have the same name when they
-- leave a smart cache as when they went into it. This leads to an issue
-- whereby we have to choose where a pipeline item gets its inputs from
-- 1) the output of the last kernel or 2) the output of the smart cache.
-- For the kernel we always need to choose the smart cache outputs but
-- the smart cache always needs to choose the previous kernel outputs.
getAvailableStreamsAndSource
  :: Maybe PipelineStage
  -> PipelineStage
  -> DMap.Map String [(PipelineItem SharedPipelineData, Stream Anno)]
getAvailableStreamsAndSource Nothing currentStage = trace
  (name currentKernel ++ " in Nothing case")
  map
 where
  (currentKernel, currentSmartCache, currentMemAccess) = currentStage
  availableStreamsAndSources =
    maybe [] getAvailableStreamsFromSource currentSmartCache
      ++ concatMap getAvailableStreamsFromSource currentMemAccess
      ++ getAvailableStreamsFromSource currentKernel
  map = makeMap availableStreamsAndSources
getAvailableStreamsAndSource (Just previousStage) currentStage = trace
  (name currentKernel ++ " in Just case")
  map
 where
  (previousKernel, _                , _               ) = previousStage
  (currentKernel , currentSmartCache, currentMemAccess) = currentStage
  availableStreamsAndSources =
    getAvailableStreamsFromSource previousKernel
      ++ maybe [] getAvailableStreamsFromSource currentSmartCache
      ++ concatMap getAvailableStreamsFromSource currentMemAccess
      ++ getAvailableStreamsFromSource currentKernel
  map = makeMap availableStreamsAndSources

-- Function groups available streams by name then, builds a map
-- stream name -> [(producer0, stream), (producer1, stream)]
makeMap
  :: [(PipelineItem SharedPipelineData, Stream Anno)]
  -> DMap.Map String [(PipelineItem SharedPipelineData, Stream Anno)]
makeMap sourceAndStreams = trace
  ("length sourceAndStreams = " ++ (show $ length sourceAndStreams))
  DMap.fromList
  mapGroups
 where
  mapItems =
    map (\(source, s@(Stream name _ _)) -> (name, (source, s))) sourceAndStreams
  toGroupMapItems = sortBy (\(n1, _) (n2, _) -> n1 `compare` n2) mapItems
  grouped =
    groupBy
        (\(n1, _) (n2, _) ->
          trace (n1 ++ " == " ++ n2 ++ " is " ++ (show $ n1 == n2) ++ "\n")
            $  n1
            == n2
        )
      $ trace ("mapItems = \n" ++ showMapItems mapItems) toGroupMapItems
  mapGroups = map (\grp -> ((fst . head) grp, map snd grp))
    $ trace ("grouped = \n" ++ showGrouped grouped) grouped

showMapItems = concatMap
  (\(streamName, (source, stream)) ->
    "map key = "
      ++ streamName
      ++ " source = "
      ++ name source
      ++ " stream = "
      ++ getStreamName stream
      ++ "\n"
  )

showGrouped
  :: [[(String, (PipelineItem SharedPipelineData, Stream Anno))]] -> String
showGrouped grpd = concatMap
  (\grp ->
    concatMap
        (\(_, (item, (Stream streamName _ _))) ->
          "\t"
            ++ "stream name = "
            ++ streamName
            ++ " source name = "
            ++ name item
            ++ "\n"
        )
        grp
      ++ "--------------------------\n"
  )
  grpd

-- for a pipelineItem get all the possible outputstreams
getAvailableStreamsFromSource
  :: PipelineItem SharedPipelineData
  -> [(PipelineItem SharedPipelineData, Stream Anno)]
getAvailableStreamsFromSource mr@MemoryReader {..} = trace
  ("memReader = \n" ++ concatMap (\s -> show s ++ "\n") memToOutputStreams)
  map
  (\(_, s) -> (mr, s))
  memToOutputStreams
getAvailableStreamsFromSource smartcache@SmartCache {..} = trace
  ("smartcache = \n" ++ concatMap (\s -> show s ++ "\n") outputStreams)
  map
  (smartcache, )
  outputStreams
getAvailableStreamsFromSource MemoryWriter{} = []
getAvailableStreamsFromSource k@Map {..}     = trace
  ("map = \n" ++ concatMap (\s -> show s ++ "\n") outputStreams)
  map
  (k, )
  outputStreams
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
getRequiredStreams
  :: PipelineItem SharedPipelineData
  -> [(Stream Anno, PipelineItem SharedPipelineData)]
getRequiredStreams item@Map {..}          = map (, item) inputStreams
getRequiredStreams item@Reduce {..}       = map (, item) inputStreams
getRequiredStreams item@SmartCache {..}   = map (, item) inputStreams
getRequiredStreams item@MemoryReader {..} = []
  --    map (\(_, s) -> (s, item)) memToOutputStreams
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
