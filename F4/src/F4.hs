{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE RecordWildCards #-}

module F4 where

import           AddKernelLoopGuards
import           AddMemoryAccessKernels
import           AddPipesToKernels
import           AddSmartCaches
import           AddSynthesisedLoopVars
import           AddTransitStreams
import           BuildDeviceModule
import           CommandLineProcessor        (F4Opts (..), f4CmdParser)
import           Control.Monad.Extra
import           Data.Generics               (everything, everywhere,
                                              everywhereM, gmapQ, gmapT, mkM,
                                              mkQ, mkT)
import           Data.List
import           Data.List.Index
import qualified Data.Map                    as DMap
import           Data.Maybe
import           Data.Tuple.Utils
import           Debug.Trace
import           DetectDriverLoopSize
import           DetectIndividualPipelines
import           KernelCodeGen
import           KernelExtraction
import           Language.Fortran
import           Language.Fortran.Pretty
import qualified LanguageFortranTools        as LFT
import           LinkReductionVars
import           MemoryAccessCodeGen
import           MergeSubroutines
import           MiniPP
import           Options.Applicative
import           Parser                      (parseProgramData)

import           RemoveConstantsFromStencils hiding (addLoopGuards)
import           RemoveConstantsWrapper
import           SanityChecks
import           ScalarizeKernels
import           SmartCacheCodeGen
import           StencilDetection
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           Transformer
import           Utils

processArgs :: IO ()
processArgs = do
  opts <- execParser f4CmdParser
  putStrLn banner
  print opts
  compilerMain opts

compilerMain :: F4Opts -> IO ()
compilerMain args = do
  print "Executing compiler main..."
  -- Parse the Fortran files specified at the command line
  initialSubroutineTable <- parseProgramData args
  -- seperate out the parsed files to be offloaded to the FPGA
  subroutineTable <- removeStencilConstantsWrapper args initialSubroutineTable
  let forOffloadSubTable = DMap.filter parallelise subroutineTable
  let notForOffloadSubTable = DMap.filter (not . parallelise) subroutineTable
  let forOffloadSubTable = DMap.filter parallelise subroutineTable
  let subroutineNames = DMap.keys forOffloadSubTable
  putStrLn (rule '+' ++ " Subroutines not for offload " ++ rule '+')
  debug_displaySubRoutineTable notForOffloadSubTable False
  putStrLn (rule '+' ++ " Subroutines for offload " ++ rule '+')
  debug_displaySubRoutineTable forOffloadSubTable False
  putStrLn (rule '+' ++ " Subroutines for offload merged " ++ rule '+')
  -- Merge the subroutines to be offloaded into one
  let subroutineTableWithOffloadSubsMerged =
        mergeSubsToBeParallelised subroutineTable
  debug_displaySubRoutineTable subroutineTableWithOffloadSubsMerged False
  let mergedOffloadName =
        head $
        DMap.keys $ DMap.filter parallelise subroutineTableWithOffloadSubsMerged
  putStrLn (rule '+' ++ " Pipeline Detection " ++ rule '+')
  (pipelineNames, pipelineSubroutineTable) <-
    updateSubTablesWithPipelines
      (subroutineTableWithOffloadSubsMerged DMap.! mergedOffloadName)
      subroutineTableWithOffloadSubsMerged
  putStrLn (rule '+' ++ " Map + Fold Detection " ++ rule '+')
  -- Map and fold detection from Gavin's compiler
  -- < STEP 4 : Parallelise the loops >
  -- WV: this is the equivalent of calling a statefull pass on every subroutine.
  let (parallelisedSubroutines, parAnnotations) =
        foldl
          (paralleliseProgUnit_foldl (ioSubs args) pipelineSubroutineTable)
          (DMap.empty, [])
          pipelineNames
  debug_displaySubRoutineTable parallelisedSubroutines False
  let srtWithParallelisedSubroutines =
        DMap.union parallelisedSubroutines pipelineSubroutineTable -- subroutineTableWithOffloadSubsMerged
  putStrLn (rule '+' ++ " Stencil Detection " ++ rule '+')
  let srtAfterStenDetect =
        detectStencilsInSubsToBeParallelise srtWithParallelisedSubroutines
  debug_displaySubRoutineTable srtAfterStenDetect False
  -- < Try to fuse the parallelised loops as much as possible (on a per-subroutine basis) >
  let (combinedKernelSubroutines, combAnnotations) =
        foldl
          (combineKernelProgUnit_foldl (loopFusionBound args))
          (srtAfterStenDetect, [])
          pipelineNames
  putStrLn (rule '+' ++ " Combined " ++ rule '+')
  let srtAfterKernelCombination =
        DMap.union combinedKernelSubroutines srtAfterStenDetect
  debug_displaySubRoutineTable srtAfterKernelCombination False
  putStrLn (rule '+' ++ " Kernels " ++ rule '+')
  --
  -- kernels = [[Kernel]] representing pipelines
  kernels <- mapM getKernels (getOffloadSubs srtAfterKernelCombination)
  scalarisedKernels <-
    concatMapM (processPipeline (length kernels)) $ indexed kernels
  (fileName, deviceCode, callingData) <-
    buildDeviceModule (length kernels) scalarisedKernels
  mapM_ print callingData
  writeToFile args (fileName ++ ".f95") (miniPPProgUnit deviceCode)
  return ()

processPipeline :: Int -> (Int, [Kernel]) -> IO [PipelineStage]
processPipeline totalPipelines (pipelineNumber, kernels) = do
  let renamedKernels =
        map (addPipelineNamePrefix totalPipelines pipelineNumber) kernels
  driverLoopParams@(largestStreamName, largestStreamDims, _) <-
    detectDriverLoopSize renamedKernels
  putStrLn (rule '+' ++ " With Loop Guards " ++ rule '+')
  withGuards <- mapM addLoopGuards renamedKernels -- (getOffloadSubs srtAfterKernelCombination)
  putStrLn (rule '+' ++ " With Transit Streams " ++ rule '+')
  kernelsWithTransitStreams <- addTransitStreams withGuards
  putStrLn (rule '+' ++ " With Reduction Vars Linked " ++ rule '+')
  kernelsWithReductionVarsLinked <- linkReductionVars kernelsWithTransitStreams
  putStrLn (rule '+' ++ " With Synthesised Loop Vars " ++ rule '+')
  withLoopVarsSynthesised <-
    synthesiseLoopVars largestStreamDims kernelsWithReductionVarsLinked
  putStrLn (rule '+' ++ " With Smart Caches " ++ rule '+')
  -- this is a [(Kernel, Maybe SmartCache)] representing kernels and their
  -- preceding smart cache if one is required
  smartCacheKernelPairs <- insertSmartCaches withLoopVarsSynthesised
  putStrLn (rule '+' ++ " With Memory Readers " ++ rule '+')
  pipelineStages <- addMemoryAccesses smartCacheKernelPairs
  let withSharedDataUpdated =
        updatePipelineSharedData driverLoopParams pipelineStages
  putStrLn (rule '+' ++ " Routing Pipes " ++ rule '+')
  withPipes <- populatePipes withSharedDataUpdated
  putStrLn (rule '+' ++ " Scalarizing Kernels " ++ rule '+')
  finalPipeline <- scalarizeKernels withPipes
  printPipelineStats pipelineNumber finalPipeline
  return finalPipeline

printPipelineStats :: Int -> [PipelineStage] -> IO ()
printPipelineStats pipelineNumber pipeline = do
  putStrLn $ rule '-'
  putStrLn $ "Pipeline Statistics"
  putStrLn $ "Pipeline ID: " ++ show pipelineNumber
  putStrLn $ "Number of Compute Kernels: " ++ show (length computeKernels)
  putStrLn $
    "Compute Kernel Names: " ++ intercalate ", " (map name computeKernels)
  putStrLn $ "Number of Smart Caches: " ++ show (length smartCaches)
  putStrLn $ "Number of Memory Readers: " ++ show (length memoryReaders)
  putStrLn $ "Number of Memory Writers: " ++ show (length memoryWriters)
  putStrLn $ "Number of Pipes: " ++ show (length pipes)
  putStrLn $ "CSV: \n" ++ intercalate "," csvData
  putStrLn $ rule '-'
  where
    computeKernels = map fst3 pipeline
    smartCaches = mapMaybe snd3 pipeline
    memoryAccess = concatMap thd3 pipeline
    memoryReaders = filter isMemRead memoryAccess
    memoryWriters = filter (not . isMemRead) memoryAccess
    pipes =
      concatMap writtenPipes (computeKernels ++ smartCaches ++ memoryAccess)
    csvData =
      [ show pipelineNumber
      , show (length computeKernels)
      , show (length smartCaches)
      , show (length memoryReaders)
      , show (length memoryWriters)
      , show (length pipes)
      ]

isMemRead j =
  case j of
    MemoryReader {} -> True
    _               -> False

addPipelineNamePrefix :: Int -> Int -> Kernel -> Kernel
addPipelineNamePrefix 1 _ k = k
addPipelineNamePrefix _ pipelineNumber k =
  k {kernelName = "p" ++ show pipelineNumber ++ "_" ++ kernelName k}

getOffloadSubs :: SubroutineTable -> [SubRec]
getOffloadSubs subTable =
  DMap.elems $ DMap.filter (\MkSubRec {..} -> parallelise) subTable

updateSubroutineTable :: [SubRec] -> SubroutineTable -> SubroutineTable
updateSubroutineTable newSubRecs oldSubTable =
  foldl (\map (key, subRec) -> DMap.insert key subRec map) oldSubTable newItems
  where
    newItems = map (\subrec -> (subName subrec, subrec)) newSubRecs

writeToFile :: F4Opts -> String -> String -> IO ()
writeToFile F4Opts {..} fileName contents = do
  createDirectoryIfMissing True outputDir
  whenM (doesFileExist filePath) (removeFile filePath)
  writeFile filePath contents
  where
    filePath = joinPath [outputDir, fileName]

validateInputFiles :: Program LFT.Anno -> IO ()
validateInputFiles fileAst = do
  let results = map (\f -> f fileAst) [checkFilesHaveOnlyOneSubroutine]
  mapM_ printErrorOrContinue results

banner =
  rule '=' ++
  "F4: Finite-element Fortran for FPGAs\n" ++
  "This compiler allows Fortran finite element codes to be compiled\n" ++
  "for execution on FPGA devices via OpenCL" ++ rule '='
