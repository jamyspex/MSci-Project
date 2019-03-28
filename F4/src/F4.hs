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
import qualified Data.Map                    as DMap
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
  putStrLn (rule '+' ++ " Stencil Constant Removal " ++ rule '+')
  -- let srtNoStencilConstants =
  --       DMap.insert
  --         mergedOffloadName
  --         withConstantsRemoved
  --         subroutineTableWithOffloadSubsMerged
  -- withConstantsRemoved <-
  --   removeConstantsFromStencilsAndPrint $
  --   subroutineTableWithOffloadSubsMerged DMap.! mergedOffloadName
  --error "Exit!"
  -- putStrLn (rule '+' ++ " Pipeline Detection " ++ rule '+')
  -- splitMergedMethodInPipelines $
  --   subroutineTableWithOffloadSubsMerged DMap.! mergedOffloadName
  putStrLn (rule '+' ++ " Map + Fold Detection " ++ rule '+')
  -- Map and fold detection from Gavin's compiler
  -- < STEP 4 : Parallelise the loops >
  -- WV: this is the equivalent of calling a statefull pass on every subroutine.
  let (parallelisedSubroutines, parAnnotations) =
        foldl
          (paralleliseProgUnit_foldl
             (ioSubs args)
             subroutineTableWithOffloadSubsMerged)
          (DMap.empty, [])
          [mergedOffloadName]
  debug_displaySubRoutineTable parallelisedSubroutines False
  let srtWithParallelisedSubroutines =
        DMap.union parallelisedSubroutines subroutineTableWithOffloadSubsMerged
  -- mapM_ (\subRecord -> putStrLn ("\n" ++ hl ++ (fst subRecord) ++ hl ++ (miniPPProgUnit (subAst (snd subRecord))) ++ hl))
  --     (DMap.toList parallelisedSubroutines)
  putStrLn (rule '+' ++ " Stencil Detection " ++ rule '+')
  let srtAfterStenDetect =
        detectStencilsInSubsToBeParallelise srtWithParallelisedSubroutines
  debug_displaySubRoutineTable srtAfterStenDetect False
  -- < STEP 5 : Try to fuse the parallelised loops as much as possible (on a per-subroutine basis) >
  let (combinedKernelSubroutines, combAnnotations) =
        foldl
          (combineKernelProgUnit_foldl (loopFusionBound args))
          (srtAfterStenDetect, [])
          [mergedOffloadName]
  putStrLn (rule '+' ++ " Combined " ++ rule '+')
  let srtAfterKernelCombination =
        DMap.union combinedKernelSubroutines srtAfterStenDetect
  debug_displaySubRoutineTable srtAfterKernelCombination False
  let combinedOffloadSub = srtAfterKernelCombination DMap.! mergedOffloadName
  putStrLn (rule '+' ++ " With Loop Guards " ++ rule '+')
  let withGuards = addLoopGuards combinedOffloadSub
  let srtWithGuards =
        DMap.insert mergedOffloadName withGuards srtAfterKernelCombination
  debug_displaySubRoutineTable srtWithGuards False
  putStrLn (rule '+' ++ " Kernels " ++ rule '+')
  let guardedMerged = srtWithGuards DMap.! mergedOffloadName
  kernels <- getKernels guardedMerged
  driverLoopParams <- detectDriverLoopSize kernels
  let mainSubName = mainSub args
  let mainArgTrans = argTranslations (notForOffloadSubTable DMap.! mainSubName)
  putStrLn "BEFORE"
  kernelsWithTransitStreams <- addTransitStreams kernels
  putStrLn (rule '+' ++ " With Reduction Vars Linked " ++ rule '+')
  kernelsWithReductionVarsLinked <- linkReductionVars kernelsWithTransitStreams
  putStrLn (rule '+' ++ " With Synthesised Loop Vars " ++ rule '+')
  withLoopVarsSynthesised <- synthesiseLoopVars kernelsWithReductionVarsLinked
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
  scalarisedKernels <- scalarizeKernels withPipes
  (fileName, deviceCode, callingData) <- buildDeviceModule scalarisedKernels
  mapM_ print callingData
  writeToFile args (fileName ++ ".f95") (miniPPProgUnit deviceCode)
  return ()

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
