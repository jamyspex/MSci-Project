{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module F4 where

import           Parser                  (SubRec (..),
                                          debug_displaySubRoutineTable,
                                          parseProgramData)

import           AddKernelLoopGuards
import           CommandLineProcessor    (F4Opts (..), f4CmdParser)
import           ConstantFolding
import           Data.Generics           (everything, everywhere, everywhereM,
                                          gmapQ, gmapT, mkM, mkQ, mkT)
import qualified Data.Map                as DMap
import           Debug.Trace
import           Language.Fortran
import           Language.Fortran.Pretty
import qualified LanguageFortranTools    as LFT
import           MergeSubroutines
import           MiniPP
import           Options.Applicative
import           SanityChecks
import           StencilDetection
import           Transformer


processArgs :: IO ()
processArgs = do
    opts <- execParser f4CmdParser
    putStrLn banner
    print opts
    compilerMain opts

compilerMain :: F4Opts -> IO ()
compilerMain args = do
    subroutineTable <- parseProgramData args

    -- traceIO $ show (DMap.keys subroutineTable)

    let notForOffloadSubTable = DMap.filter (\subrec -> (not . parallelise) subrec) subroutineTable
    let forOffloadSubTable = DMap.filter (\subRec -> parallelise subRec) subroutineTable
    let subroutineNames = DMap.keys forOffloadSubTable

    putStrLn ((rule '+') ++ " Subroutines not for offload " ++ (rule '+'))

    debug_displaySubRoutineTable notForOffloadSubTable

    -- traceIO $ show (DMap.keys notForOffloadSubTable)

    putStrLn ((rule '+') ++ " Subroutines for offload " ++ (rule '+'))

    debug_displaySubRoutineTable forOffloadSubTable

    -- traceIO $ show (DMap.keys forOffloadSubTable)

    putStrLn ((rule '+') ++ " Subroutines for offload merged " ++ (rule '+'))

    let subroutineTableWithOffloadSubsMerged = mergeSubsToBeParallelised subroutineTable

    -- traceIO $ show (DMap.keys subroutineTableWithOffloadSubsMerged)

    debug_displaySubRoutineTable subroutineTableWithOffloadSubsMerged

    let mergedForOffload = DMap.filter (\subRec -> parallelise subRec) subroutineTableWithOffloadSubsMerged
    let mergedOffloadName = DMap.keys mergedForOffload

    putStrLn ((rule '+') ++ " Map + Fold Detection " ++ (rule '+'))

    -- < STEP 4 : Parallelise the loops >
    -- WV: this is the equivalent of calling a statefull pass on every subroutine.
    let (parallelisedSubroutines, parAnnotations) = foldl (paralleliseProgUnit_foldl (ioSubs args) subroutineTableWithOffloadSubsMerged) (DMap.empty, []) mergedOffloadName

    debug_displaySubRoutineTable parallelisedSubroutines

    let srtWithParallelisedSubroutines = DMap.union parallelisedSubroutines subroutineTableWithOffloadSubsMerged

    -- mapM_ (\subRecord -> putStrLn ("\n" ++ hl ++ (fst subRecord) ++ hl ++ (miniPPProgUnit (subAst (snd subRecord))) ++ hl))
    --     (DMap.toList parallelisedSubroutines)

    putStrLn ((rule '+') ++ " Stencil Detection " ++ (rule '+'))

    let srtAfterStenDetect = detectStencilsInSubsToBeParallelise srtWithParallelisedSubroutines

    debug_displaySubRoutineTable srtAfterStenDetect

    -- < STEP 5 : Try to fuse the parallelised loops as much as possible (on a per-subroutine basis) >
    let (combinedKernelSubroutines, combAnnotations) = foldl (combineKernelProgUnit_foldl (loopFusionBound args)) (srtAfterStenDetect, []) mergedOffloadName

    putStrLn ((rule '+') ++ " Combined " ++ (rule '+'))

    let srtAfterKernelCombination = DMap.union combinedKernelSubroutines srtAfterStenDetect

    debug_displaySubRoutineTable srtAfterKernelCombination

    let combinedOffloadSub = srtAfterKernelCombination DMap.! (head mergedOffloadName)

    addLoopGuards combinedOffloadSub

    -- mapM_ (\subRecord -> putStrLn ("\n" ++ hl ++ (fst subRecord) ++ hl ++ (miniPPProgUnit (subAst (snd subRecord))) ++ hl))
    --     (DMap.toList combinedKernelSubroutines)

    -- JM: This is simply so status information can be printed.
    -- < STEP 6a : create annotation listings >
    -- let annotationListings = map (combineAnnotationListings_map parAnnotations) combAnnotations

    -- --    < STEP 7a : >
    -- let argTranslations = extractSubroutineArgumentTranslationMaps combinedKernelSubroutines parsedMain
    --         -- WV: TODO: put these into SubRec.subCalledSubs.ArgMap or at least in SubRec.subCalledSubsArgMaps

    return ()

validateInputFiles :: Program LFT.Anno -> IO ()
validateInputFiles fileAst = do
    let results = map (\f -> f fileAst)
            [checkFilesHaveOnlyOneSubroutine]
    mapM_ printErrorOrContinue results
    return ()

banner =
    rule '=' ++
    "F4: Finite-element Fortran for FPGAs\n" ++
    "This compiler allows Fortran finite element codes to be compiled\n" ++
    "for execution on FPGA devices via OpenCL" ++
    rule '='

hl = rule '-'

rule char = "\n" ++ (take 80 (repeat char)) ++ "\n"
