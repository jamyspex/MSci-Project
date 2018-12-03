module Main

where

-- --     This is the top level of the whole compiler. This module makes calls to all of the analysis, code emission and loop fusion elements of the
-- --     the whole process.

-- --    High Level Steps
-- --        1)    Command line arguments are processed
-- --        2)    The main program and files listed after the '-modules' flag are parsed and held separately
-- --        3)    The non-main files are processed to separate out individual subroutines and place them in a 'subroutine table'
-- --        4)    Each one of the subroutines is parallelised using functions located in 'Transformer.hs', which in turn uses
-- --            functionality present in 'LoopAnalysis.hs'
-- --        5)    Each of the subroutines is then subjected to loop/kernel fusion, in an attempt to reduce the number of kernels
-- --            That will be called.
-- --        6)    Annotations that describe obstacles to parallelism and information of loop fusion are then extracted and later
-- --            presented to the user (if in verbose mode).
-- --        7)    At this point, two divergent paths are taken using the current subroutine ASTs.
-- --            a)    One version of the ASTs goes through buffer transfer optimisation which involves reducing the number of variables
-- --                that make up the read and written arguments for a kernel. Reducing these arguments results in less buffer manipulation and
-- --                therefore less memory use in the final program. This process (and the final emission process) requires argument translation
-- --                 information and so that it generated at this stage too. The argument translation information is Data.Map for each subroutine.
-- --                where each key is a variable name in the subroutine and the associated item is the name of that variable in the main, when you
-- --                consider calls to the subroutine. This is used to ensure that global buffers are used correctly.
-- --            b)    The other version of the ASTs is used to produce a list of (AST, filename) pairs. This list is used during code emission
-- --                to produce kernel code as it is necessary to know the filenames for each AST to ease the generation process.
-- --             c)     The buffer optimised versions of the ASTs also go through this process as the non buffer optimised version as they are needed
-- --                with their filenames to generate host code.
-- --        8)    Finally, code is emitted.

-- {-
-- Concerns/TODO
--  - Improve naming conventions
--  - Convert clusters of buffer reads/writes to subroutine calls
--  - Consider the possibility of a call to a host subroutine happening after a call to an OpenCL kernel inside a small loop. The vars that are written by the host must therefore be rewritten to buffers at the start of the loop
--  - Problem with reducing into array elements.
--    For example, in press when p(i,j,k) is a reduction variable in the sor loop. Problem seems to be worse when loop is detected as an iterative reduction rather than a normal reduction (this loop is a normal reduction when the calls to boundp1 and boundp2 are commented out). The issue may stem from the fact that no initial value for the array element may be produced. It is this problem that means that p is not read back to the host for calls to boundp1 and boundp2

-- -}

-- import           Data.Generics          (Data, Typeable, everything, everywhere,
--                                          gmapQ, gmapT, mkQ, mkT)
-- import qualified Data.Map               as DMap
-- import           Language.Fortran
-- import           System.Environment

-- import           BufferTransferAnalysis (optimiseBufferTransfers,
--                                          replaceSubroutineAppearances)
-- import           CodeEmitter            (emit)
-- import           LanguageFortranTools   (Anno, appendToMap, compilerName,
--                                          errorLocationFormatting, generateVar,
--                                          nullAnno, nullSrcSpan, outputTab,
--                                          parseFile)
-- import           MiniPP                 (miniPPProgUnit)
-- import           Platform
-- import           SubroutineTable        (SubRec (..), addToSubroutineTable,
--                                          constructSubroutineTable,
--                                          extractSubroutineArgumentTranslationMaps)
-- import           Transformer            (combineKernelProgUnit_foldl,
--                                          paralleliseProgUnit_foldl)


-- hl = rule '-'

-- rule char = "\n" ++ (take 80 (repeat char)) ++ "\n"

-- -- main :: IO [()]
-- main = do
--     -- < STEP 1 : Argument processin >
--     args <- getArgs
--     let argMap = processArgs args
--     let filenames = case DMap.lookup filenameFlag argMap of
--                         Just filenames -> filenames
--                         Nothing        -> usageError
--     let ioWriteSubroutines = case DMap.lookup ioWriteRoutineFlag argMap of
--                         Just subs -> subs
--                         Nothing   -> []
--     let ioReadSubroutines = case DMap.lookup ioReadRoutineFlag argMap of
--                         Just subs -> subs
--                         Nothing   -> []
--     let mainFilename = case DMap.lookup mainFileFlag argMap of
--                         Just filenames -> head filenames
--                         Nothing        -> usageError
--     let outDirectory = case DMap.lookup outDirectoryFlag argMap of
--                         Just dirList -> head dirList
--                         Nothing      -> "./"
--     let noLoopFusion = case DMap.lookup noLoopFusionFlag argMap of
--                         Just a  -> True
--                         Nothing -> False
--     let
--         loopFusionBound :: Maybe Float
--         loopFusionBound = if noLoopFusion
--                             then Just 0.0
--                             else case DMap.lookup loopFusionBoundFlag argMap of
--                                     Just bound -> Just (read (head bound) :: Float)
--                                     Nothing -> Nothing
--     let verbose = case DMap.lookup verboseFlag argMap of
--                         Just a  -> True
--                         Nothing -> False
--     let fixedForm = case DMap.lookup fixedFormFlag argMap of
--                         Just a  -> True
--                         Nothing -> False
--     let plat = case DMap.lookup platFlag argMap of
--                 Just [p] ->  (read p) :: Platform
--                 Nothing  -> GPU -- defaults to GPU
--     let cppDFlags = DMap.findWithDefault [] cppDefineFlag argMap
--     let cppXFlags = DMap.findWithDefault [] cppExcludeFlag argMap

--     -- < STEP 2 : Parsing >
--     parsedPrograms_stashes <- mapM (parseFile cppDFlags cppXFlags fixedForm) filenames
--     let
--         (parsedPrograms,stashes,moduleVarTables) = unzip3 parsedPrograms_stashes
--     (parsedMain,mainStash,mainModuleVarTable) <- parseFile cppDFlags cppXFlags fixedForm mainFilename
--     -- < STEP 3 : Construct subroutine AST lists>
--     let parsedSubroutines' = constructSubroutineTable (zip parsedPrograms filenames)
--     let subroutineNames = DMap.keys parsedSubroutines'
--     let parsedSubroutines = parsedSubroutines'

--     -- < STEP 4 : Parallelise the loops >
--     -- WV: this is the equivalent of calling a statefull pass on every subroutine.
--     let (parallelisedSubroutines, parAnnotations) = foldl (paralleliseProgUnit_foldl ioWriteSubroutines parsedSubroutines') (DMap.empty, []) subroutineNames

--     mapM_ (\subRecord -> putStrLn ("\n" ++ hl ++ (fst subRecord) ++ hl ++ (miniPPProgUnit (subAst (snd subRecord))) ++ hl))
--         (DMap.toList parallelisedSubroutines)

--     -- < STEP 5 : Try to fuse the parallelised loops as much as possible (on a per-subroutine basis) >
--     -- (SubroutineTable, [(String, String)])
--     let (combinedKernelSubroutines, combAnnotations) = foldl (combineKernelProgUnit_foldl loopFusionBound) (parallelisedSubroutines, []) subroutineNames

--     putStrLn ((rule '+') ++ " Combined " ++ (rule '+'))

--     mapM_ (\subRecord -> putStrLn ("\n" ++ hl ++ (fst subRecord) ++ hl ++ (miniPPProgUnit (subAst (snd subRecord))) ++ hl))
--         (DMap.toList combinedKernelSubroutines)

--     -- JM: This is simply so status information can be printed.
--     -- < STEP 6a : create annotation listings >
--     let annotationListings = map (combineAnnotationListings_map parAnnotations) combAnnotations

--     --    < STEP 7a : >
--     let argTranslations = extractSubroutineArgumentTranslationMaps combinedKernelSubroutines parsedMain
--     -- WV: TODO: put these into SubRec.subCalledSubs.ArgMap or at least in SubRec.subCalledSubsArgMaps

--     mapM_ (\argTrans ->
--         putStrLn ( hl ++ fst argTrans ++ hl ++ (concatMap (\(VarName _ key, VarName _ val) -> "\t" ++ key ++ " -> " ++ val ++ "\n")
--         (DMap.toList $ snd argTrans))))
--         $ filter (\(name, _) -> name /= "init") (DMap.toList argTranslations)

--     -- WV: This is host-side
--     let (optimisedBufferTransfersSubroutines, newMainAst) = optimiseBufferTransfers (ioWriteSubroutines,ioReadSubroutines) combinedKernelSubroutines argTranslations parsedMain

--     --    < STEP 7b : >
--     --    WV: this is kernel-side
--     let parallelisedSubroutineList = map (\x -> DMap.findWithDefault (error "parallelisedSubroutineList") x combinedKernelSubroutines) subroutineNames
--     let fileCoordinated_parallelisedMap = foldl (\dmap (MkSubRec ast filename []) -> appendToMap filename ast dmap) DMap.empty parallelisedSubroutineList
--     let fileCoordinated_parallelisedList = map (\x -> (DMap.findWithDefault (error "fileCoordinated_parallelisedMap") x fileCoordinated_parallelisedMap, x)) filenames

--     --    < STEP 7c : >
--     --    WV: host-side again
--     let
--         code_units_w_orig_lines = replaceSubroutineAppearances optimisedBufferTransfersSubroutines parsedPrograms
--         code_units = map fst code_units_w_orig_lines
--         fileCoordinated_bufferOptimisedPrograms = zip code_units filenames

--     -- < STEP 6b : Print annotation listings >
--     mapM (\(filename, par_anno, comb_anno) -> putStr $ compilerName ++ ": Analysing " ++ filename ++ (if verbose then "\n\n" ++ par_anno ++ "\n" ++ comb_anno ++ "\n" else "\n")) annotationListings

--     -- < STEP 8 : Emitter >
--     putStrLn $ compilerName ++ ": Synthesising OpenCL files"
--     putStrLn $ "Output dir: " ++ outDirectory
--     -- WV: added parsedSubroutines
--     -- WV: This is a bit strange, to deal with the kernel and host-side code in one step
--     emit outDirectory cppDFlags cppXFlags plat fixedForm fileCoordinated_parallelisedList fileCoordinated_bufferOptimisedPrograms argTranslations (newMainAst, mainFilename) [] [] parsedSubroutines (mainStash,stashes) (mainModuleVarTable,moduleVarTables)-- < STEP 8 >

-- filenameFlag = "-modules"
-- outDirectoryFlag = "-out"
-- loopFusionBoundFlag = "-lfb"
-- verboseFlag = "-v"
-- ioWriteRoutineFlag = "-iowrite"
-- ioRWRoutineFlag = "-iorw"
-- ioReadRoutineFlag = "-ioread"
-- mainFileFlag = "-main"
-- cppDefineFlag = "-D"
-- cppExcludeFlag = "-X"
-- fixedFormFlag = "-ffixed-form"
-- platFlag = "-plat"
-- noLoopFusionFlag = "-N"

-- flags = [filenameFlag, outDirectoryFlag, loopFusionBoundFlag, platFlag, cppDefineFlag, cppExcludeFlag, verboseFlag, mainFileFlag, ioWriteRoutineFlag, ioReadRoutineFlag, fixedFormFlag,noLoopFusionFlag]

-- processArgs :: [String] -> DMap.Map String [String]
-- processArgs [] = usageError
-- processArgs (flag:arg:args)
--     |    elem flag flags = gatherFlag flag (arg:args) []
--     |    otherwise         = gatherFlag filenameFlag (flag:arg:args) []

-- processArgs' :: [String] -> DMap.Map String [String]
-- processArgs' (flag:arg:args)
--     |    elem flag flags = gatherFlag flag (arg:args) []
--     |    otherwise = error (flag ++ " not a recognised argument")
-- processArgs' (flag:arg)            =    gatherFlag flag arg []
-- processArgs' [] = DMap.empty

-- gatherFlag :: String -> [String] -> [String] -> DMap.Map String [String]
-- gatherFlag flag (arg:args)  collected
--     |    elem arg flags = DMap.insert flag collected (processArgs' (arg:args))
--     |    otherwise = gatherFlag flag args (collected ++ [arg])
-- gatherFlag flag [] collected = DMap.insert flag collected (processArgs' [])

-- addArg :: DMap.Map String [String] -> String -> String -> DMap.Map String [String]
-- addArg argMap flag value = DMap.insert flag newValues argMap
--         where
--             oldValues = DMap.findWithDefault [] flag argMap
--             newValues = oldValues ++ [value]

-- usageError = error "USAGE: [<filename1>,<filename2>,... ] -main <filename> [<flag> <value>]"

-- combineAnnotationListings_map :: [(String, String)] -> (String, String) -> (String, String, String)
-- combineAnnotationListings_map annotations (currentFilename, currentAnno) =
--     foldl (\accum (filename, anno) ->
--             if filename == currentFilename
--                 then (filename, currentAnno, anno)
--                 else accum) (currentFilename, currentAnno, "") annotations
