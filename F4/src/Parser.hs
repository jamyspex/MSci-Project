{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import           CommandLineProcessor
-- import           Control.Monad.State.Lazy
import           Data.Generics        (everything, everywhere, everywhereM,
                                       gmapQ, gmapT, mkM, mkQ, mkT)
import qualified Data.Map             as DMap
import           Language.Fortran
import           LanguageFortranTools as LFT
import           MiniPP
import           SanityChecks
import           System.FilePath      (FilePath, (</>))

-- data ParsedProgram = ParsedProgram {
--     main       :: SubRec,
--     forOffload :: [SubRec],
--     otherSubs  :: [SubRec]
-- }

data InitialProgramData = InitialProgramData {
    mainParseResult       :: (Program Anno, [String], String),
    forOffloadParseResult :: [(Program Anno, [String], String)]
    -- ,
    -- otherSubsParseResult  :: [(Program Anno, [String], String)]
}

data SubRecAnalysis = SRA {
    subroutineToFileMap    :: DMap.Map String FilePath,
    subroutineNameToAstMap :: DMap.Map String (ProgUnit Anno),
    subroutineToCalls      :: DMap.Map String [(Fortran Anno)]
}

getAst (ast, _, _) = ast
getLines (_, lines, _) = lines
getFileName (_, _, filename) = filename

parseProgramData :: F4Opts -> IO ()
parseProgramData opts = do
    filesToBeParallelised <- mapM parseCurried $ subsForFPGA opts
    mapM_ (validateInputFile . getAst) (filesToBeParallelised)
    main <- parseCurried $ mainSub opts
    let sra = createInitialSubMap $ InitialProgramData main filesToBeParallelised -- otherSubroutines
    let sra' = populateSubCalls sra
    parseOtherRequiredFiles parseCurried sra'
    -- debug_displaySubRecAnalysis sra'
    return ()
    where
        -- otherSubroutines =
        cppD = cppDefines opts
        cppX = cppExcludes opts
        fixF = fixedForm opts
        dir = sourceDir opts
        parseCurried = Parser.parseFile cppD cppX fixF dir

parseOtherRequiredFiles :: (String -> IO (Program Anno, [String], String)) -> SubRecAnalysis -> IO (SubRecAnalysis)
parseOtherRequiredFiles parse sra = do
    -- foundSubRoutines = sra
    -- otherRequiredSubs =
    putStrLn $ concatMap (\s -> s ++ ", ") subNames
    return (sra)
    where
        -- parseCurried =
        subNames = concatMap (\(_, calls) -> concatMap extractCallSubName calls)
            $ DMap.toList (subroutineToCalls sra)

debug_displaySubRecAnalysis :: SubRecAnalysis -> IO ()
debug_displaySubRecAnalysis sra = do
    mapM_ (\(key, val) -> putStrLn (key ++ " --> " ++ val)) subFilesList
    mapM_ (\(key, val) -> putStrLn (key ++ " --> \n" ++ miniPPProgUnit val)) subAstsList
    mapM_ (\(key, val) -> putStrLn (key ++ " --> \n" ++ (concatMap (\call -> "\t" ++ miniPPF call ++ "\n") val))) subCallsList
    where
        subAstsList = DMap.toList $ subroutineNameToAstMap sra
        subCallsList = DMap.toList $ subroutineToCalls sra
        subFilesList = DMap.toList $ subroutineToFileMap sra
--                                                                          AST           Lines    Filename
parseFile :: [String] -> [String] -> Bool -> String -> String -> IO ((Program Anno, [String], String))
parseFile cppDArgs cppXArgs fixedForm dir filename = do
    parseOutput <- LFT.parseFile cppDArgs cppXArgs fixedForm path
    let ((parsedProgram, lines), _, _) = parseOutput
    -- validateInputFile parsedProgram
    return (parsedProgram, lines, path)
    where
        path = dir </> filename

-- getFilePath filename dir = case dir of
--                             Nothing -> filename
--                             Just s  -> s </> filename

-- processParsed :: InitialProgramData -> ParsedProgram
-- processParsed input =



createInitialSubMap :: InitialProgramData -> SubRecAnalysis
createInitialSubMap input = SRA (DMap.fromList toFileMap) (DMap.fromList toSubAstMap) DMap.empty
    where
        toSubAstMap =
            -- (zip otherSubNames otherAsts) ++
                [(mainSubName, mainAst)] ++
                (zip forOffloadNames forOffloadAsts)
        mainAst = (getFileAst . getAst . mainParseResult) input
        mainSubName = getSubName mainAst
        -- otherAsts = map (getFileAst . getAst) (otherSubsParseResult input)
        -- otherSubNames = map getSubName otherAsts
        forOffloadAsts = map (getFileAst . getAst) (forOffloadParseResult input)
        forOffloadNames = map getSubName forOffloadAsts
        getFileAst = head . extractMainProgUnit
        getSubName = extractProgUnitName
        toFileMap = map (\(filename, ast) -> ((getSubName. getFileAst) ast, filename)) (forOffloadFiles ++ [(mainFileName, (getAst . mainParseResult) input)])
        forOffloadFiles = map (\(ast, _, filename) -> (filename, ast)) (forOffloadParseResult input)
        mainFileName = (getFileName . mainParseResult) input

populateSubCalls :: SubRecAnalysis -> SubRecAnalysis
populateSubCalls sra = sra { subroutineToCalls = DMap.fromList callsInFiles}
    where
        fileAstsList = DMap.toList $ subroutineNameToAstMap sra
        callsInFiles = map (\(subname, ast) -> (subname, extractAllCalls ast)) fileAstsList

-- getSubCallsInAst :: (String, ProgUnit Anno) -> (String, [Fortran Anno])
-- getSubCallsInAst (subname, ast) = (subname, extractAllCalls ast)

extractProgUnitName :: ProgUnit Anno -> String
extractProgUnitName ast     |    subNames == [] = error ((show ast) ++ "\n\nextractProgUnitName: no subNames")
                            |    otherwise = extractStringFromSubName (head subNames)
        where
            subNames = everything (++) (mkQ [] getSubNames) ast

-- -- WV: a list of all subroutines in the code unit, but actually assuming there is just one
-- extractSubroutine subs
--     | null subs = NullProg nullAnno (nullSrcLoc,nullSrcLoc)
--     | otherwise = head $ extractSubroutines subs
-- extractSubroutines :: Program Anno -> [ProgUnit Anno]
-- extractSubroutines ast = everything (++) (mkQ [] extractSubroutines') ast

-- extractSubroutines' :: ProgUnit Anno -> [ProgUnit Anno]
-- extractSubroutines' codeSeg = case codeSeg of
--                                 (Sub _ _ _ _ _ _) -> [codeSeg]
--                                 _                 -> []

extractMainProgUnit' :: ProgUnit Anno -> [ProgUnit Anno]
extractMainProgUnit' codeSeg = case codeSeg of
                                (Sub _ _ _ _ _ _)  -> [codeSeg]
                                (Main _ _ _ _ _ _) -> [codeSeg]
                                _                  -> []

extractMainProgUnit :: Program Anno -> [ProgUnit Anno]
extractMainProgUnit ast = everything (++) (mkQ [] extractMainProgUnit') ast

replaceKernels :: [(Fortran Anno, Fortran Anno)] -> ProgUnit Anno -> ProgUnit Anno
replaceKernels kernelPairs subroutine = foldl (\accumSub (old, optim) -> replaceFortran accumSub old optim) subroutine kernelPairs

extractAllCalls ast = everything (++) (mkQ [] extractCalls) ast

extractCalls codeSeg = case codeSeg of
                            Call _ _ _ _ -> [codeSeg]
                            _            -> []

extractStringFromSubName :: SubName Anno -> String
extractStringFromSubName (SubName _ str) = str

extractCallSubName (Call _ _ expr _ ) = everything (++) (mkQ [] extractCallSubName') expr

extractCallSubName' :: VarName Anno -> [String]
extractCallSubName' call = case call of
                            VarName _ name -> [name]
                            -- _              -> []

validateInputFile :: Program LFT.Anno -> IO ()
validateInputFile fileAst = do
    let results = map (\f -> f fileAst)
            [checkFilesHaveOnlyOneSubroutine]
    mapM_ printErrorOrContinue results

