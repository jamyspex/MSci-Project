{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import           CommandLineProcessor
-- import           Control.Monad.State.Lazy
import           Data.Generics        (everything, everywhere, everywhereM,
                                       gmapQ, gmapT, mkM, mkQ, mkT)
import           Data.List
import qualified Data.Map             as DMap
import           Language.Fortran
import           LanguageFortranTools as LFT
import           MiniPP
import           SanityChecks
import           System.FilePath      (FilePath, (</>))
import           System.IO.Unsafe

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

type ParseResult = (Program Anno, [String], String)

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
    main <- parseCurried $ mainSub opts
    let mainParseResult = main
    let (mainAstMapItem, mainFileMapItem) = getMapEntries mainParseResult
    let mainOnlySra = SRA (DMap.fromList [mainFileMapItem]) (DMap.fromList [mainAstMapItem]) DMap.empty
    fullSra <- searchForSubCalls mainOnlySra 1
    debug_displaySubRecAnalysis fullSra
    return ()
    where
        cppD = cppDefines opts
        cppX = cppExcludes opts
        fixF = fixedForm opts
        dir = sourceDir opts
        parseCurried = Parser.parseFile cppD cppX fixF dir
        searchForSubCalls :: SubRecAnalysis -> Int -> IO (SubRecAnalysis)
        searchForSubCalls sra foundLastItr =
            if foundLastItr == 0 then
                return (sra)
            else do
                let sraWithSubCalls = populateSubCalls sra
                otherSubsParseResults <- findOtherRequiredSubs parseCurried sraWithSubCalls
                let (otherAstMapItems, otherFileMapItems) = unzip $ map getMapEntries otherSubsParseResults
                let sra' = populateSubCalls $ SRA (DMap.fromList (previousFileMapItems ++ otherFileMapItems))
                                            (DMap.fromList (previousAstMapItems ++ otherAstMapItems))
                                            DMap.empty
                searchForSubCalls sra' (length otherAstMapItems)
                where
                    previousAstMapItems = DMap.toList $ subroutineNameToAstMap sra
                    previousFileMapItems = DMap.toList $ subroutineToFileMap sra

findOtherRequiredSubs ::  (String -> IO (ParseResult)) -> SubRecAnalysis -> IO ([ParseResult])
findOtherRequiredSubs parse sra = do
    otherSubs <- mapM parse $ map (\subname -> subname ++ ".f95") otherUsedSubs
    return (otherSubs)
    where
        otherUsedSubs = filter (not . (flip elem) previouslyFound) allUsedSubNames
        previouslyFound = (map (\(subname, _) -> subname) $ DMap.toList (subroutineNameToAstMap sra))
        allUsedSubNames = concatMap (\(_, calls) -> concatMap extractCallSubName calls)
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

getMapEntries :: ParseResult -> ((String, ProgUnit Anno), (String, String))
getMapEntries (ast, lines, filename) = ((subname, subAst), (subname, filename))
    where
        subAst = getFileAst ast
        subname = getSubName subAst

getFileAst = head . extractMainProgUnit
getSubName = extractProgUnitName

populateSubCalls :: SubRecAnalysis -> SubRecAnalysis
populateSubCalls sra = sra { subroutineToCalls = DMap.fromList callsInFiles}
    where
        fileAstsList = DMap.toList $ subroutineNameToAstMap sra
        callsInFiles = map (\(subname, ast) -> (subname, extractAllCalls ast)) fileAstsList

extractProgUnitName :: ProgUnit Anno -> String
extractProgUnitName ast     |    subNames == [] = error ((show ast) ++ "\n\nextractProgUnitName: no subNames")
                            |    otherwise = extractStringFromSubName (head subNames)
        where
            subNames = everything (++) (mkQ [] getSubNames) ast

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

