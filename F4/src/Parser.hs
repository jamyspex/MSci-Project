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
-- import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools as LFT
import           MiniPP
import           SanityChecks
import qualified SubroutineTable      as ST
import           System.FilePath      (FilePath, (</>))
import           System.IO.Unsafe

-- data ParsedProgram = ParsedProgram {
--     main       :: SubRec,
--     forOffload :: [SubRec],
--     otherSubs  :: [SubRec]
-- }

type SrcName = String
data SubRec = MkSubRec {
       subAst          :: ProgUnit Anno,
       subSrcFile      :: String,
       subSrcLines     :: [String],
       subsCalled      :: DMap.Map ST.SubNameStr SubRec,
       subName         :: String,
       argTranslations :: ArgumentTranslationTable,
       parallelise     :: Bool
}

-- type SubroutineArgumentTranslationMap = DMap.Map SubNameStr ArgumentTranslation
type ArgumentTranslationTable = DMap.Map ST.SubNameStr [ArgumentTranslation]


data ArgumentTranslation = ArgTrans {
    parameter :: ArgName Anno,
    argument  :: VarName Anno
} deriving Show

data InitialProgramData = InitialProgramData {
    mainParseResult       :: (Program Anno, [String], String),
    forOffloadParseResult :: [(Program Anno, [String], String)]
}

type ParseResult = (Program Anno, [String], String)

data SubRecAnalysis = SRA {
    subroutineToFile     :: DMap.Map String FilePath,
    subroutineToLines    :: DMap.Map String [String],
    subroutineToAst      :: DMap.Map String (ProgUnit Anno),
    subroutineToCalls    :: DMap.Map String (DMap.Map String (Fortran Anno)),
    subroutineToArgTrans :: DMap.Map String (DMap.Map String [ArgumentTranslation])
}

-- updateArgTransTableForSubroutine :: (ArgumentTranslation -> ArgumentTranslationTable -> ArgumentTranslationTable)
--                                  -> ArgumentTranslationTable
--                                  -> [ArgumentTranslation]
--                                  -> ArgumentTranslationTable
-- updateArgTransTableForSubroutine =

getAst (ast, _, _) = ast
getLines (_, lines, _) = lines
getFileName (_, _, filename) = filename

-- This function assumes all subroutines are in their own file
-- with the file having the same name as the subroutine
-- e.g the subroutine 'test' would be in 'test.f95'
parseProgramData :: F4Opts -> IO ()--(ST.SubroutineTable)
parseProgramData opts = do
    -- parse main and update maps in SubRecAnalysis
    main <- parseCurried $ mainSub opts
    let mainParseResult = main
    let (mainAstMapItem, mainFileMapItem, mainLinesMapItem) = getMapEntries mainParseResult
    let mainOnlySra = SRA (DMap.fromList [mainFileMapItem])
            (DMap.fromList [mainLinesMapItem])
            (DMap.fromList [mainAstMapItem])
            DMap.empty DMap.empty
    -- recursively search for other files to parse based on call statements found previously
    sraWithSubCalls <- searchForSubCalls mainOnlySra 1

    -- calculate argument translations for called subroutines
    let sraWithArgTrans = populateArgTrans sraWithSubCalls

    -- display currently parsed data
    debug_displaySubRecAnalysis sraWithArgTrans

    return ()
    -- return (createSubRoutineTable sraWithArgTrans)

    -- putStrLn $ show $ (getArgNames . getSubParams) ((subroutineToAst fullSra) DMap.! "dyn")
    -- putStrLn $ show $ (getArgNames . getSubParams) ((subroutineToAst fullSra) DMap.! "shapiro")
    -- putStrLn $ show $ (getArgNames . getSubParams) ((subroutineToAst fullSra) DMap.! "vernieuw")

    -- putStrLn $ show $ ((subroutineToCalls fullSra) DMap.! "wave2d")

    where
        cppD = cppDefines opts
        cppX = cppExcludes opts
        fixF = fixedForm opts
        dir = sourceDir opts
        parseCurried = Parser.parseFile cppD cppX fixF dir
        -- function calls itself recursively until no outstanding call statements left
        searchForSubCalls :: SubRecAnalysis -> Int -> IO (SubRecAnalysis)
        searchForSubCalls sra foundLastItr =
            -- if no new call statements found in last pass we can
            -- return there are no new files to find
            if foundLastItr == 0 then
                return (sra)
            else do
                -- based on ast data currently in SubRecAnalysis find
                -- all call statments in currently discovered files
                let sraWithSubCalls = populateSubCalls sra
                -- then using this call data find other files containing used subroutines
                otherSubsParseResults <- findOtherRequiredSubs parseCurried sraWithSubCalls
                -- update the SubRecAnalysis structure based of newly found files
                let (otherAstMapItems, otherFileMapItems, otherLineMapItems)
                        = unzip3 $ map getMapEntries otherSubsParseResults
                let sra' = populateSubCalls $ SRA (DMap.fromList (previousFileMapItems ++ otherFileMapItems))
                                                (DMap.fromList (previousLineMapItems ++ otherLineMapItems))
                                                (DMap.fromList (previousAstMapItems ++ otherAstMapItems))
                                                DMap.empty DMap.empty
                -- recursively call - length otherAstMapItems = 0 when no new files are found
                searchForSubCalls sra' (length otherAstMapItems)
            where
                previousAstMapItems = DMap.toList $ subroutineToAst sra
                previousFileMapItems = DMap.toList $ subroutineToFile sra
                previousLineMapItems = DMap.toList $ subroutineToLines sra

findOtherRequiredSubs ::  (String -> IO (ParseResult)) -> SubRecAnalysis -> IO ([ParseResult])
findOtherRequiredSubs parse sra = do
        otherSubs <- mapM parse $ map (\subname -> subname ++ ".f95") otherUsedSubs
        return (otherSubs)
    where
        otherUsedSubs = filter (not . (flip elem) previouslyFound) allUsedSubNames
        previouslyFound = (map (\(subname, _) -> subname) $ DMap.toList (subroutineToAst sra))
        allUsedSubNames = concatMap (\(_, calls) -> concatMap extractCallSubName calls)
            $ DMap.toList (subroutineToCalls sra)

                                    -- computeSubRoutineArgTranslations :: SubRecAnalysis -> SubRecAnalysis


-- getCallStatements :: ProgUnit Anno -> Fortran Anno
-- getCallStatements sub =

-- findCallFromMethod
constructSubroutineTable :: SubRecAnalysis -> [SubRec]
constructSubroutineTable sra = 
    

populateArgTrans :: SubRecAnalysis -> SubRecAnalysis
populateArgTrans sra = sra { subroutineToArgTrans = DMap.fromList forConversionToArgTransMap }

    where
        subNames = map (\(key, _) -> key) $ DMap.toList (subroutineToAst sra)
        argNamesForSubCall = (getArgNames . getSubParams . ((subroutineToAst sra) DMap.!))
        subNameParamMap = DMap.fromList $ map (\name -> (name, argNamesForSubCall name)) subNames
        forConversionToArgTransMap = map (\(calledFrom, callsMap) ->
            (calledFrom, DMap.fromList $ map (\(calledSub, callStatement) ->
                (calledSub, buildArgTransMapValue (subNameParamMap DMap.! calledSub) (getVarNamesFromCall callStatement))) $ DMap.toList callsMap))
            $ DMap.toList (subroutineToCalls sra)

buildArgTransMapValue :: [ArgName Anno] -> [VarName Anno] -> [ArgumentTranslation]
buildArgTransMapValue argNames varNames = map (\(arg, var) -> ArgTrans arg var) matched
    where
        matched = zip argNames varNames


getVarNamesFromCall :: Fortran Anno -> [VarName Anno]
getVarNamesFromCall (Call _ _ _ arglist) = extractVarNamesFromCall arglist

extractVarNamesFromCall :: ArgList Anno -> [VarName Anno]
extractVarNamesFromCall (ArgList _ expr) = everything (++) (mkQ [] extractVarNamesFromCall') expr


extractVarNamesFromCall' :: Expr Anno -> [VarName Anno]
extractVarNamesFromCall' expr = case expr of
                            Var _ _ varnameList -> map (\(varname, _) -> varname) varnameList
                            _ -> []

-- temp :: Expr Anno -> [VarName Anno]
-- temp expr = case expr of
--             VarName _
--                                             --error "Expr in ArgList more complicated than just VarName"

getArgNames :: Arg Anno -> [ArgName Anno]
getArgNames (Arg _ argnames _) = everything (++) (mkQ [] extractArgNames) argnames

extractArgNames :: ArgName Anno -> [ArgName Anno]
extractArgNames argname = case argname of
                        a@(ArgName _ _) -> [a]
                        _               -> []

getSubParams :: ProgUnit Anno -> Arg Anno
getSubParams sub = head $ everything (++) (mkQ [] extractSubArgs) sub
-- computeSubRoutineArgTranslations sra =

extractSubArgs :: ProgUnit Anno -> [Arg Anno]
extractSubArgs prog = case prog of
                            Main _ _ _  arg _  _ -> [arg]
                            Sub  _ _ _ _ arg _   -> [arg]
                            _                    -> []

debug_displaySubRecAnalysis :: SubRecAnalysis -> IO ()
debug_displaySubRecAnalysis sra = do
    mapM_ (\(key, val) -> putStrLn (key ++ " --> " ++ val)) subFilesList
    mapM_ (\(key, val) -> putStrLn (key ++ " --> \n" ++ miniPPProgUnit val)) subAstsList
    mapM_ (\(key, val) -> putStrLn (key ++ " --> \n" ++
        (concatMap (\(subname, call) -> "\t" ++ subname ++ "->" ++ miniPPF call ++ "\n" ++ show call ++ "\n\n") $ DMap.toList val))) subCallsList
    mapM_ (\(key, val) -> putStrLn (key ++ " --> \n" ++
        (concatMap (\(subname, argTransList) -> "\t" ++ subname ++ "->\n" ++
            (concatMap (\argTrans -> "\t" ++ show argTrans ++ "\n") argTransList)) $ DMap.toList val))) subArgTransList
    where
        subAstsList = DMap.toList $ subroutineToAst sra
        subCallsList = DMap.toList $ subroutineToCalls sra
        subFilesList = DMap.toList $ subroutineToFile sra
        subArgTransList = DMap.toList $ subroutineToArgTrans sra
--                                                                          AST           Lines    Filename
parseFile :: [String] -> [String] -> Bool -> String -> String -> IO ((Program Anno, [String], String))
parseFile cppDArgs cppXArgs fixedForm dir filename = do
    parseOutput <- LFT.parseFile cppDArgs cppXArgs fixedForm path
    let ((parsedProgram, lines), _, _) = parseOutput
    -- validateInputFile parsedProgram
    return (parsedProgram, lines, path)
    where
        path = dir </> filename

getMapEntries :: ParseResult -> ((String, ProgUnit Anno), (String, String), (String, [String]))
getMapEntries (ast, lines, filename) = ((subname, subAst), (subname, filename), (subname, lines))
    where
        subAst = getFileAst ast
        subname = getSubName subAst

getFileAst = head . extractMainProgUnit
getSubName = extractProgUnitName

populateSubCalls :: SubRecAnalysis -> SubRecAnalysis
populateSubCalls sra = sra { subroutineToCalls = DMap.fromList subnamesToCallsMap}
    where
        fileAstsList = DMap.toList $ subroutineToAst sra
        callsInFiles = map (\(subname, ast) -> (subname, extractAllCalls ast)) fileAstsList
        subnamesToCallsMap = map (\(subname, calls) ->
            (subname, DMap.fromList $ map (\call -> (getCalledSubName call, call)) calls)) callsInFiles

getCalledSubName :: Fortran Anno -> String
getCalledSubName call@(Call _ _ (expr) _) = case expr of
                                Var _ _ (((VarName _ name), _):rest) -> name
                                _ -> error "sub call expr more complicated than you thought"

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

