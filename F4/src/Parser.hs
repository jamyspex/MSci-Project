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

-- data ParsedProgram = ParsedProgram {
--     main       :: SubRec,
--     forOffload :: [SubRec],
--     otherSubs  :: [SubRec]
-- }

data InitialProgramData = InitialProgramData {
    mainParseResult       :: (Program Anno, [String], String),
    forOffloadParseResult :: [(Program Anno, [String], String)],
    otherSubsParseResult  :: [(Program Anno, [String], String)]
}

data SubRecAnalysis = SRA {
    subroutineNameToAstMap :: DMap.Map String (ProgUnit Anno)
}

getAst (ast, _, _) = ast
getLines (_, lines, _) = lines
getFileName (_, _, filename) = filename

parseProgramData :: F4Opts -> IO ()
parseProgramData opts = do
    filesToBeParallelised <- mapM parseCurried $ subsForFPGA opts
    main <- parseCurried $ mainSub opts
    otherSubroutines <- mapM parseCurried $ otherSubs opts
    mapM_ (validateInputFiles . getAst) (filesToBeParallelised ++ otherSubroutines)
    let sra = createSubMap $ InitialProgramData main filesToBeParallelised otherSubroutines
    debug_displaySubRecAnalysis sra
    return ()
    where
        cppD = cppDefines opts
        cppX = cppExcludes opts
        fixF = fixedForm opts
        parseCurried = Parser.parseFile cppD cppX fixF

debug_displaySubRecAnalysis :: SubRecAnalysis -> IO ()
debug_displaySubRecAnalysis sra = do
    mapM_ (\(key, val) -> putStrLn (key ++ " --> \n" ++ miniPPProgUnit val)) asList
    where
        asList = DMap.toList $ subroutineNameToAstMap sra
--                                                           AST           Lines    Filename
parseFile :: [String] -> [String] -> Bool -> String -> IO ((Program Anno, [String], String))
parseFile cppDArgs cppXArgs fixedForm filename = do
    parseOutput <- LFT.parseFile cppDArgs cppXArgs fixedForm filename
    let ((parsedProgram, lines), _, _) = parseOutput
    return (parsedProgram, lines, filename)

-- processParsed :: InitialProgramData -> ParsedProgram
-- processParsed input =

createSubMap :: InitialProgramData -> SubRecAnalysis
createSubMap input = SRA $ DMap.fromList toMap
    where
        toMap :: [(String, ProgUnit Anno)] = (zip otherSubNames otherAsts) ++
                (zip forOffloadNames forOffloadAsts)
        -- mainAst = (extractSubroutines . getAst . mainParseResult) input
        -- mainSubName = getSubName mainAst
        otherAsts = map (getSubAst . getAst) (otherSubsParseResult input)
        otherSubNames = map getSubName otherAsts
        forOffloadAsts = map (getSubAst . getAst) (forOffloadParseResult input)
        forOffloadNames = map getSubName forOffloadAsts
        getSubAst = head . extractSubroutines
        getSubName = extractProgUnitName


extractProgUnitName :: ProgUnit Anno -> String
extractProgUnitName ast     |    subNames == [] = error ((show ast) ++ "\n\nextractProgUnitName: no subNames")
                            |    otherwise = extractStringFromSubName (head subNames)
        where
            subNames = everything (++) (mkQ [] getSubNames) ast

-- WV: a list of all subroutines in the code unit, but actually assuming there is just one
extractSubroutine subs
    | null subs = NullProg nullAnno (nullSrcLoc,nullSrcLoc)
    | otherwise = head $ extractSubroutines subs
extractSubroutines :: Program Anno -> [ProgUnit Anno]
extractSubroutines ast = everything (++) (mkQ [] extractSubroutines') ast

extractSubroutines' :: ProgUnit Anno -> [ProgUnit Anno]
extractSubroutines' codeSeg = case codeSeg of
                                (Sub _ _ _ _ _ _) -> [codeSeg]
                                _                 -> []

replaceKernels :: [(Fortran Anno, Fortran Anno)] -> ProgUnit Anno -> ProgUnit Anno
replaceKernels kernelPairs subroutine = foldl (\accumSub (old, optim) -> replaceFortran accumSub old optim) subroutine kernelPairs

extractAllCalls ast = everything (++) (mkQ [] extractCalls) ast

extractCalls codeSeg = case codeSeg of
                            Call _ _ _ _ -> [codeSeg]
                            _            -> []

extractStringFromSubName :: SubName Anno -> String
extractStringFromSubName (SubName _ str) = str

validateInputFiles :: Program LFT.Anno -> IO ()
validateInputFiles fileAst = do
    let results = map (\f -> f fileAst)
            [checkFilesHaveOnlyOneSubroutine]
    mapM_ printErrorOrContinue results

