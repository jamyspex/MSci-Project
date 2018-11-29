{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module F4 where

import           Parser

import           ConstantFolding
import           Data.Generics           (everything, everywhere, everywhereM,
                                          gmapQ, gmapT, mkM, mkQ, mkT)
import qualified Data.Map                as DMap
import           Language.Fortran
import           Language.Fortran.Pretty
import qualified LanguageFortranTools    as LFT
import           MiniPP
import           System.Console.CmdArgs


processArgs :: IO ()
processArgs = compilerMain =<< cmdArgs f4Opts

compilerMain:: F4Opts -> IO ()
compilerMain args = do
    filesToBeParallelised <-
        mapM (parseFile (cppDefines args) (cppExcludes args) (fixedForm args)) $ files args
    putStrLn ((show $ length filesToBeParallelised) ++ " files parsed!")
    let constantFolded = map (everywhere (mkT foldConstants)) filesToBeParallelised
    mapM_ (\ast -> putStrLn $ miniPPProgram ast) constantFolded
    return ()
    -- parseTestFile


parseTestFile :: IO ()
parseTestFile = do
    parseOutput <- LFT.parseFile [] [] False "Shallow-Water-2D/dyn.f95"
    let
        (parsedProgram, stash, moduleVarTable) = parseOutput
        stashValues = snd stash
        astObj = fst parsedProgram

    putStrLn $ "AST: " ++ (show astObj)
    -- playAboutWithAst astObj
    putStrLn $ "Program lines:"
    mapM_ putStrLn $ map (\line -> "\t" ++ line) $ snd parsedProgram

    -- writeFile "./ast.txt" $ show astObj

    -- writeFile "./preprocessed.f95" $ unlines (snd parsedProgram)

    putStrLn $ "Code stash name: " ++ (fst stash)

    let folded = everywhere (mkT foldConstants) astObj

    let astPP = miniPPProgram astObj

    let foldedPP = miniPPProgram folded

    putStrLn astPP

    writeFile "./fromAst.f95" astPP

    writeFile "./folded.f95" foldedPP

    -- if (length $ DMap.keys stashValues) > 0 then
    --     printStash stashValues
    -- else
    --     putStrLn "No entries in code stash"

    -- if (length $ DMap.keys moduleVarTable) > 0 then
    --     printModVarTable moduleVarTable
    -- else
    --     putStrLn "No entries in module var table"

    -- putStrLn $ "Program String" ++ (snd parsedProgram)
    return ()


-- getLoopVariables :: Fortran Anno -> [String]
-- getLoopVariables (For _ _ (VarName _ name) e1 e2 e3 _) = [name]
-- getLoopVariables _                                     = []

-- getVarName :: VarName Anno -> [String]
-- getVarName (VarName _ name) = [name]

-- playAboutWithAst :: Program Anno -> IO (Program Anno)
-- playAboutWithAst ast = do
--     putStrLn $ show $ everything (++) (mkQ [] getLoopVariables) ast
--     return ast

-- printModVarTable :: DMap.Map String String -> IO ()
-- printModVarTable table = do
--     putStrLn $ "Mod var table values: \n"
--     mapM_ putStrLn (map (printModVarTableEntry table) $ DMap.keys table)

-- printModVarTableEntry :: DMap.Map String String -> String -> String
-- printModVarTableEntry moduleVarTable key = formattedItem where
--     keyText = "Key: " ++ (show key) ++ " --->\n"
--     entry = moduleVarTable DMap.! key
--     formattedItem = keyText ++ entry

-- printStash :: DMap.Map Int [String] -> IO ()
-- printStash stash = do
--     putStrLn $ "Code stash values: \n"
--     mapM_ putStrLn (map (printStashEntry stash) $ DMap.keys stash)

-- printStashEntry :: DMap.Map Int [String] -> Int -> String
-- printStashEntry stash key = formattedItem where
--     keyText = "Key: " ++ (show key) ++ " --->\n"
--     entries = stash DMap.! key
--     formattedEntries = foldl (\acc cur -> acc ++ "\n\t" ++ cur) "\t" entries
--     formattedItem = keyText ++ formattedEntries

data F4Opts =
    F4Opts {
        files       :: [FilePath],
        cppDefines  :: [String],
        cppExcludes :: [String],
        fixedForm   :: Bool
    }
    deriving (Data,Typeable,Show,Eq)

f4Opts = F4Opts {
        files = def &= opt "files" &= name "f" &= typFile &= help "Files to offload to FPGA",
        cppDefines = def &= opt "cppDefine" &= typ "NAME[=VALUE]" &= help "CPP #define",
        cppExcludes = def &= opt "cppExcludes" &= typ "NAME[=VALUE]" &= help "CPP include path",
        fixedForm = def &= help "Fixed form: limit input file lines to 72 columns"
    } &=
    verbosity &=
    help "Compiler to convert FORTRAN finite element codes to be executed on FPGA devices" &=
    summary "F4 v0.0.0, (C) James Macdonald" &=
    details ["F4 is a source-to-source compiler that allows FORTRAN finite element codes to be compiled to OpenCL optimsed for execution on FPGAs."]
