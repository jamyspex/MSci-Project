{-# LANGUAGE QuasiQuotes, FlexibleInstances #-} 

module Parser (parseTestFile) where

import LanguageFortranTools
import qualified Data.Map as DMap
import Language.Fortran
import MiniPP
import Language.Fortran.Pretty

import Data.Generics                 (mkQ, mkT, mkM, gmapQ, gmapT, everything, everywhere, everywhereM)


blockToFortran :: Block Anno -> Fortran Anno 
blockToFortran (Block _ _ _ _ _ f) = f

getFortranFromProgUnit :: ProgUnit Anno -> [Fortran Anno]
getFortranFromProgUnit (Main _ _ _ _ b p) = [blockToFortran b] ++ concatMap getFortranFromProgUnit p
getFortranFromProgUnit (Sub _ _ _ _ _ b) = [blockToFortran b]
getFortranFromProgUnit (Function _ _ _ _ _ _ b) = [blockToFortran b]
getFortranFromProgUnit (Module _ _ _ _ _ _ p) = concatMap getFortranFromProgUnit p
getFortranFromProgUnit (BlockData _ _ _ _ _ _) = []
getFortranFromProgUnit (PSeq _ _ p1 p2) = getFortranFromProgUnit p1 ++ getFortranFromProgUnit p2
getFortranFromProgUnit (Prog _ _ p) = getFortranFromProgUnit p
getFortranFromProgUnit (NullProg _ _) = []
getFortranFromProgUnit (IncludeProg _ _ _ (Just f)) = [f]
getFortranFromProgUnit (IncludeProg _ _ _ (Nothing)) = [] 

getFortranForProgram :: Program Anno -> [Fortran Anno]
getFortranForProgram prog = concatMap getFortranFromProgUnit prog

printFortranAnno :: Fortran Anno -> IO (Fortran Anno)
printFortranAnno input = do
    putStrLn $ miniPPF input
    return input
    
parseTestFile :: IO ()
parseTestFile = do
    parseOutput <- parseFile [] [] False "Shallow-Water-2D/main.f95"
    let
        (parsedProgram, stash, moduleVarTable) = parseOutput
        stashValues = snd stash
        astObj = fst parsedProgram
    putStrLn $ "AST: " ++ (show astObj)
    playAboutWithAst astObj 
    putStrLn $ "Program lines:"
    mapM_ putStrLn $ map (\line -> "\t" ++ line) $ snd parsedProgram

    writeFile "./preprocessed.f95" $ unlines (snd parsedProgram)

    putStrLn $ "Code stash name: " ++ (fst stash)

    -- everywhereM (mkM printFortranAnno) astObj

    let astPP = miniPPProgram astObj

    putStrLn astPP

    writeFile "./fromAst.f95" astPP

    if (length $ DMap.keys stashValues) > 0 then
        printStash stashValues
    else
        putStrLn "No entries in code stash"

    if (length $ DMap.keys moduleVarTable) > 0 then
        printModVarTable moduleVarTable
    else
        putStrLn "No entries in module var table"

    -- putStrLn $ "Program String" ++ (snd parsedProgram)
    return ()


getLoopVariables :: Fortran Anno -> [String]
getLoopVariables (For _ _ (VarName _ name) e1 e2 e3 _) = [name]
getLoopVariables _ = []

getVarName :: VarName Anno -> [String]
getVarName (VarName _ name) = [name]

playAboutWithAst :: Program Anno -> IO (Program Anno)
playAboutWithAst ast = do
    putStrLn $ show $ everything (++) (mkQ [] getLoopVariables) ast
    return ast

printModVarTable :: DMap.Map String String -> IO ()
printModVarTable table = do
    putStrLn $ "Mod var table values: \n" 
    mapM_ putStrLn (map (printModVarTableEntry table) $ DMap.keys table) 

printModVarTableEntry :: DMap.Map String String -> String -> String
printModVarTableEntry moduleVarTable key = formattedItem
    where 
        keyText = "Key: " ++ (show key) ++ " --->\n"
        entry = moduleVarTable DMap.! key
        formattedItem = keyText ++ entry

printStash :: DMap.Map Int [String] -> IO ()
printStash stash = do
    putStrLn $ "Code stash values: \n" 
    mapM_ putStrLn (map (printStashEntry stash) $ DMap.keys stash)

printStashEntry :: DMap.Map Int [String] -> Int -> String
printStashEntry stash key = formattedItem
    where 
        keyText = "Key: " ++ (show key) ++ " --->\n"
        entries = stash DMap.! key
        formattedEntries = foldl (\acc cur -> acc ++ "\n\t" ++ cur) "\t" entries
        formattedItem = keyText ++ formattedEntries