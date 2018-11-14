{-# LANGUAGE QuasiQuotes, FlexibleInstances #-} 

module Parser (parseTestFile) where

import LanguageFortranTools
import qualified Data.Map as DMap
import Language.Fortran
import qualified QuickPlot 
import QuickPlot.Plotly
import Control.Concurrent
import MiniPP
import Language.Fortran.Pretty

import Data.Generics                 (mkQ, mkT, mkM, gmapQ, gmapT, everything, everywhere, everywhereM)

-- import Data.Aeson (FromJSON, ToJSON, decode, encode)
-- import qualified Data.ByteString.Lazy.Char8 as BL

-- instance FromJSON (Program Anno)
-- instance ToJSON (Program Anno)

-- plot :: (QuickPlot.Plottable p)
--      => MVar ()
--      -> p
--      -> IO ()
-- plot isStarted content = do
--     takeMVar isStarted
--     QuickPlot.plot content
--     putStrLn "Press enter to continue..."
--     getLine 
--     putMVar isStarted ()
--     return ()

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

    putStrLn $ "Code stash name: " ++ (fst stash)


    everywhereM (mkM printFortranAnno) astObj

    -- putStrLn astObj
    
    -- quickPlotStarted <- QuickPlot.runQuickPlot

    -- let test = [treant|{
    --     text: {
    --         name: "img/malory.png"
    --     },
    --     children: [
    --         {
    --             text: {
    --                 name: "img/lana.png"
    --             },
    --             collapsed: true,
    --             children: [
    --                 {
    --                     text: {
    --                         name: "img/figgs.png"
    --                     }
    --                 }
    --             ]
    --         },
    --         {
    --             text: {
    --                 name: "img/sterling.png"
    --             },
    --             childrenDropLevel: 1,
    --             children: [
    --                 {
    --                     text: {
    --                         name: "img/woodhouse.png"
    --                     }
    --                 }
    --             ]
    --         },
    --         {
    --             pseudo: true,
    --             children: [
    --                 {
    --                     text: {
    --                         name: "img/cheryl.png"
    --                     }
    --                 },
    --                 {
    --                     text: {
    --                         name: "img/pam.png"
    --                     } 
                        
    --                 }
    --             ]
    --         }
    --     ]
    -- }|]

    -- plot quickPlotStarted test

    -- plot quickPlotStarted test

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