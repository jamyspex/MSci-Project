module Parser
    ( 
        parseTestFile

    ) where

import LanguageFortranTools (parseFile)
import qualified Data.Map as DMap
  
parseTestFile :: IO ()
parseTestFile = do
    parseOutput <- parseFile [] [] False "Shallow-Water-2D/init.f95"
    let
        (parsedProgram, stash, moduleVarTable) = parseOutput
        stashValues = snd stash
    putStrLn $ "AST: " ++ (show $ fst parsedProgram)
    putStrLn $ "Program lines:"
    mapM_ putStrLn $ map (\line -> "\t" ++ line) $ snd parsedProgram

    putStrLn $ "Code stash name: " ++ (fst stash)

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