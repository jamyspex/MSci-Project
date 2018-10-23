{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE DuplicateRecordFields        #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE NoMonomorphismRestriction    #-}
{-# LANGUAGE OverloadedLabels             #-}
{-# LANGUAGE PartialTypeSignatures        #-}
{-# LANGUAGE Rank2Types                   #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# OPTIONS_GHC -Wno-missing-signatures   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports  #-}

module Parser
    ( 
        parseTestFile

    ) where

import LanguageFortranTools
import qualified Data.Map as DMap
import Language.Fortran

import Control.Lens
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)
import Data.Function ((&))
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics
import Data.Generics.Internal.VL.Iso
import Data.Generics.Internal.VL.Prism
import Data.Generics.Internal.Profunctor.Lens
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism
  
parseTestFile :: IO ()
parseTestFile = do
    parseOutput <- parseFile [] [] False "Shallow-Water-2D/init.f95"
    let
        (parsedProgram, stash, moduleVarTable) = parseOutput
        stashValues = snd stash
        astObj = fst parsedProgram
    putStrLn $ "AST: " ++ (show astObj)
    playAboutWithLens astObj 
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

playAboutWithLens :: Program Anno -> IO (Program Anno)
playAboutWithLens ast = do
    putStrLn $ show $ toListOf (types @(VarName _)) ast
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