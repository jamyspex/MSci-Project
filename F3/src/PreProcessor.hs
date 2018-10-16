module PreProcessor (preProcess, removeBlankLines)

where

--    Simple preprocessor used to circumvent some issues with Language-Fortran parser.

import Data.Char
import qualified Data.Map as DMap 

import Warning

preProcess :: Bool -> [String] -> String -> (String, DMap.Map Int [String])
preProcess False macros inputStr = (replaceIfDefByLabel macros) $ removeBlankLinesStr $ andOperatorFix $ orOperatorFix $ containsStatementFix $ caseStatementFix $ inputStr
preProcess True macros inputStr = (replaceIfDefByLabel macros) $ removeBlankLinesStr $ andOperatorFix $ orOperatorFix $ containsStatementFix $ caseStatementFix $ fixedForm $ inputStr

caseInsensitive_strReplace :: [Char] -> [Char] -> [Char] -> [Char]
caseInsensitive_strReplace original replace str     
    | take (length original) (map (toLower) str) == original     
        = replace ++ caseInsensitive_strReplace original replace (drop (length original) str)
    | str == []        = []
    | otherwise     = (take 1 str) ++ caseInsensitive_strReplace original replace (drop 1 str)

removeBlankLinesStr :: String -> String
-- removeBlankLines inputStr = foldl (removeBlankLines_foldl) "" allLines
--         where
--             allLines = lines inputStr

-- removeBlankLines_foldl :: String -> String -> String
-- removeBlankLines_foldl accum item = accum ++ (if all (isSpace) item then "" else item ++ "\n")

-- WV
removeBlankLinesStr inputStr = unlines $ removeBlankLines $ lines inputStr
removeBlankLines  :: [String] -> [String]
removeBlankLines = filter (not . (all isSpace)) 

-- removeBlankLines :: [Char] -> [Char]
-- removeBlankLines [] = []
-- removeBlankLines ('\n':'\n':str) = removeBlankLines ('\n':str)
-- removeBlankLines (char:str) = char:(removeBlankLines str)

caseStatementFix :: String -> String
caseStatementFix input = caseInsensitive_strReplace "\ncase(" "\n case(" (caseInsensitive_strReplace "\ncase " "\n case " input)

containsStatementFix :: String -> String
containsStatementFix input = (caseInsensitive_strReplace "\ncontains" "\n contains " (caseInsensitive_strReplace "\ncontains " "\n contains " input))

orOperatorFix :: String -> String
orOperatorFix input = caseInsensitive_strReplace ".or." " .or. " input

andOperatorFix :: String -> String
andOperatorFix input = caseInsensitive_strReplace ".and." " .and. " input

semiColonFix :: String -> String
semiColonFix input = caseInsensitive_strReplace ";" "\n" input

fixedForm :: String -> String
fixedForm  inputStr = foldl (\accum item -> accum ++ (take 72 item) ++ "\n") "" allLines
        where
            allLines = lines inputStr

replaceIfDefByLabel :: [String] -> String -> (String, DMap.Map Int [String])
replaceIfDefByLabel macros inputStr = 
    let
        src_lines = lines inputStr
        -- state            
        -- (add_to_stash, nest_counter, stash, lines_to_stash, lines_to_output, label_counter)
        init_state = (False,0,DMap.empty,[],[],0)
        (add_to_stash, nest_counter, stash, lines_to_stash, lines_to_output, label_counter) = foldl (stashLine macros) init_state src_lines
    in   
--        (inputStr, stash)    
        (unlines lines_to_output, stash)

-- logic
stashLine :: [String] -> (Bool, Int, DMap.Map Int [String], [String], [String], Int) -> String -> (Bool, Int, DMap.Map Int [String], [String], [String], Int)
stashLine macros (add_to_stash, nest_counter, stash, lines_to_stash, lines_to_output, label_counter) line =
    let        
        (nest_counter', add_to_stash') 
            | nest_counter == 0  = if ( lineHasIfMacro line macros ) then  (1,True) else (0, False)
            | line `startsWith` "#if"  = (nest_counter+1, add_to_stash)
            | line `startsWith` "#endif" = (nest_counter-1,add_to_stash)    
            | otherwise = (nest_counter,add_to_stash)

        (lines_to_stash',lines_to_output') 
            | add_to_stash' = (lines_to_stash++[line],lines_to_output)
            | otherwise = (lines_to_stash,lines_to_output++[line])
    
        (add_to_stash'', stash', lines_to_stash'' , lines_to_output'', label_counter') 
            | nest_counter' == 0 && add_to_stash' =
                let
                    label_counter_ =label_counter+1              
                    stash_ = DMap.insert (7188+label_counter_) lines_to_stash' stash
                    add_to_stash_ = False
                    lines_to_stash_ = []
                    lines_to_output_ = lines_to_output' ++["  "++(show (7188+label_counter_))++" continue"]
                in                  
                    (add_to_stash_, stash_, lines_to_stash_ ,  lines_to_output_, label_counter_)
            | otherwise =
                    (add_to_stash', stash, lines_to_stash' , lines_to_output', label_counter)
    in
        (add_to_stash'', nest_counter', stash', lines_to_stash'', lines_to_output'', label_counter')
    
lineHasIfMacro line macros = foldl (||) False $ map (lineHasIfMacroHelper line) macros 
lineHasIfMacroHelper line macro = foldl (||) False $ map (\ifexpr -> (normLine line) `startsWith` (ifexpr++" "++macro)) ["#ifdef","#ifndef","#if"]

-- remove extraneous spaces
normLine line = unwords $ words line

startsWith line str =  (take (length str) line) == str
