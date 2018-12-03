module CodeEmitter                         (emit)

where

--    Code in this file handles the final emission of code. The function 'emit' is called against an AST that has been transformed and has had kernels fused.
--    Trys to make as much use as it can of code that is the same as it was in the original source file. Otherwise, it uses mostly simple functions to generate
--    code segments. 

--    Most of the heavily lifting is now performed by FortranSynthesiser.hs and FortranGenerator.hs, using functions found in CodeEmitterUtils.hs

import Control.Monad
import Data.Generics                     (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.IO
import System.Process
import Data.Maybe
import qualified Data.Map as DMap 

import CodeEmitterUtils
import LanguageFortranTools
import SubroutineTable                     (
        SubroutineTable(..), SubRec(..),
        ArgumentTranslation, SubroutineArgumentTranslationMap, emptyArgumentTranslation, getSubroutineArgumentTranslation, translateArguments,
                                        extractSubroutines, extractProgUnitName)
import FortranSynthesiser
import Platform
-- fileCoordinated_parallelisedList fileCoordinated_bufferOptimisedPrograms argTranslations (newMainAst, mainFilename) [] []
emit :: String -> [String] -> [String] -> Platform -> Bool -> [(Program Anno, String)] -> 
    [(Program Anno, String)] -> SubroutineArgumentTranslationMap -> (Program Anno, String) -> [VarName Anno] -> [VarName Anno] -> SubroutineTable -> 
    ((String, CodeStash),[(String, CodeStash)]) -> (ModuleVarsTable, [ModuleVarsTable])   -> IO [()]
emit specified cppDFlags cppXFlags plat fixedForm programs_verboseArgs 
    programs_optimisedBuffers argTranslations (mainAst, mainFilename) initWrites tearDownReads orig_asts 
    (mainStash, stashes) (mainModVarTable,modVarTables) = do

--                kernels_code <- mapM (emitKernelsM plat cppDFlags fixedForm orig_asts) programs_verboseArgs 
                let kernels_code = map (emitKernels plat orig_asts) programs_verboseArgs 
                let allKernels = foldl (++) [] kernels_code
                let kernelNames = map snd allKernels

                let originalFilenames = map (\x -> getModuleName (snd x)) programs_verboseArgs
                let superkernelName = synthesiseSuperKernelName originalFilenames
                let moduleName = "module_" ++ superkernelName
                let moduleFilename = specified ++ "/" ++ moduleName ++ ".f95"
                let newMainFilename = specified ++ "/" ++ (hostModuleName (getModuleName mainFilename)) ++ ".f95"
                let initModuleFilename = specified ++ "/" ++ (initModuleName moduleName) ++ ".f95"

                let (superKernel_module, allKernelArgsMap) = synthesiseSuperKernelModule moduleName superkernelName programs_verboseArgs allKernels
                let initModule = synthesiseInitModule moduleName superkernelName programs_verboseArgs allKernelArgsMap allKernels orig_asts
                -- WV: TODO: use orig_asts
                host_code <- mapM (produceCode_prog allKernelArgsMap argTranslations cppDFlags cppXFlags plat fixedForm moduleName superkernelName) (zip programs_optimisedBuffers modVarTables)
                main_code <- produceCode_prog allKernelArgsMap argTranslations cppDFlags cppXFlags plat fixedForm moduleName superkernelName ( (mainAst , mainFilename), mainModVarTable) 
                --WV: now I need to parse this generated code, i.e. unlines it and inspect the lines an substitute the labeled lines for their contents from the stash
                let
                    main_code' = restoreIfDefRegions main_code mainStash
                -- WV: TODO: I should do the same for the _host code    
                let host_programs = zip host_code (map (\x -> specified ++ "/" ++ x ++ "_host.f95") originalFilenames)

                writeFile moduleFilename (if fixedForm then fixedFormFormat superKernel_module else superKernel_module)
                writeFile newMainFilename (if fixedForm then fixedFormFormat main_code' else main_code')
                writeFile initModuleFilename (if fixedForm then fixedFormFormat initModule else initModule)

                mapM (\(code, filename) -> writeFile filename (if fixedForm then fixedFormFormat code else code)) host_programs

restoreIfDefRegions :: String -> (String, DMap.Map Int [String]) -> String                
restoreIfDefRegions code (fname,stash) =  unlines $ map (restoreIfDefRegion stash) (lines code)

restoreIfDefRegion ::  DMap.Map Int [String] -> String -> String
restoreIfDefRegion stash line 
    | length chunks == 2 && chunks !! 1 == "continue" = 
        let
            maybe_label_str = head $ words line
            maybe_label = read maybe_label_str :: Int
        in
            case DMap.lookup maybe_label stash of
                Just original_lines -> unlines original_lines
                Nothing -> line
    | otherwise = line
  where
   chunks = words line  

fixedFormFormat :: String -> String
fixedFormFormat inputStr = foldl (\accum item -> accum ++ "\n" ++ (fixedFormFormat_line item)) "" allLines
        where
            allLines = lines inputStr

fixedFormFormat_line :: String -> String
fixedFormFormat_line "" = ""
fixedFormFormat_line inputLine     |    fixedFormFormat_isComment inputLine = inputLine
                                |    otherwise =  addedLeadingWhiteSpace ++ thisLine
                                ++ if nextLineExists then lineCont ++ "\n" ++ (if nextLineNotContinue then nextLine else "") else ""
        where
            thisLine = (take lineLength_contAndWhiteSpace (inputLine))
            nextLine = (fixedFormFormat_line (drop lineLength_contAndWhiteSpace (inputLine)))

            whiteSpaceCount = fixedFormFormat_leadingWhiteSpaceCount inputLine
            addedLeadingWhiteSpace = foldl (\accum item -> accum ++ " ") "" [whiteSpaceCount + 1 .. desiredLeadingWhiteSpace]
            lineCont = " &"
            desiredLeadingWhiteSpace = 6
            desiredLineLength = 72
            lineLength_contAndWhiteSpace = (desiredLineLength - (length lineCont)) - (max 0 (desiredLeadingWhiteSpace - whiteSpaceCount))

            nextLineNotContinue = (not $ fixedFormFormat_containsOnlyContinuation nextLine)
            nextLineExists = nextLine /= ""

fixedFormFormat_containsOnlyContinuation :: String -> Bool
fixedFormFormat_containsOnlyContinuation (char:str)     |    char == '&' = fixedFormFormat_containsOnlyContinuation str
                                        |    isSpace char = fixedFormFormat_containsOnlyContinuation str
                                        |    otherwise = False
fixedFormFormat_containsOnlyContinuation [] = True

fixedFormFormat_isComment :: String -> Bool
fixedFormFormat_isComment (char:str)     |    char == '!' = True
                                        |    isSpace char = fixedFormFormat_containsOnlyContinuation str
                                        |    otherwise = False
fixedFormFormat_isComment [] = True

fixedFormFormat_leadingWhiteSpaceCount :: String -> Int
fixedFormFormat_leadingWhiteSpaceCount (char:str)     |    isSpace char = 1 + fixedFormFormat_leadingWhiteSpaceCount str
                                                    |    otherwise = 0
-- getSubRec :: SubroutineTable -> String -> SubRec
-- getSubRec orig_asts filename = DMap.findWithDefault (error ("No rec for "++filename++": "++(show $ DMap.keys orig_asts)) ) filename orig_asts

getOrigAST :: SubroutineTable -> String -> ProgUnit Anno
getOrigAST orig_asts filename = let
        orig_asts_list = DMap.toList orig_asts
        orig_ast_list = filter (\(subname,subrec) -> subSrcFile subrec == filename) orig_asts_list
    in
        (\(subname,subrec) -> subAst subrec) $ head orig_ast_list

getSubRec :: SubroutineTable -> String -> SubRec
getSubRec  orig_asts filename = let
        orig_asts_list = DMap.toList orig_asts
        orig_ast_list = filter (\(subname,subrec) -> subSrcFile subrec == filename) orig_asts_list
    in
       snd $ head orig_ast_list




emitKernelsM :: Platform -> SubroutineTable -> (Program Anno, String) -> IO [(String, String)]
emitKernelsM plat orig_asts (ast, filename) = do
                let originalLines = subSrcLines (getSubRec  orig_asts filename)
                -- WV: seems to me this is simply "unlines"; and it is unused too
                -- let originalListing = case originalLines of
                --                        []    -> ""
                --                        _ -> foldl (\accum item -> accum ++ "\n" ++ item) (head originalLines) (tail originalLines)
                let orig_ast = getOrigAST orig_asts filename 
                let kernels_code = everything (++) (mkQ [] (synthesiseKernels plat originalLines orig_ast (ast, filename))) ast
--                let kernels_renamed = map (\(code, kernelname) -> (code ,kernelname)) kernels_code
--                return kernels_renamed
                return kernels_code



emitKernels :: Platform -> SubroutineTable -> (Program Anno, String) -> [(String, String)]
emitKernels plat orig_asts (ast, filename) = let
                originalLines = subSrcLines (getSubRec  orig_asts filename)
                orig_ast = getOrigAST orig_asts filename 
                kernels_code = everything (++) (mkQ [] (synthesiseKernels plat originalLines orig_ast (ast, filename))) ast
            in
                kernels_code                
