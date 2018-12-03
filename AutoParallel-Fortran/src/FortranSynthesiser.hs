module FortranSynthesiser 

where

-- The intention of this module is to house all of the functions that take some AST node and produce a final String
-- representation that will be used as correct Fortran 95 code. The module also includes functions to generate names
-- for modules and suproutines. Most functions are fairly straightforward but some look rather packed. In the cases 
-- of larger functions, I've tried to make it very clear which variables are used (and in which order) for the final
-- output from the function. 

-- Most, if not all, of the functions have a 'tabs' argument. This is purely for presentation as it allows the current
-- indentation in the output source to be tracked. 

-- In some cases, functions for very simple AST nodes, or AST nodes that are unlikely to differ very much simple output 
-- a mostly hardcoded string. In more complicated cases, the whole string representation of a node is formed through 
-- recursive calls to more modular functions.

-- The most interesting function is 'synthesiseOpenCLReduce' as it requires lots of transformation of original code, including
-- the insertion of partial reductions for the sake of effective GPU use. The function is explained in more detail in a header
-- comment. The 'synthesiseOpenCLReduce' function is one of many that produce code from AST nodes that would not exist in 
-- normal Fortran. Another example is 'synthesiseKernelCall'
--
{-
synthesiseKernels    
    synthesiseOpenCLMap
-}    
import Warning 
import System.Directory
import MiniPP (miniPPF, miniPPD, showSubName)

import Data.Generics                     (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran
import Language.Fortran.Parser (context_parse)
import Data.Char
import Data.List
import Data.Set (toList, fromList)
import qualified Data.Map as DMap 

import FortranGenerator -- WV: TODO: add the import list here
import CodeEmitterUtils
import LanguageFortranTools
import Language.Fortran.Parser (statement_parse)
import SubroutineTable                     (SubroutineTable(..),SubRec(..),ArgumentTranslation, SubroutineArgumentTranslationMap, emptyArgumentTranslation, getSubroutineArgumentTranslation, translateArguments,
                                        extractSubroutines, extractProgUnitName)
import Platform

--    This function produces a list of strings where each element is a line of the original source. This
--    list is used heavily in this module.
readOriginalFileLines :: [String] -> [String] -> Bool -> String -> IO ([String])
readOriginalFileLines cppDFlags cppXFlags fixedForm filename = do
                content <- runCpp cppDFlags cppXFlags fixedForm filename
                let contentLines = lines content
                return contentLines
                -- so here I have to go through these lines, and find 'use' declarations, and substitute these 
                --let 
--                expandedContentLines <- foldlM (++) [] $ 
--                inlineDeclsFromUsedModules contentLines 

defaultFilename :: [String] -> String
defaultFilename (x:[]) = "par_" ++ x
defaultFilename (x:xs) = x ++ "/" ++ defaultFilename xs

generateStateName :: String -> String
generateStateName kernelName = "ST_" ++ (map (toUpper) kernelName)
-- WV: The numbers at the end of the  kernel names are the line numbers of the start of the subroutine in its source file
-- I would prefer to have simple contiguous numbering.
-- That would require keeping a running counter and adding it as an argument.
generateKernelName :: String -> SrcSpan -> String
generateKernelName identifier src = (getModuleName filename) ++ "_" ++ identifier
                                            ++ "_" ++ show line
            where
                ((SrcLoc filename line _), _) = src

generateKernelNameOLD :: String -> SrcSpan -> [VarName Anno] -> String
generateKernelNameOLD identifier src varnames = (getModuleName filename) ++ "_" ++ identifier
                                            ++ "_" ++ show line
            where
                ((SrcLoc filename line _), _) = src

generateOriginalFileName :: [String] -> String
generateOriginalFileName (x:[]) = "original_" ++ x
generateOriginalFileName (x:xs) = x ++ "/" ++ generateOriginalFileName xs

synthesiseSuperKernelName :: [String] -> String
synthesiseSuperKernelName originalFilenames = base ++ suffix
        where
            maxLen = 30
            base = take maxLen (map (toLower) ((foldl1 (\accum item -> accum ++ "_" ++ item) originalFilenames)))
            suffix = if (length base == maxLen) 
                then 
                    (case (last base) of
                                    '_' -> "etc_superkernel"
                                    _ -> "_etc_superkernel")
                else 
                    (case (last base) of
                                        '_' -> "superkernel"
                                        _ -> "_superkernel")

produceCode_prog :: KernelArgsIndexMap -> SubroutineArgumentTranslationMap -> [String] -> [String] -> Platform -> Bool -> String -> String -> ((Program Anno, String),ModuleVarsTable) -> IO(String)
produceCode_prog allKernelArgsMap argTranslation cppDFlags cppXFlags plat fixedForm kernelModuleName superKernelName ((prog, filename),modVarTable) = do
                    originalLines <- readOriginalFileLines cppDFlags cppXFlags fixedForm filename
                    let result = foldl (\accum item -> accum ++ produceCode_progUnit allKernelArgsMap emptyArgumentTranslation ((prog, filename),modVarTable) kernelModuleName superKernelName originalLines item) "" prog
                    return result -- (warning result result)

produceCode_progUnit :: KernelArgsIndexMap -> SubroutineArgumentTranslationMap -> ((Program Anno, String),ModuleVarsTable) -> String -> String -> [String] -> ProgUnit Anno -> String
-- Code for Main
produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable) kernelModuleName superKernelName originalLines (Main _ src subname _ block progUnits) =    
    if (length originalLines == 0) 
        then
            error "produceCode_progUnit: length originalLines == 0\n"
        else 
                  nonGeneratedHeaderCode
           ++     nonGeneratedBlockCode_indent ++ "use oclWrapper\n" 
           ++     nonGeneratedBlockCode_indent ++ "use " ++ (initModuleName kernelModuleName) ++ "\n" 
           ++     maybeRestoredUseModuleDecls
           ++     maybeImplicitNone
           -- for some reason, the declarations have gone missing
           ++ nonGeneratedBlockCode_indent 
           ++ "\n! Original declarations\n"
           ++ origDecls ++ "\n\n"           
           ++ everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap emptyArgumentTranslation progWithFilename modVarTable nonGeneratedBlockCode_indent originalLines "!\n" "!NOTHING!\n" maybeOclInitCall)) block
--           ++ "! containedProgUnitCode\n"
           ++     containedProgUnitCode -- WV: apparently this is empty
--           ++ "! Footer (produceCode_progUnit a)\n"
           ++     nonGeneratedFooterCode
           ++ nonGeneratedBlockCode_indent++"end program "++(showSubName subname)
--           ++ "! END of code generated by produceCode_progUnit "
  where
            (Block blockAnno useBlock implicit blockSrc blockDecl blockFortran) = block
            (origDecls,maybeRestoredUseModuleDecls) = restoreUsedModuleDecls blockDecl modVarTable
--            origDecls = unlines $ map miniPPD $ traverseDSeq blockDecl []
            ((SrcLoc _ block_ls _), _) = blockSrc
            (nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src blockSrc
--            ((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
--            nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]
            (nonGeneratedHeaderCode, maybeImplicitNone) = checkForImplicitNone $ extractOriginalCode_Offset1 originalLines nonGeneratedHeaderSrc
--            maybeImplicitNone = warning maybeImplicitNone' maybeImplicitNone'
--            nonGeneratedHeaderCode_lines = lines nonGeneratedHeaderCode
--            nonGeneratedHeaderCode' = unlines (warning nonGeneratedHeaderCode_lines (show nonGeneratedHeaderCode_lines))
--            ((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
--            nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls..nonGeneratedFooter_le-1]
            nonGeneratedFooterCode = extractOriginalCode_Offset1 originalLines nonGeneratedFooterSrc
            containedProgUnitCode = foldl (\accum item -> accum ++ (produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable)  kernelModuleName superKernelName originalLines item)) "" progUnits
            nonGeneratedBlockCode_indent 
                | block_ls < 0 || length originalLines < block_ls = "      " -- 
--                | otherwise = extractIndent (originalLines!!(block_ls-1))
                | otherwise = let
                        tab1 = extractIndent (originalLines!!(block_ls-1))
                        tab2 = extractIndent (originalLines!!block_ls)
                    in
                        if length tab1 > length tab2 then tab1 else tab2

            maybeOclInitCall = Just (Call nullAnno nullSrcSpan (generateVar (VarName nullAnno ((initModuleName superKernelName)))) (ArgList nullAnno (NullExpr nullAnno nullSrcSpan))) -- this is the OpenCL init call node
-- Code for Modules
produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable)  kernelModuleName superKernelName originalLines (Module _ src _ _ _ _ progUnits) =
    if (length originalLines == 0) 
        then "" 
        else 
            nonGeneratedHeaderCode ++
            containedProgUnitCode ++
           -- "! Footer (produceCode_progUnit b)\n" ++
            nonGeneratedFooterCode
  where
            firstProgUnitSrc = srcSpan (head progUnits)
            lastProgUnitSrc = srcSpan (last progUnits)
            
            (nonGeneratedHeaderSrc, _) = getSrcSpanNonIntersection src firstProgUnitSrc
            (_, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src lastProgUnitSrc
--            ((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
--            nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]
            nonGeneratedHeaderCode = extractOriginalCode_Offset1 originalLines nonGeneratedHeaderSrc
--            ((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
--            nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls..nonGeneratedFooter_le-1]
            nonGeneratedFooterCode = extractOriginalCode_Offset1 originalLines nonGeneratedFooterSrc
            containedProgUnitCode = foldl (\accum item -> accum ++ (produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable)  kernelModuleName superKernelName originalLines item)) "" progUnits
-- Code for Subroutines
produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable)  kernelModuleName superKernelName originalLines (Sub _ src _ (SubName _ subroutineName) _ block) =  
            if (length originalLines == 0) 
                then
                                                everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap argTranslation progWithFilename modVarTable nonGeneratedBlockCode_indent originalLines "" "" Nothing)) block
                else
                                                   nonGeneratedHeaderCode 
                                                ++ nonGeneratedBlockCode_indent ++ "use " ++ (initModuleName kernelModuleName) ++ "\n" 
                                                ++ nonGeneratedBlockCode_indent ++ "use oclWrapper\n" 
                                                ++ "!" ++ maybeImplicitNone
--                                                ++ nonGeneratedBlockCode_indent ++ "real (kind=4) :: exectime\n"
--                                                ++ global_reductionArraysDeclStr
                                                ++ everything (++) (mkQ "" (produceCodeBlock allKernelArgsMap argTranslation progWithFilename modVarTable nonGeneratedBlockCode_indent originalLines maybeImplicitNone extraDecls Nothing)) block
                                                ++ "! Footer (produceCode_progUnit c)\n"
                                                ++ nonGeneratedFooterCode    
        where
            extraDecls = nonGeneratedBlockCode_indent ++ "real (kind=4) :: exectime\n" ++ global_reductionArraysDeclStr ++ "\n"
            firstFortranSrc = head (everything (++) (mkQ [] (getFirstFortranSrc)) block)
            blockSrc = srcSpan block
            (nonGeneratedHeaderSrc, nonGeneratedFooterSrc) = getSrcSpanNonIntersection src blockSrc

--            ((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrc
--            nonGeneratedHeaderCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedHeader_ls..nonGeneratedHeader_le-1]
            -- nonGeneratedHeaderCode = extractOriginalCode_Offset1 originalLines nonGeneratedHeaderSrc
            (nonGeneratedHeaderCode, maybeImplicitNone) = checkForImplicitNone $ extractOriginalCode_Offset1 originalLines nonGeneratedHeaderSrc
--            ((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrc
--            nonGeneratedFooterCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [nonGeneratedFooter_ls..nonGeneratedFooter_le-1]
            nonGeneratedFooterCode = extractOriginalCode_Offset1 originalLines nonGeneratedFooterSrc

            (src_b@(SrcLoc _ block_ls _), (SrcLoc _ _ _)) = blockSrc
            (src_e@(SrcLoc _ fortran_ls _), (SrcLoc _ _ _)) = firstFortranSrc
            srcspan = (src_b,src_e)
--            nonGeneratedBlockCode = foldl (\accum item -> accum ++ (originalLines!!(item-1)) ++ "\n") "" [block_ls..fortran_ls-1]
            nonGeneratedBlockCode = extractOriginalCode_Offset1 originalLines srcspan
            nonGeneratedBlockCode_indent 
                | fortran_ls < 0 || length originalLines < fortran_ls = "      " -- 
--                | otherwise = extractIndent (originalLines!!(fortran_ls-1))
                | otherwise = let
                        tab1 = extractIndent (originalLines!!(fortran_ls-1))
                        tab2 = extractIndent (originalLines!!fortran_ls)
                    in
                        if length tab1 > length tab2 then tab1 else tab2

            argTranslation = getSubroutineArgumentTranslation argTranslationSubroutines subroutineName

            reduceKernels = extractOpenCLReduces block
            reductionVarNames = foldl (\accum item -> listConcatUnique accum (extractReductionVarNames item)) [] reduceKernels

            global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) (fst progWithFilename)) reductionVarNames
            global_reductionArraysDeclsWithoutIntent = map (removeIntentFromDecl) global_reductionArraysDecls
            global_reductionArraysDeclStr = synthesiseDecls nonGeneratedBlockCode_indent global_reductionArraysDeclsWithoutIntent

produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable)  kernelModuleName superKernelName originalLines progunit = 
    foldl (++) "" (gmapQ (mkQ "" (produceCode_progUnit allKernelArgsMap argTranslationSubroutines (progWithFilename,modVarTable)  kernelModuleName superKernelName originalLines)) progunit)


checkForImplicitNone :: String -> (String, String)
checkForImplicitNone lines_str = 
    let
        code_lines = lines lines_str
        (other_code_lines, maybeImplicitNone) = foldl isImplicitNone ([],"") code_lines
    in
        ((unlines other_code_lines)++"\n",maybeImplicitNone++"\n")

isImplicitNone :: ([String],String) -> String -> ([String],String)
isImplicitNone (other_code_lines, _) line = 
            let
                chunks = words line
            in 
                if "implicit" `elem` chunks && "none" `elem` chunks
                    then
                        (other_code_lines, line)
                    else
                        (other_code_lines++[line],"")

restoreUsedModuleDecls' :: [Decl Anno] -> ModuleVarsTable -> (String,String)
restoreUsedModuleDecls' declList modVarTable = 
    let
        (remainingDeclList,restoredUseModuleDeclList) =foldl (\acc decl ->
                let
                    (remainingDeclList,restoredUseModuleDeclList) = acc
                    Decl _ _ ((var_name_expr,_,_):[]) _ = decl
                    var_name = (\(VarName _ v) -> v) $ head $ extractVarNames var_name_expr
                    mod_name = case DMap.lookup var_name modVarTable of
                        Just m -> m
                        Nothing -> ""
                in
                    if mod_name == "" 
                        then (remainingDeclList ++ [decl], restoredUseModuleDeclList)
                        else (remainingDeclList, restoredUseModuleDeclList++[mod_name])
              ) ([],[]) declList
        origDecls = unlines $ map (\decl -> (miniPPD decl)++" !!" ) remainingDeclList
        maybeRestoredUseModuleDecls = unlines $ nub $ map (\mod_name -> "      use "++mod_name) restoredUseModuleDeclList
    in
        (origDecls,maybeRestoredUseModuleDecls) 

restoreUsedModuleDecls :: Decl Anno -> ModuleVarsTable -> (String,String)
restoreUsedModuleDecls blockDecl modVarTable = 
    let
        declList = traverseDSeq blockDecl []
    in
        restoreUsedModuleDecls' declList modVarTable                                     

traverseDSeq fseq acc = case fseq of
    NullDecl _ _ -> acc
    (DSeq _ f1 f2) -> acc++(traverseDSeq f1 [])++(traverseDSeq f2 [])
    f -> acc ++ [f]    
{- 
produceCodeBlock allKernelArgsMap emptyArgumentTranslation progWithFilename nonGeneratedBlockCode_indent originalLines maybeOclInitCall block   
-}

produceCodeBlock :: KernelArgsIndexMap -> ArgumentTranslation -> (Program Anno, String) -> ModuleVarsTable -> String -> [String] -> String -> String -> Maybe (Fortran Anno) -> Block Anno -> String
produceCodeBlock allKernelArgsMap argTranslation prog modVarTable tabs originalLines maybeImplicitNone extraDecls maybePrefix (Block anno useBlock imp src decl fort)     
                                                                        |    length originalLines == 0 =  produceCode_fortran prog tabs originalLines fort
                                                                        |    nonGeneratedHeader_ls < 1 = error "produceCodeBlock: nonGeneratedHeader_ls < 1"
                                                                        |    otherwise =                                                                                
                                                                                  maybeRestoredUseModuleDecls ++ "\n"
                                                                            ++    maybeImplicitNone ++ "\n"
                                                                            ++    "! otherStatements\n"
                                                                            ++    (unlines otherStatements) ++"\n"
                                                                            ++    "! remainingDecls\n"
                                                                            ++    remainingDecls ++"\n"
                                                                            ++    tabs ++ "! Extra declarations\n"
                                                                            ++    extraDecls
                                                                            ++    tabs ++ "! Buffer declarations\n"
                                                                            ++    bufferDeclarationStatements ++ "\n"
                                                                            ++    statePtrDeclStr ++ "\n"
                                                                            ++    tabs ++ "! Size declarations\n"
                                                                            ++    sizeDeclarations ++ "\n"
                                                                            ++    (if openCLReducePresent then nonGeneratedBlockCode_indent ++ reductionIteratorDecl ++ "\n" else "")
                                                                            ++    prefixString -- a misnomer, for Main this is the call to the OpenCL init subroutine
                                                                            ++    tabs ++ "\n! Size assignments\n"
                                                                            ++    shapeStatements ++ "\n"
                                                                            ++    tabs ++ "! Buffer loads\n"
                                                                            ++    loadBufferStatements ++ "\n"
                                                                            ++    tabs ++ "! Original code with buffer writes and reads\n"
                                                                            ++     produceCode_fortran prog tabs originalLines fort
                                                                            -- ++    "! Start of footer produceCodeBlock\n"
                                                                            ++    nonGeneratedFooterCode
                                                                            -- ++    "! End of block\n"
        where
            -- fort is the code with extra OpenCL buffer reads/writes
            block = Block anno useBlock imp src decl fort
            fortranSrc = srcSpan fort
            ((SrcLoc _ fortran_ls _), _) = fortranSrc -- warning fortranSrc (show (src,fortranSrc))
            -- src is the SrcSpan of the *original* ast
            (nonGeneratedHeaderSrcSpan, nonGeneratedFooterSrcSpan) = getSrcSpanNonIntersection src fortranSrc

            ((SrcLoc _ nonGeneratedHeader_ls _), (SrcLoc _ nonGeneratedHeader_le _)) = nonGeneratedHeaderSrcSpan
--            BOOM! Bring in line with code on lines 106-136      
            nonGeneratedHeaderCode' = extractOriginalCode_Offset1 originalLines nonGeneratedHeaderSrcSpan
            -- WV NOTE to do this correctly we need to label each line
            nonGeneratedHeaderCodeStatements = 
                let
                    code_lines = lines nonGeneratedHeaderCode'
--                    labeled_code_lines = zip code_lines [1 .. length code_lines]
                    (decls,rest) = partition findDeclLine code_lines 
                    parsedDecls = map context_parse  $ decls
                in
                    (parsedDecls, rest)
            (parsedDecls,otherStatements) = nonGeneratedHeaderCodeStatements
            (remainingDecls,maybeRestoredUseModuleDecls) = restoreUsedModuleDecls' parsedDecls modVarTable
{-
            nonGeneratedHeaderCode = 
                    maybeRestoredUseModuleDecls ++ "\n"
                ++ "! otherStatements\n"
                ++ (unlines otherStatements) ++"\n"
                ++ "! remainingDecls\n"
                ++ remainingDecls ++"\n"
                -}
            ((SrcLoc _ nonGeneratedFooter_ls _), (SrcLoc _ nonGeneratedFooter_le _)) = nonGeneratedFooterSrcSpan
            nonGeneratedFooterCode
                | nonGeneratedFooter_ls < 1 = "" -- "! nonGeneratedFooterSrc: "++(show nonGeneratedFooterSrc)
                | otherwise = extractOriginalCode_Offset (1,-1) originalLines nonGeneratedFooterSrcSpan

            nonGeneratedBlockCode_indent
                | fortran_ls < 0 || length originalLines < fortran_ls = "      " -- 
                | otherwise = let
                        tab1 = extractIndent (originalLines!!(fortran_ls-1))
                        tab2 = extractIndent (originalLines!!fortran_ls)
                    in
                        if length tab1 > length tab2 then tab1 else tab2
            (sizeDeclarations, shapeStatements) = synthesiseSizeStatements_kernel nonGeneratedBlockCode_indent (fst prog)
            (bufferDeclarationStatements, loadBufferStatements) = synthesiseBufferLoads_kernel nonGeneratedBlockCode_indent allKernelArgsMap argTranslation block
            statePtrDeclStr = synthesiseDecl tabs statePtrDecl

            openCLReducePresent = (extractOpenCLReduces block) /= []

            prefixString = case maybePrefix of
                                Just fortPrefix -> produceCode_fortran prog tabs originalLines fortPrefix
                                Nothing -> ""

--    This function is called very often. It is the default when producing the body of each of the kernels and calls other functions
--    based on the node in the AST that it is called against. Each of the 'synthesise...' functions check whether the node in question
--    is a 'generated' node to determine whether or not code from the original file can be used.
produceCode_fortran :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
produceCode_fortran prog tabs originalLines codeSeg = case codeSeg of
                        If _ _ _ _ _ _ -> synthesiseIf prog tabs originalLines codeSeg
                        Assg _ _ _ _ -> synthesiseAssg prog tabs originalLines codeSeg
                        For _ _ _ _ _ _ _ -> synthesiseFor prog tabs originalLines codeSeg
                        NullStmt _ _ -> ""
                        OpenCLBufferRead _ _ _ -> synthesiseOpenCLBufferRead prog tabs originalLines codeSeg
                        OpenCLBufferWrite _ _ _ -> synthesiseOpenCLBufferWrite prog tabs originalLines codeSeg
                        OpenCLMap _ _ _ _ _ _ _ -> (synthesiseKernelCall prog tabs codeSeg) ++ (commentSeparator "END")  -- WV20170426
                        OpenCLReduce _ _ _ _ _ _ rv f -> (synthesiseKernelCall prog tabs codeSeg) -- WV20170426
                                                                ++ (mkQ "" (produceCode_fortran prog tabs originalLines) hostReductionLoop) ++ "\n" ++ (commentSeparator "END")
                                where 
                                    reductionVarNames = map (\(varname, expr) -> varname) rv
                                    r_iter = generateReductionIterator
                                    hostReduction = generateFinalHostReduction reductionVarNames r_iter f
                                    hostReductionLoop = generateLoop r_iter (generateIntConstant 1) nunitsVar hostReduction                                    
                        Call _ _ _ _ -> synthesiseCall prog tabs originalLines codeSeg
                        FSeq _ _ fortran1 fortran2 -> (mkQ "" (produceCode_fortran prog tabs originalLines) fortran1) ++ (mkQ "" (produceCode_fortran prog tabs originalLines) fortran2)
                        _ ->     case anyChildGenerated codeSeg || isGenerated codeSeg of
                                    True -> foldl (++) tabs (gmapQ (mkQ "" (produceCode_fortran prog "" originalLines)) codeSeg)
                                    False ->  extractOriginalCode originalLines (srcSpan codeSeg)

synthesiseInitModule :: String -> String ->  [(Program Anno, String)] -> KernelArgsIndexMap -> [(String, String)] -> SubroutineTable -> String
synthesiseInitModule moduleName superKernelName programs allKernelArgsMap kernels orig_subrecs =     
                                                  initModuleHeader 
                                            ++    stateDefinitions 
                                            ++    bufferIndexNameDecls
                                            ++    "\ncontains\n\n"
                                            ++    oneTab ++ initSubroutineHeader
                                            ++    twoTab ++ "use oclWrapper\n"
                                            ++    usesString                                            
                                            ++    kernelInitialisationStrs
                                            ++    "! parameters\n" -- these are *all* parameters used in any kernel
                                            ++    paramDeclsStr
                                            ++    "! declarations\n" 
                                            ++    declarationsStr
                                            ++    "! buffer declarations\n" 
                                            ++    bufferDeclarationStatements
                                            ++    sizeDeclarations_str ++ "\n"

                                            ++    produceCode_fortran ([], "") twoTab [] oclInitCall
                                            ++    "\n" ++ shapeStatements_str
                                            ++    "\n" ++ makeBufferStatementStr
                                            ++    "\n" ++ oclSetArgStatements_str
                                            ++    "\n" ++ storeBufferStatements_str ++ "\n"
                                             
                                        ++    "\n" ++ oneTab ++ initSubroutineFooter
                                        ++     initModuleFooter
                where
                    bufferIndexNameDecls = synthesiseBufferIndexNames twoTab allKernelArgsMap emptyArgumentTranslation kernelArgs 
                    kernelNames = map (snd) kernels
                    initModuleHeader = "module " ++ (initModuleName moduleName) ++ "\n\n"
                    initModuleFooter = "\nend module " ++ (initModuleName moduleName)

                    initSubroutineHeader = "subroutine " ++ (initModuleName superKernelName) ++ "()\n\n"
                    initSubroutineFooter = "end subroutine " ++ (initModuleName superKernelName)

                    stateNames = map (generateStateName) kernelNames
                    stateDefinitions = synthesiseStateDefinitions (zip kernelNames stateNames) 0

                    kernelInitialisationStrs =     twoTab ++ "character(len=*), parameter :: srcstr = \"" ++ moduleName ++".cl\"\n"
                                             ++    twoTab ++ "character(len=*), parameter :: kstr   = \"" ++ superKernelName ++ "\"\n"

                    oclInitCall = Call nullAnno nullSrcSpan (generateVar (VarName nullAnno "oclInit")) (generateArgList [VarName nullAnno "kstr", VarName nullAnno "srcstr"])

                    kernelArgs = DMap.keys allKernelArgsMap

                    programAsts = map (fst) programs
                    kernelAstLists = map extractKernels programAsts
                    kernelAsts = foldl (++) [] (map (\(k_asts, p_ast) -> map (\a -> (a, p_ast)) k_asts) (zip kernelAstLists programAsts))

                    extractedUses = foldl (\accum item -> listConcatUnique accum (everything (++) (mkQ [] getUses) item)) [] programAsts
                    usesString = foldl (\accum item -> accum ++ synthesisUses twoTab item) "" extractedUses 

                    kernelDeclarations = map (\(kernel_ast, prog_ast) -> generateKernelDeclarations prog_ast kernel_ast) kernelAsts
                    kernelLoopVars = nub $ foldl (++) [] $ map (\t -> getLoopVars (fst t)) kernelAsts
                    (readDecls, writtenDecls, readWriteDecls) =  foldl (\(accum_r, accum_w, accum_g) (r, w, g) -> (accum_r ++ r, accum_w ++ w, accum_g ++ g)) ([],[],[]) kernelDeclarations
                    -- WV: The parameter declarations are missing. The simplest, crudest way to fix this is by taking *all* parameter decls from all subs and nub them.
                    -- WV: This blatantly ignores conflicts in parameter names
--                    paramDeclsStr = unlines $ map miniPPD $ filter isParamDecl $ nub $ map context_parse $ foldl1 (++) $ map (\(k,v) -> subSrcLines v) (DMap.toList orig_subrecs)
                    paramDeclsStr = unlines $ nub $ map (\decl ->twoTab++(miniPPD decl)) $ concatMap (extractParamDecls . subAst) (DMap.elems orig_subrecs)

                    -- WV this is not correct, there are way too many because none of the local scalar variables needs to be here!
                    declarations_maybe_with_loop_vars = foldl collectDecls [] (readDecls ++ writtenDecls ++ readWriteDecls ++ [statePtrDecl])
                    declarations = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem` kernelLoopVars  ) ) declarations_maybe_with_loop_vars
                    declarations_noIntent = map removeIntentFromDecl declarations
                    declarationsStr = foldl (\accum item -> accum ++ synthesiseDecl twoTab item) "" declarations_noIntent

                    makeBuffers = map (synthesiseBufferMake twoTab) declarations
                    makeBufferStatementStr = foldl (\accum item -> accum ++ item ++ "\n") "" makeBuffers

                    bufferDeclarationStatements = foldl (\accum decl -> accum ++ (synthesiseBufferDeclaration twoTab decl) ++ "\n") "" declarations_noIntent

                    (sizeDeclarations, shapeStatements) = generateSizeStatements_decls twoTab declarations
                    sizeDeclarations_noIntent = map (removeIntentFromDecl) sizeDeclarations
                    sizeDeclarations_str = foldl (\accum item -> accum ++ (synthesiseDecl twoTab item)) "" sizeDeclarations_noIntent
                    shapeStatements_str = foldl (\accum item -> accum ++ (produceCode_fortran ([], "") twoTab [] item)) "" shapeStatements

                    (_, storeBufferStatements_str) = synthesiseBufferStores twoTab allKernelArgsMap emptyArgumentTranslation kernelArgs

                    oclSetArgStatements_str = foldl (\accum item -> accum ++ (synthesiseSetOclArg twoTab allKernelArgsMap item) ++ "\n") "" declarations_noIntent

                    oneTab = tabInc
                    twoTab = oneTab ++ tabInc

-- WV:
-- I guess I could write a extractNodes f = everything (++) ([] `mkQ` f) 
-- but it might be a problem if the result type can't be shown
extractParamDecls :: ProgUnit Anno -> [Decl Anno]
extractParamDecls  = everything (++) ([] `mkQ` isPD)
     where isPD decl@(Decl _ _ _ ( BaseType _ _ [Parameter _] _ _ )) = [decl]
           isPD _ = []
                  
synthesiseSuperKernelModule :: String -> String -> [(Program Anno, String)] -> [(String, String)] -> (String, KernelArgsIndexMap)
synthesiseSuperKernelModule moduleName superKernelName programs kernels =  (kernelModuleHeader
                                                                            ++ contains
                                                                            ++ "\n"
                                                                            ++ (foldl (++) "" kernelCode) 
                                                                            ++ superKernelCode 
                                                                            ++ kernelModuleFooter,
                                                            allKernelArgsMap)
                where
                    kernelCode = map (fst) kernels

                    kernelModuleHeader = "module " ++ moduleName ++ "\n\n"

                    contains = "\n" ++ tabInc ++ "contains\n\n"
                    kernelModuleFooter = "end module " ++ moduleName

                    (superKernelCode, allKernelArgsMap) = synthesiseSuperKernel moduleName outputTab superKernelName programs kernels

synthesiseStateDefinitions :: [(String, String)] -> Int -> String
synthesiseStateDefinitions [] currentVal =  ""
synthesiseStateDefinitions ((kernel, state):xs) currentVal =  "integer, parameter :: " ++ state ++ " = " ++ show currentVal ++ " !  " ++ kernel ++ "\n" ++ (synthesiseStateDefinitions xs (currentVal+1))

synthesiseSuperKernel :: String -> String -> String -> [(Program Anno, String)] -> [(String, String)] -> (String, KernelArgsIndexMap)
synthesiseSuperKernel moduleName tabs name programs [] = ("", DMap.empty)
synthesiseSuperKernel moduleName tabs name programs kernels = if allKernelArgs == [] then error "synthesiseSuperKernel" else (superKernel, allKernelArgsMap)
                where
                    programAsts = map fst programs
                    kernelAstLists = map (extractKernels) programAsts

                    extractedUses = foldl (\accum prog -> listConcatUnique accum (everything (++) (mkQ [] getUses) prog)) [] programAsts
                    useStatements = "use " ++ initModuleName moduleName ++ "\n" 
                                        ++ (foldl (\accum item -> synthesiseUse ([], "") "" [] item) "" extractedUses)

                    kernelNames = map snd kernels
                    stateNames = map generateStateName kernelNames
                    stateDefinitions = synthesiseStateDefinitions (zip kernelNames stateNames) 0

                    kernelAsts = foldl (++) [] (map (\(k_asts, p_ast) -> map (\a -> (a, p_ast)) k_asts) (zip kernelAstLists programAsts))
                    kernelArgs = (map (\(x, _) -> extractKernelArguments x) kernelAsts)
                    allKernelArgs = (listRemoveDuplications (foldl (++) [] kernelArgs)) ++ [statePtrVarName]

                    allKernelArgsMap = foldl (\dmap (arg, index) -> DMap.insert arg index dmap) DMap.empty (zip allKernelArgs ([1..(length allKernelArgs)]))

                    kernelDeclarations = map (\(kernel_ast, prog_ast) -> generateKernelDeclarations prog_ast kernel_ast) kernelAsts
                    (readDecls, writtenDecls, readWriteDecls) =  foldl (\(accum_r, accum_w, accum_g) (r, w, g) -> (accum_r ++ r, accum_w ++ w, accum_g ++ g)) ([],[],[]) kernelDeclarations

                    declarations = map (convertScalarToOneDimArray) (foldl (collectDecls) [] (readDecls ++ writtenDecls ++ readWriteDecls))
                    declarationsStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" declarations

                    stateVarDeclStr = synthesiseDecl tabs stateVarDecl
                    statePointerDeclStr = synthesiseDecl tabs statePtrDecl
                    stateAssignment = tabs ++ (varNameStr stateVarName) ++ " = " ++ (varNameStr stateVarName) ++ "_ptr(1) ! state \n"

                    superKernel_header = "subroutine " ++ name ++ "(" ++ (foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head allKernelArgs) (tail allKernelArgs)) ++ ")\n"
                    superKernel_footer = "end subroutine " ++ name ++ "\n"
                    superKernel_body = "! SUPERKERNEL BODY\n" ++ selectCase

                    caseAlternatives = foldl (\accum (state, name, args) -> accum ++ synthesiseKernelCaseAlternative (tabs ++ outputTab) state name args) "" (zip3 stateNames kernelNames kernelArgs)
                    selectCase = outputTab ++ "select case(" ++ (varNameStr stateVarName) ++ ")\n" ++ caseAlternatives ++ outputTab ++ "end select\n"

                    superKernel = superKernel_header ++ useStatements ++ declarationsStr ++ "\n" ++ stateVarDeclStr ++ statePointerDeclStr ++ stateDefinitions ++ stateAssignment ++ superKernel_body ++ superKernel_footer

synthesiseKernelCaseAlternative :: String -> String -> String -> [VarName Anno] -> String
synthesiseKernelCaseAlternative tabs state kernelName [] = "\n! Skipped call to "++kernelName++", has no args\n" -- error $ "synthesiseKernelCaseAlternative: no arguments for "++kernelName
synthesiseKernelCaseAlternative tabs state kernelName args =  tabs ++ "case (" ++ state ++ ")\n" ++ tabs ++ outputTab ++ "call " ++ kernelName ++ "(" ++ argsString ++ ")" ++ "\n" 
                where
                    argsString = foldl (\accum item -> accum ++ "," ++ (varNameStr item)) (varNameStr $ head args) (tail args)

synthesiseKernels :: Platform -> [String] -> ProgUnit Anno -> (Program Anno, String) -> Fortran Anno -> [(String, String)]
synthesiseKernels plat originalLines orig_ast prog codeSeg = case codeSeg of
                OpenCLMap _ src _ w _ _ _ -> [synthesiseOpenCLMap plat "" originalLines orig_ast prog codeSeg] -- WV20170426
                OpenCLReduce _ src _ _ _ _ rv _ ->  [synthesiseOpenCLReduce plat "" originalLines orig_ast prog codeSeg] -- WV20170426
                _ -> []

synthesiseSizeStatements :: String -> [VarName Anno] -> Program Anno -> (String, String)
synthesiseSizeStatements tabs vars ast = (sizeDeclarations ++ scalarPointerDeclarationStrs, shapeStatements)
        where
            shapeStatements = foldl (\accum varname -> accum ++ tabs ++ (varNameStr (varSizeVarName varname)) ++ " = shape(" ++ (varNameStr varname) ++ ")\n") "" vars_onlyArrays
            sizeDeclarations = foldl (\accum (varname, rank) -> accum ++ tabs ++ "integer, dimension(" ++ (show rank) ++ ") :: " ++ (varNameStr (varSizeVarName varname)) ++ "\n") "" varsWithRanks_arrays
            scalarPointerDeclarationStrs = synthesiseDecls tabs scalarPointerDeclarations

            reduceKernels = extractOpenCLReduces ast
            reductionVarNames = foldl (\accum item -> accum ++ (extractReductionVarNames item)) [] reduceKernels
            global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) ast) reductionVarNames
            global_reductionArrayNames = map (generateGlobalReductionArray) reductionVarNames

            allVars = (listSubtract vars global_reductionArrayNames) ++ global_reductionArrayNames -- Odd code means that allVars has varNames in the correct order

            decl_list = foldl (\accum item -> accum ++ extractDeclaration_varname item ast) [] vars
            dimensionRanks = map (getDeclRank) (decl_list ++ global_reductionArraysDecls)
            varsWithRanks = (statePtrVarName, 1):(zip allVars dimensionRanks)

            varsWithRanks_arrays = map (\(var, rank) -> if rank == 0 then (scalarPointerVarName var, 1) else (var, rank)) varsWithRanks

            vars_onlyArrays = map (\(var, rank) -> if rank == 0 then scalarPointerVarName var else var) varsWithRanks
            vars_onlyScalars = listSubtract allVars vars_onlyArrays
            scalarPointerDeclarations = map (\x -> removeIntentFromDecl (declareScalarPointer x ast)) vars_onlyScalars

synthesiseSizeStatements_kernel :: String -> Program Anno -> (String, String)
synthesiseSizeStatements_kernel tabs ast = synthesiseSizeStatements tabs allBufferAccesses ast
        where
            kernels = extractKernels ast
            bufferReads = extractBufferReads ast
            bufferWrites = extractBufferWrites ast
            
            kernelArgs = listRemoveDuplications (foldl (\accum item -> accum ++ (extractKernelArguments item)) [] kernels)
            bufferReadVars = map (\(OpenCLBufferRead _ _ var) -> var) bufferReads
            bufferWrittenVars = map (\(OpenCLBufferWrite _ _ var) -> var) bufferWrites
        
            allBufferAccesses = listRemoveDuplications (kernelArgs ++ bufferWrittenVars ++ bufferReadVars)


synthesiseSetOclArg :: String -> KernelArgsIndexMap -> Decl Anno -> String
synthesiseSetOclArg tabs allKernelArgsMap (Decl anno src lst typ) = case baseType of
                                                        Integer _ -> prefix ++ "Int" ++ suffix
                                                        Real _ -> prefix ++ "Float" ++ suffix
                                                        _ -> ""
        where
            assigneeName = extractAssigneeFromDecl (Decl anno src lst typ)
            kernelArgIndex_C = (DMap.findWithDefault (error ("synthesiseSetOclArg: arg "++(show assigneeName)++" doesn't exist in KernelArgsIndexMap")) assigneeName allKernelArgsMap) - 1

            baseType = extractBaseType typ

            prefix = tabs ++ "call oclSet"
            suffix = "ArrayArg(" ++ (show kernelArgIndex_C) ++ ", " ++ (varNameStr (varBufVarName assigneeName)) ++ ")"

synthesiseBufferDeclaration :: String -> Decl Anno -> String
synthesiseBufferDeclaration tabs decl = tabs ++ "integer(8) :: " ++ (varNameStr (varBufVarName assignee))
        where
            assignee = extractAssigneeFromDecl decl

synthesiseBufferStores :: String -> KernelArgsIndexMap -> ArgumentTranslation -> [VarName Anno] -> (String, String)
synthesiseBufferStores tabs allKernelArgsMap argTranslation vars = (declarationStatements, storeBufferStatements)
        where
            -- storeBufferStatements = foldl (\accum (var, index) -> accum ++ tabs ++ "call oclStoreBuffer(" ++ (show index) ++ ", " ++ (varNameStr (varBufVarName var)) ++ ")" ++  "\n") "" kernelBufferIndices
            storeBufferStatements = foldl (\accum (var, index) -> accum ++ tabs ++ "call oclStoreBuffer(" ++ (map toUpper (varNameStr (varBufVarName var))) ++ "_IDX" ++ ", " ++ (varNameStr (varBufVarName var)) ++ ")" ++  "\n") "" kernelBufferIndices
            declarationStatements = foldl (\accum var -> accum ++ tabs ++ "integer(8) :: " ++ (varNameStr (varBufVarName var)) ++ "\n") "" translatedVars

            translatedVars = translateArguments argTranslation vars
            kernelBufferIndices = zip vars (map (\x -> DMap.findWithDefault (-1) x allKernelArgsMap) translatedVars)

synthesiseBufferLoads :: String -> KernelArgsIndexMap -> ArgumentTranslation -> [VarName Anno] -> (String, String)
synthesiseBufferLoads tabs allKernelArgsMap argTranslation vars = (declarationStatements, loadBufferStatements)
        where
            --loadBufferStatements = foldl (\accum (var, index) -> accum ++ tabs ++ "call oclLoadBuffer(" ++ (show index) ++ ", " ++ (varNameStr (varBufVarName var)) ++ ")" ++  "\n") "" kernelBufferIndices
            loadBufferStatements = foldl (\accum (var, index) -> accum ++ tabs ++ "call oclLoadBuffer(" ++  (map toUpper (varNameStr (varBufVarName var))) ++ "_IDX" ++ ", " ++ (varNameStr (varBufVarName var)) ++ ")" ++  "\n") "" kernelBufferIndices
            declarationStatements = foldl (\accum var -> accum ++ tabs ++ "integer(8) :: " ++ (varNameStr (varBufVarName var)) ++ "\n") "" translatedVars

            translatedVars = translateArguments argTranslation vars
            kernelBufferIndices = zip vars (map (\x -> DMap.findWithDefault (-1) x allKernelArgsMap) translatedVars)

synthesiseBufferIndexNames tabs allKernelArgsMap argTranslation vars = bufferIndexNameDecls  
        where
            bufferIndexNameDecls = unlines $ map (\(var,index) -> (tabs ++ "integer, parameter ::" ++ (map toUpper (varNameStr (varBufVarName var))) ++ "_IDX = " ++  (show index) ))  kernelBufferIndices
            translatedVars = translateArguments argTranslation vars
            kernelBufferIndices = zip vars (map (\x -> DMap.findWithDefault (-1) x allKernelArgsMap) translatedVars)


synthesiseBufferLoads_kernel :: String -> KernelArgsIndexMap -> ArgumentTranslation -> Block Anno -> (String, String)
synthesiseBufferLoads_kernel tabs allKernelArgsMap argTranslation ast = synthesiseBufferLoads tabs allKernelArgsMap argTranslation allBufferAccesses
        where
            kernels = extractKernels ast
            bufferReads = extractBufferReads ast
            bufferWrites = extractBufferWrites ast
            
            kernelArgs = listRemoveDuplications (foldl (\accum item -> accum ++ (extractKernelArguments item)) [] kernels)
            bufferReadVars = map (\(OpenCLBufferRead _ _ var) -> var) bufferReads
            bufferWrittenVars = map (\(OpenCLBufferWrite _ _ var) -> var) bufferWrites

            allBufferAccesses = listRemoveDuplications ([statePtrVarName] ++ kernelArgs ++ bufferWrittenVars ++ bufferReadVars)

synthesiseUse :: (Program Anno, String) -> String -> [String] -> Uses Anno -> String
synthesiseUse prog tabs originalLines (Use _ (moduleName, _) _ _) = tabs ++ "use " ++ moduleName ++ "\n"

synthesiseCall :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseCall prog tabs originalLines (Call anno src expr args)    |    partialGenerated = prefix ++ tabs ++ "call " ++ (outputExprFormatting expr) ++ (synthesiseArgList args) ++ suffix ++ "\n"
                                                                    |    otherwise = prefix ++ (extractOriginalCode originalLines src) ++ suffix
        where
            partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
            codeSeg = (Call anno src expr args)
            subroutineName = (outputExprFormatting expr)
            prefix = ""
            suffix = ""

synthesiseArgList :: ArgList Anno -> String
synthesiseArgList (ArgList _ expr) = "(" ++ (synthesiseESeq expr) ++ ")"

synthesiseESeq :: Expr Anno -> String
synthesiseESeq (ESeq _ _ expr1 expr2) = (synthesiseESeq expr1) ++ "," ++ (synthesiseESeq expr2)
synthesiseESeq expr = outputExprFormatting expr

synthesisUses :: String -> Uses Anno -> String
synthesisUses tabs (Use _ (str, rename) _ _) = tabs ++ "use " ++ str ++ "\n"
synthesisUses tabs _ = ""

synthesiseOpenCLBufferWrite :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseOpenCLBufferWrite (progAst, filename) tabs originalLines (OpenCLBufferWrite anno src varName) = bufferWrite ++ "\n"
        where
            bufferWrite = synthesiseBufferAccess tabs "Write" writeDecl
            (_, writeDecl:_, _) = generateKernelDeclarations progAst (OpenCLBufferWrite anno src varName)

synthesiseOpenCLBufferRead :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseOpenCLBufferRead (progAst, filename) tabs originalLines (OpenCLBufferRead anno src varName) = bufferRead ++ "\n"
        where
            bufferRead = synthesiseBufferAccess tabs "Read" readDecl
            (readDecl:_, _, _) = generateKernelDeclarations progAst (OpenCLBufferRead anno src varName)

synthesiseFor :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseFor prog tabs originalLines (For anno src varname expr1 expr2 expr3 fort)     |    partialGenerated = tabs ++ "do " ++ (varNameStr varname) ++ "=" ++ outputExprFormatting expr1 
                                                                                                ++ ", " ++ outputExprFormatting expr2 ++ (if expr3isOne then "" else outputExprFormatting expr3)
                                                                                                ++ "\n" ++ (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) fort) ++ tabs ++ "end do\n"
                                                                                        |    otherwise = extractOriginalCode originalLines src
                                                                    where
                                                                        expr3isOne = case expr3 of
                                                                                        Con _ _ "1" -> True
                                                                                        _ -> False
                                                                        partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
                                                                        codeSeg = For anno src varname expr1 expr2 expr3 fort

synthesiseAssg :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseAssg prog tabs originalLines (Assg anno src expr1 expr2)    |    partialGenerated = tabs ++ outputExprFormatting expr1 ++ " = " ++ outputExprFormatting expr2 ++ "\n"
                                                            |    otherwise = extractOriginalCode originalLines src
                                            where 
                                                partialGenerated = isGenerated codeSeg
                                                codeSeg = Assg anno src expr1 expr2

synthesiseIf :: (Program Anno, String) -> String -> [String] -> Fortran Anno -> String
synthesiseIf prog tabs originalLines (If anno src expr fortran lst maybeFort)     |    partialGenerated = tabs ++ "if (" ++ outputExprFormatting expr ++ ") then\n" 
                                                                    ++ mainFortranStr
                                                                    ++ elseIfFortranStr
                                                                    ++ elseFortranStr
                                                                    ++ tabs ++ "end if\n"
                                                            |    otherwise = extractOriginalCode originalLines src
                                            where 
                                                partialGenerated = anyChildGenerated codeSeg || isGenerated codeSeg
                                                codeSeg = If anno src expr fortran lst maybeFort
                                                mainFortranStr = (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) fortran)
                                                elseIfFortranStr = foldl (synthesisElses prog tabs originalLines) "" lst
                                                elseFortranStr = case maybeFort of
                                                                    Just a -> tabs ++ "else\n" ++ (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) a)
                                                                    Nothing -> ""

synthesisElses :: (Program Anno, String) -> String -> [String] -> String -> (Expr Anno, Fortran Anno) -> String
synthesisElses prog tabs originalLines accum (expr, fortran) = accum ++ tabs ++ "else if (" ++ outputExprFormatting expr ++ ") then\n" 
                                                                ++ (mkQ "" (produceCode_fortran prog (tabs ++ tabInc) originalLines) fortran)
                                                                ++ "\n"

synthesiseDecl :: String -> Decl Anno -> String
synthesiseDecl tabs (Decl anno src lst typ) = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ "\n"
synthesiseDecl tabs (DSeq _ decl1 decl2) = (synthesiseDecl tabs decl1) ++ (synthesiseDecl tabs decl2)
synthesiseDecl tabs (NullDecl nullAnno nullSrcSpan) = tabs ++ "[Variable not declared in orignal source]\n"
synthesiseDecl tabs _ = tabs ++ "[Unimplemented declaration syntax] \n"

synthesiseDecls :: String -> [Decl Anno] -> String
synthesiseDecls tabs decls = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" decls

synthesiseDecl_Acc :: String -> Decl Anno -> String -> String
synthesiseDecl_Acc tabs (Decl anno src lst typ) acc = tabs ++ (synthesiseType typ) ++ " :: " ++ (synthesiseDeclList lst) ++ " " ++ acc ++ "\n"
synthesiseDecl_Acc tabs (DSeq _ decl1 decl2) acc = (synthesiseDecl_Acc tabs decl1 acc) ++ (synthesiseDecl_Acc tabs decl2 acc)
synthesiseDecl_Acc tabs decl _ = synthesiseDecl tabs decl

synthesiseDecls_Acc :: String -> [Decl Anno] -> String -> String
synthesiseDecls_Acc tabs decls acc = foldl (\accum item -> accum ++ synthesiseDecl_Acc tabs item acc) "" decls

synthesiseType :: Type Anno -> String
synthesiseType (BaseType anno base attrList (expr1) (expr2)) = baseStr ++ kindStr ++ attrStr
                                            where
                                                baseStr = synthesiseBaseType base
                                                kindStr = case outputExprFormatting expr1 of
                                                                "" -> ""
                                                                str -> "(kind=" ++ str ++ ")"
                                                attrStr = case synthesiseAttrList attrList of
                                                                "" -> ""
                                                                str -> ", " ++ str

synthesiseBaseType :: BaseType Anno -> String
synthesiseBaseType (Integer _) = "integer"
synthesiseBaseType (Real _) = "real"
synthesiseBaseType (Character _) = "character"
synthesiseBaseType typ = "[Incompatible type]"
-- synthesiseBaseType (SomeType _) = "<SomeType>"
-- synthesiseBaseType (DerivedType _ _) = "<DerivedType>"
-- synthesiseBaseType (Recursive _) = "<Recursive>"
-- synthesiseBaseType (Pure _) = "<Pure>"
-- synthesiseBaseType (Elemental _) = "<Elemental>"
-- synthesiseBaseType (Logical _) = "<Logical>"
-- synthesiseBaseType (Complex _) = "<Complex>"

synthesiseAttrList :: [Attr Anno] -> String
synthesiseAttrList [] = ""
synthesiseAttrList (attr:[]) = if paramCheck_attr attr then "" else synthesiseAttr attr
synthesiseAttrList attrList = if attrStrList == [] then error "synthesiseAttrList" else attrStrs
                where 
                    attrStrList = map (synthesiseAttr) (filter (\x -> not (paramCheck_attr x)) attrList)
                    attrStrs = foldl (\accum item -> accum ++ ", " ++ item) (head attrStrList) (tail attrStrList)

synthesiseAttr :: Attr Anno -> String
synthesiseAttr (Dimension _ exprList) = "dimension(" ++ synthesiseRangeExpr exprList ++ ")"
synthesiseAttr (Intent _ intentAttr) = "intent(" ++ intentStr ++ ")"
                    where 
                        intentStr = case intentAttr of
                            In _ -> "In"
                            Out _ -> "Out"
                            _ -> "InOut"
synthesiseAttr (Parameter _) = "parameter"
synthesiseAttr attr = "[Incompatible attribute]"
-- synthesiseAttr (Allocatable _) = ""
-- synthesiseAttr (External _) = ""
-- synthesiseAttr (Intrinsic _) = ""
-- synthesiseAttr (Optional _) = ""
-- synthesiseAttr (Pointer _) = ""
-- synthesiseAttr (Save _) = ""
-- synthesiseAttr (Target _) = ""
-- synthesiseAttr (Volatile _) = ""
-- synthesiseAttr (Public _) = ""
-- synthesiseAttr (Private _) = ""
-- synthesiseAttr (Sequence _) = ""
-- synthesiseAttr (MeasureUnit _ _) = ""

synthesiseRangeExpr :: [(Expr Anno, Expr Anno)] -> String
synthesiseRangeExpr [] = ""
synthesiseRangeExpr ((NullExpr _ _, expr2):[]) = outputExprFormatting expr2
synthesiseRangeExpr ((NullExpr _ _, expr2):xs) = outputExprFormatting expr2 ++ "," ++ synthesiseRangeExpr xs
synthesiseRangeExpr ((expr1, expr2):[]) = outputExprFormatting expr1 ++ ":" ++ outputExprFormatting expr2
synthesiseRangeExpr ((expr1, expr2):xs) = outputExprFormatting expr1 ++ ":" ++ outputExprFormatting expr2 ++ "," ++ synthesiseRangeExpr xs

synthesiseDeclList :: [(Expr Anno, Expr Anno, Maybe Int)] -> String
synthesiseDeclList ((expr1, expr2, maybeInt):xs) = outputExprFormatting expr1 ++ 
                                                (case expr2 of
                                                    NullExpr _ _ -> ""
                                                    _ ->  " = " ++ outputExprFormatting expr2)
                                                ++ maybeIntStr
                                                ++ followingItems
                        where
                            maybeIntStr = case maybeInt of
                                            Just a -> ", " ++ show a
                                            Nothing -> ""
                            followingItems = case xs of 
                                                [] -> ""
                                                _ -> ", " ++ synthesiseDeclList xs

--    Function handles the construction of map kernels. The body of the kernel is the original source that appeared in the loop that
--    has been parallelised. The extra elements are initialising OpenCL related constructs and wrapping subroutine syntax.
--                    | OpenCLMap p SrcSpan                   -- Node to represent the data needed for an OpenCL map kernel
--                  [VarName p]                           -- List of arguments to kernel that are READ
--                  [VarName p]                           -- List of arguments to kernel that are WRITTEN
--                  [(VarName p, Expr p, Expr p, Expr p)] -- Loop variables of nested maps: var, from, to, step
--                  (Fortran p)                           -- Body of kernel code
--WV: so here is where we are missing some variable declarations, and also in the *Reduce version      
--So what we need to do here is 
--1/ find *all* variables used in the code segment 'fortran'
--2/ determine which ones need to be arguments and which one locals
-- WV: I wonder if there is any benefit in padding the NDRange for Map. If so then that needs to be done partially on the host of course. TODO
-- WV: Also, apart from padding, there is also loop unrolling to consider, which might help for CPU and MIC, and which would require padding. TODO
synthesiseOpenCLMap :: Platform -> String -> [String] -> ProgUnit Anno -> (Program Anno, String) -> Fortran Anno -> (String,String)
synthesiseOpenCLMap plat inTabs originalLines orig_ast programInfo (OpenCLMap anno src r w l il fortran) = -- WV20170426
                                                                (
                                                                    inTabs ++ "subroutine " ++ kernelName
                                                                    ++"(" 
                                                                                    ++ allArgs_ptrAdaptionStr ++ ")\n"
                                                                    ++ usesString
                                                                    ++ "\n"
                                                                    ++ paramDeclStrs
                                                                    ++ tabs ++localVarsStr 
                                                                    ++ localDeclStrs
                                                                    ++ tabs ++ "! " ++ compilerName ++ ": Synthesised loop variable decls\n"
                                                                    ++ range_rel_decls_str
                                                                    ++ "! READ\n"
                                                                    ++ readDeclStr
                                                                    ++ "! WRITTEN\n"
                                                                    ++ writtenDeclStr
                                                                    ++ "! READ & WRITTEN\n"
                                                                    ++ readWriteDeclStr
                                                                    ++ "! globalIdDeclaration\n"
                                                                    ++ tabs ++ globalIdDeclaration
                                                                    ++ "! globalIdInitialisation\n"
                                                                    ++ tabs ++ globalIdInitialisation
                                                                    ++ "! ptrAssignments_fseq\n"
                                                                    ++ (produceCode_fortran programInfo tabs originalLines ptrAssignments_fseq)
                                                                    ++ "\n"
                                                                    ++ tabs ++ "! " ++ compilerName ++ ": Synthesised loop variables\n"
                                                                    ++ produceCode_fortran programInfo tabs originalLines loopInitialiserCode 
                                                                    ++ "\n\n"
                                                                    ++ tabs ++ "! " ++ compilerName ++ ": Original code\n" 
                                                                    ++ (mkQ "" (produceCode_fortran programInfo (tabs) originalLines) fortran) 
                                                                    ++ "\n"
                                                                    ++ inTabs ++ "end subroutine " ++ kernelName
                                                                    ++ "\n\n\n"
                                                                    , kernelName
                                                                 )

                                            where
                                                orig_decls_stmts = ( \(Block _ _ _ _ decls stmts) -> (decls,stmts) ) ( ( \(Sub _ _ _ _ _ b) -> b ) orig_ast )
                                                extractedUses = everything (++) (mkQ [] getUses) prog
                                                usesString = foldl (\accum item -> accum ++ synthesisUses tabs item) "" extractedUses 

                                                prog = fst programInfo

                                                kernelName = generateKernelName "map" src
                                                globalIdVar = generateVar (VarName nullAnno "global_id")
                                                tabs = inTabs ++ tabInc
                                                allArgs = extractKernelArguments (OpenCLMap anno src r w l il fortran)       -- WV20170426
                                                allKernelVars = extractAllVarNames fortran
                                                -- WV: local variable declarations
                                                (localVarsStr,localDeclStrs,paramDeclStrs) = getLocalDeclStrs allArgs allKernelVars orig_decls_stmts originalLines tabs kernelName
                                                -- WV: declarations for _range and _rel
                                                range_rel_decls_str = generateRangeRelDecls l tabs --  we can do this because they *must* be integers    
                                                (readDecls', writtenDecls', readWriteDecls', ptrAssignments_fseq, allArgs_ptrAdaption) = 
                                                    adaptForReadScalarDecls allArgs (generateKernelDeclarations prog (OpenCLMap anno src r w l il fortran)) -- WV20170426
                                                allArgs_ptrAdaptionStr = case allArgs_ptrAdaption of
                                                            [] -> ""
                                                            args -> foldl (\accum item -> accum ++ "," ++ varNameStr item) (varNameStr (head args)) (tail args)
                                                loopVars = map (\(v,_,_,_) -> v) l
                                                readDecls = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem`  loopVars ) ) readDecls'
                                                writtenDecls = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem`  loopVars ) ) writtenDecls'
                                                readWriteDecls = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem`  loopVars ) ) readWriteDecls'
                                                            

                                                readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" readDecls
                                                writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (writtenDecls)
                                                readWriteDeclStr = foldl (\accum item -> accum ++ synthesiseDecl (tabs) item) "" (readWriteDecls)

                                                globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"
                                                globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"

                                                loopInitialisers = generateLoopInitialisers l globalIdVar Nothing
                                                loopInitialiserCode = case loopInitialisers of
                                                                        [] -> error "synthesiseOpenCLMap: loopInitialiserCode - empty list"
                                                                        _ -> foldl1 (\accum item -> appendFortran_recursive item accum) loopInitialisers

generateRangeRelDecls :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> String -> String
generateRangeRelDecls loopvartups tabs =
    let
        loopvarnames = nub $ map (\(v,_,_,_) -> varNameStr v) loopvartups
        rangevardecls = map (\v -> tabs++"integer :: "++v++"_range") loopvarnames
        relvardecls = map (\v -> tabs++"integer :: "++v++"_rel") loopvarnames
    in
        unlines $ rangevardecls ++ relvardecls

getLocalDeclStrs :: [VarName Anno] -> [VarName Anno] -> (Decl Anno, Fortran Anno) -> [String] -> String -> String -> (String,String,String)
getLocalDeclStrs allArgs allKernelVars orig_decls_stmts originalLines tabs kernelName =
    let
                                                allArgs_strs = map varNameStr allArgs
                                                allVars' =  extractAllVarNames fortran -- (warning fortran ("\n! KERNEL: "++kernelName++"\n"++(miniPPF fortran)))
                                                allVars = nub allVars' 
                                                allVars_strs = map varNameStr allVars
                                                (decls,fortran) = orig_decls_stmts -- WV: TODO: currently unused
                                                localVars_strs' = filter (\var -> not (var `elem` allArgs_strs)) allVars_strs -- so here u0 and test are missing from allVars AND from allArgs
                                                -- WV of these localVars_strs, only the ones occuring in the kernel should be retained
                                                -- WV so we need allKernelVars_strs 
                                                allKernelVars_strs  = map varNameStr allKernelVars
                                                localVars_strs = filter (\var ->  (var `elem` allKernelVars_strs)) localVars_strs'
                                                localVarsStr =  "! Local vars: "++ ( intercalate "," localVars_strs) ++"\n" -- ++(miniPPD decls);
                                                -- so now I need to get the corresponding declarations from the original subroutine
                                                -- FIXME: GD's code assumes only one subroutine per filename and also that the sub and the file have the same name! getModuleName just removes the extension!
                                                -- TODO: GD does not emit code from the AST, rather the uses the original lines as text. I do the same, but its UGLY!
                                                -- now get the declarations, so we get them from the prog
                                                -- If we assume that the code went through rf4a first, we can do this:
                                                origDeclLines =  filter findDeclLine originalLines
--                                                origDeclLines = lines $ miniPPD decls
                                                matchingOrigDeclLines = map (\var_name -> unlines $ filter (matchVarNameInDecl var_name) origDeclLines) localVars_strs
                                                matchingOrigDeclLinesNoIntent = map removeIntent matchingOrigDeclLines                                                
                                                localDeclStrs = unlines (map (\l->tabs++l) matchingOrigDeclLinesNoIntent)
                                                paramDeclStrs = unlines (map (\l->tabs++l) ( filter matchParameterDecl origDeclLines))
--                                                localDeclStrs = "\n! BEGIN local decsl !\n"++(miniPPD decls)++"\n! END of local decls !\n"
    in
        (localVarsStr,localDeclStrs,paramDeclStrs)
        
getLocalDeclStrs_OLD :: [VarName Anno] -> Fortran Anno -> [String] -> String -> String -> (String,String)
getLocalDeclStrs_OLD allArgs fortran originalLines tabs kernelName =
    let
                                                allArgs_strs = map varNameStr allArgs
                                                allVars' =  extractAllVarNames fortran -- (warning fortran ("\n! KERNEL: "++kernelName++"\n"++(miniPPF fortran)))
                                                allVars = Data.Set.toList $ Data.Set.fromList allVars' -- this is a trick to remove duplicates
                                                allVars_strs = map varNameStr allVars
                                                localVars_strs = filter (\var -> not (var `elem` allArgs_strs)) allVars_strs -- so here u0 and test are missing from allVars AND from allArgs
                                                localVarsStr =  "! Local vars: "++ ( intercalate "," localVars_strs) ++"\n";
                                                -- so now I need to get the corresponding declarations from the original subroutine
                                                -- FIXME: GD's code assumes only one subroutine per filename and also that the sub and the file have the same name! getModuleName just removes the extension!
                                                -- TODO: GD does not emit code from the AST, rather the uses the original lines as text. I do the same, but its UGLY!
                                                -- now get the declarations, so we get them from the prog
                                                -- If we assume that the code went through rf4a first, we can do this:
                                                origDeclLines =  filter findDeclLine originalLines
                                                matchingOrigDeclLines = map (\var_name -> unlines $ filter (matchVarNameInDecl var_name) origDeclLines) localVars_strs
                                                matchingOrigDeclLinesNoIntent = map removeIntent matchingOrigDeclLines                                                
                                                localDeclStrs = unlines (map (\l->tabs++l) matchingOrigDeclLinesNoIntent)
    in
        (localVarsStr,localDeclStrs)

-- So I think I need to identify any non-local variable in the loop body, i.e. get all vars from the loop body (OK, done) and check which ones are arguments of the original function. These must be args

getArgsAndLocalVarsForLoopBody :: Fortran Anno -> Program Anno -> ( [String], [String] )
getArgsAndLocalVarsForLoopBody fortran prog  =
    let
                                                allVars' =  extractAllVarNames fortran
                                                allVars = Data.Set.toList (Data.Set.fromList allVars')
                                                allVars_strs = map varNameStr allVars
                                                origSubAST = head $ extractSubroutines prog
                                                (Sub _ _ _ _  args _) = origSubAST
                                                origSubArgs = everything (++) (mkQ [] extractArgName) args
                                                origSubArgs_strs = map (\(ArgName _ an) ->an) origSubArgs
                                                loopkernel_args_strs = filter (\v -> v `elem` origSubArgs_strs) allVars_strs
                                                loopkernel_localvars_strs = filter (\v -> not (v `elem` origSubArgs_strs)) allVars_strs                                                
    in
       (loopkernel_localvars_strs, loopkernel_args_strs)


getMissingArgDeclStrs missingArgs_strs originalLines tabs =
    let
        missingArgsStr = tabs ++ "! Missing args: "++ (intercalate "," missingArgs_strs) ++ "\n"
        origDeclLines =  filter findDeclLine originalLines
        matchingOrigDeclLines = map (\var_name -> unlines $ filter (matchVarNameInDecl var_name) origDeclLines) missingArgs_strs 
        missingArgDeclsStr = if null matchingOrigDeclLines then "" else foldl1 (++) matchingOrigDeclLines
    in
        (missingArgsStr,missingArgDeclsStr) 

--    Function handles the production of OpenCL reduction kernels. Clearly it is much more complicated the the 'synthesiseOpenCLMap' function because the final reduction
--    kernel need be smarter than a map.

--    BASIC STEPS
--    -    Produce subroutine header with appropriate arguments 
--    -    Deteremine imports and produce 'using' statements 
--    -    Produce declarations for new variables that are introduced for OpenCL's sake.
--    -    Get declarations for the existing variables from the original srouce
--    -    Declare variables that are used directly by the tree based reduction
--    -    Initialise OpenCL related variables like 'local_id'
--    -    Produce the first reduction loop, the one that happens per thread. The body of this loop is functionally the same code that appeared
--        in the original source.
--    -    Produce synchronisation barrier
--    -    Produce second reduction loop, computer unit loop. Functionality is adapted from original code and only includes the assosciative,
--        primary reduction operation.
--    -    Assign values to global result array that host will reduce later
--    -    End subroutine. 
synthesiseOpenCLReduce :: Platform -> String ->  [String] -> ProgUnit Anno ->  (Program Anno, String)-> Fortran Anno -> (String,String)
synthesiseOpenCLReduce plat inTabs originalLines orig_ast programInfo (OpenCLReduce anno src r w l il rv fortran)  = -- WV20170426
                                (
                                       inTabs ++ "subroutine " ++ kernelName
                                       ++ "("                 
                                       ++ allArgs_ptrAdaptionStr
                                       ++ ")\n"
                                       ++ usesString
                                       ++ "\n"
                                       -- ++ allArgsStr
                                       -- ++ origArgsStr
                                       ++ paramDeclStrs
                                       ++ missingArgsStr
                                       ++ missingArgDeclsStr
                                       ++ tabs ++ localVarsStr
                                       ++ "\n" ++ localDeclStrs ++ "\n"                                                                            
                                       ++ tabs ++ "! " ++ compilerName ++ ": Synthesised loop variable decls\n"
                                       ++ range_rel_decls_str
                                       ++ "\n"
                                       ++ tabs ++ chunk_sizeDeclaration
                                       ++ tabs ++ localIdDeclaration
                                       ++ tabs ++ localIdFortranDeclaration
                                       ++ tabs ++ groupIdDeclaration
                                       ++ tabs ++ groupIdFortranDeclaration
                                       ++ tabs ++ globalIdDeclaration 
                                       ++ tabs ++ reductionIteratorDeclaration
                                       ++ (if plat == GPU then
                                              tabs ++ idxDeclaration
                                           ++ tabs ++ ndrangeDeclaration
                                           ++ tabs ++ ndrange_padded_Declaration
                                           ++ tabs ++ nthreads_Declaration
                                          else "")
                                       ++ tabs ++ localChunkSizeDeclaration
                                       ++ tabs ++ startPositionDeclaration
                                       ++ "\n"
                                       ++ readDeclStr
                                       ++ writtenDeclStr
                                       ++ readWriteDeclStr
                                       ++ "\n"
                                       ++ "#if NTH > 1\n"
                                       ++ tabs ++ "! Arrays prefixed with \'local_\' should be declared using the \'__local\' modifier in C kernel version\n"
                                       ++ workGroup_reductionArraysDeclStr
                                       ++ "#endif\n"
                                       ++ local_reductionVarsDeclatationStr
                                       ++ "\n"
                                       ++ tabs ++ groupIdInitialisation
                                       ++ tabs ++ globalIdInitialisation
                                       ++ "#if NTH > 1\n"
                                       ++ tabs ++ localIdInitialisation
                                       ++ "\n" ++ tabs ++ "! local_id_fortran and group_id_fortran are used to reconcile the fact that fortran arrays are referenced from 1"
                                       ++ "\n" ++ tabs ++ "! not 0 like other OpenCL supporting languages\n"
                                       ++ tabs ++ localIdFortranInitialisation
                                       ++ "#endif\n"
                                       ++ tabs ++ groupIdFortranInitialisation
                                       ++ (if plat == GPU then
                                              tabs ++ "nthreads = NUNITS*NTH\n"
                                           ++ tabs ++ "ndrange = "++ globalWorkItems_str ++"\n"
                                           ++ tabs ++ "ndrange_padded = ndrange\n"
                                           ++ tabs ++ "\n"
                                           ++ tabs ++ "if (mod(ndrange_padded, (NUNITS*NTH) ) > 0) then\n" 
                                           ++ tabs ++ "    ndrange_padded = ( (ndrange_padded/ (NUNITS*NTH) ) +1)* (NUNITS*NTH)\n"
                                           ++ tabs ++ "end if\n"      
                                           ++ tabs ++ "chunk_size = ndrange_padded / NUNITS\n"
                                        else "")

                                       ++ "#if NTH > 1\n"
                                       ++ tabs ++ (if plat == GPU then "local_chunk_size = chunk_size / NTH\n" else localChunkSize_GPU_str)
                                       ++ "#else\n"
                                       ++ tabs ++ (if plat == GPU then "local_chunk_size = chunk_size\n" else localChunkSize_CPU_str)
                                       ++ "#endif\n"
                                       ++ tabs ++ (if plat == CPU then startPosition_str else startPosition_str') ++ "\n"
                                       ++ local_reductionVarsInitStr
                                       ++ "\n"
                                       ++ (if plat == GPU then
                                           (mkQ "" (produceCode_fortran programInfo tabs originalLines) workItem_guarded_loop)
                                       else
                                           (mkQ "" (produceCode_fortran programInfo tabs originalLines) workItem_loop)
                                          )
                                       ++ "\n"
                                       ++ "#if NTH > 1\n"
                                       ++ workGroup_reductionArraysInitStr
                                       ++ "\n"
                                       ++ "#ifdef BARRIER_OK\n"
                                       ++ tabs ++ localMemBarrier
                                       ++ "#endif\n"
                                       ++ "\n"
                                       ++ local_reductionVarsInitStr
                                       ++ (mkQ "" (produceCode_fortran programInfo (tabs) originalLines) workGroup_loop)
                                       ++ "#endif\n"
                                       ++ global_reductionArraysAssignmentStr
                                       ++ "\n"
                                       ++ inTabs ++ "end subroutine " ++ kernelName
                                       ++"\n\n\n"
                                       , kernelName
                                   )
       where
           orig_decls_stmts = (\(Block _ _ _ _ decls stmts) -> (decls,stmts)) (( \(Sub _ _ _ _ _ b) -> b ) orig_ast)
           prog = fst programInfo

           kernelName = generateKernelName "reduce" src 
           tabs = inTabs ++ tabInc

           extractedUses = everything (++) (mkQ [] getUses) prog
           usesString = foldl (\accum item -> accum ++ synthesisUses tabs item) "" extractedUses 

           reductionVarNames = map (\(varname, expr) -> varname) rv
           -- readVarNames = listSubtract (listSubtract r w) reductionVarNames
           -- writtenVarNames = listSubtract (listSubtract w r) reductionVarNames
           -- readWriteVarNames = listSubtract (listIntersection w r) reductionVarNames

           localIdVar = generateVar (VarName nullAnno "local_id")
           localIdFortranVar = generateVar (VarName nullAnno "local_id_fortran")
           groupIdVar = generateVar (VarName nullAnno "group_id")
           groupIdFortranVar = generateVar (VarName nullAnno "group_id_fortran")
           globalIdVar = generateVar (VarName nullAnno "global_id")

           localIdDeclaration = "integer :: " ++ outputExprFormatting localIdVar ++ "\n"
           localIdFortranDeclaration = "integer :: " ++ outputExprFormatting localIdFortranVar ++ "\n"
           groupIdDeclaration = "integer :: " ++ outputExprFormatting groupIdVar ++ "\n"
           groupIdFortranDeclaration = "integer :: " ++ outputExprFormatting groupIdFortranVar ++ "\n"
           globalIdDeclaration = "integer :: " ++ outputExprFormatting globalIdVar ++ "\n"
           reductionIteratorDeclaration = "integer :: " ++ varNameStr reductionIterator ++ "\n"
           localChunkSizeDeclaration = "integer :: " ++ outputExprFormatting localChunkSize ++ "\n"
           startPositionDeclaration = "integer :: " ++ outputExprFormatting startPosition ++ "\n"
           chunk_sizeDeclaration = "integer :: " ++ outputExprFormatting chunk_size ++ "\n"
           idxDeclaration  = emitDeclStr "integer" "idx"
           ndrangeDeclaration = emitDeclStr "integer" "ndrange"
           ndrange_padded_Declaration = emitDeclStr "integer" "ndrange_padded"
           nthreads_Declaration = emitDeclStr "integer" "nthreads"
           localIdInitialisation = "call " ++ outputExprFormatting (getLocalId localIdVar) ++ "\n"
           localIdFortranInitialisation = synthesiseAssg programInfo inTabs originalLines (generateAssgCode localIdFortranVar (generateAdditionExpr localIdVar (generateIntConstant 1)))
           groupIdInitialisation = "call " ++ outputExprFormatting (getGroupID groupIdVar) ++ "\n"
           groupIdFortranInitialisation = synthesiseAssg programInfo inTabs originalLines (generateAssgCode groupIdFortranVar (generateAdditionExpr groupIdVar (generateIntConstant 1)))
           globalIdInitialisation = "call " ++ outputExprFormatting (getGlobalID globalIdVar) ++ "\n"
           globalWorkItems_str = outputExprFormatting (generateGlobalWorkItemsExpr l)

           allArgs = extractKernelArguments (OpenCLReduce anno src r w l il rv fortran) -- WV: this is incorrect, these are only *some* args -- WV20170426
           -- FIXME: so here I am adding the missing ones. Instead I should figure out what's wrong with the r/w arguments
           allArgs_strs = map varNameStr allArgs
           allKernelVars = extractAllVarNames fortran
           -- all args from the original subroutine used in the loop body
           (loopkernel_localvars_strs, loopkernel_args_strs) = getArgsAndLocalVarsForLoopBody fortran prog
           allArgsStr = tabs ++ "ALL ARGS: "++  ( intercalate "," allArgs_strs ) ++"\n"
           origArgsStr = tabs ++ "ORIG ARGS USED IN LOOP BODY: "++ ( intercalate "," loopkernel_args_strs)++"\n"
           missingArgs_strs = filter (\arg -> not (arg `elem` allArgs_strs)) loopkernel_args_strs
           missingArgs = map (\arg -> (VarName nullAnno arg)) missingArgs_strs
           (localVarsStr,localDeclStrs,paramDeclStrs) = getLocalDeclStrs (allArgs++missingArgs++reductionVarNames) allKernelVars orig_decls_stmts originalLines tabs kernelName
           (missingArgsStr,missingArgDeclsStr) = getMissingArgDeclStrs missingArgs_strs originalLines tabs
           -- WV: declarations for _range and _rel
           range_rel_decls_str = generateRangeRelDecls l tabs --  we can do this because they *must* be integers                                                

           (readDecls', writtenDecls', readWriteDecls', ptrAssignments, allArgs_ptrAdaption') = 
               adaptForReadScalarDecls (allArgs++missingArgs) (generateKernelDeclarations prog (OpenCLReduce anno src r w l il rv fortran)) -- WV20170426
           -- WV: Remove the loop variables from the arguments
           loopVars = map (\(v,_,_,_) -> v) l
           allArgs_ptrAdaption = listSubtract allArgs_ptrAdaption' loopVars
           readDecls = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem`  loopVars ) ) readDecls'
           writtenDecls = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem`  loopVars ) ) writtenDecls'
           readWriteDecls = filter (\elt -> not ((extractAssigneeFromDecl elt) `elem`  loopVars ) ) readWriteDecls'
           allArgs_ptrAdaptionStr = case allArgs_ptrAdaption of
                       [] -> ""
                       args -> foldl (\accum item -> accum ++ "," ++ varNameStr item) (varNameStr (head args)) (tail args)

           readDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" readDecls
           writtenDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" writtenDecls
           readWriteDeclStr = foldl (\accum item -> accum ++ synthesiseDecl tabs item) "" readWriteDecls
           
           local_reductionVars = map (generateLocalReductionVar) reductionVarNames
           local_reductionVarsInitStr = foldl (\accum (var, expr) -> accum ++ tabs ++ "local_" ++ varNameStr var ++ " = " ++ outputExprFormatting expr ++ "\n") "" rv
           local_reductionVarsDeclatation = map (\(red, local) -> stripDeclAttrs $ adaptOriginalDeclaration_varname red local prog) (zip reductionVarNames local_reductionVars)
           local_reductionVarsDeclatationStr = synthesiseDecls tabs local_reductionVarsDeclatation

           localChunkSize_GPU_assg = generateAssgCode 
                                       localChunkSize 
                                       (generateDivisionExpr
                                           (generateDivisionExpr 
                                               (generateGlobalWorkItemsExpr l)
                                               nthVar)
                                           nunitsVar)
           -- for CPU, NTH==1
           localChunkSize_CPU_assg = generateAssgCode 
                                       localChunkSize 
                                       (generateDivisionExpr
                                               (generateGlobalWorkItemsExpr l)
                                           nunitsVar)
           localChunkSize_GPU_str = synthesiseAssg programInfo inTabs originalLines localChunkSize_GPU_assg
           localChunkSize_CPU_str = synthesiseAssg programInfo inTabs originalLines localChunkSize_CPU_assg

           startPosition_str = (outputExprFormatting startPosition) ++ " = " ++ (outputExprFormatting localChunkSize) ++
               " * " ++ (outputExprFormatting globalIdVar) ++ "\n"
           startPosition_str' =    (outputExprFormatting startPosition) ++ " = " ++ (outputExprFormatting chunk_size) ++ " * " ++ (outputExprFormatting groupIdVar)
           localChunkSize_str = outputExprFormatting localChunkSize

           reductionIterator = generateReductionIterator
           r_iter = generateVar reductionIterator
           idx_var = VarName nullAnno "idx"
           idx = generateVar idx_var
           local_id = generateVar (VarName nullAnno "local_id")
           chunk_size = generateVar (VarName nullAnno "chunk_size")
           -- (r_iter<ndrange)
           ndrange = generateVar (VarName nullAnno "ndrange")
           guard_cond = Bin nullAnno nullSrcSpan  (RelLT nullAnno) r_iter ndrange
           loopPaddingGuard = generateIfNoElse guard_cond workItem_loopCode
           r_iter_assignment = Assg nullAnno nullSrcSpan r_iter r_iter_assignment_rhs
           -- r_iter = start_position + local_id+NTH*idx
           r_iter_assignment_rhs = generateAdditionExpr startPosition (generateAdditionExpr local_id (generateProductExpr (generateVar (VarName nullAnno "NTH")) idx) )
           workItem_guarded_loop_body = FSeq  nullAnno nullSrcSpan r_iter_assignment loopPaddingGuard
           -- do idx=0,local_chunk_size-1 
           workItem_guarded_loop = generateLoop idx_var (generateIntConstant 0) workItem_guarded_loopEnd workItem_guarded_loop_body
           workItem_guarded_loopEnd = generateSubtractionExpr localChunkSize (generateIntConstant 1)
           workItem_loopEnd = generateSubtractionExpr (generateAdditionExpr startPosition localChunkSize) (generateIntConstant 1)
           workItem_loopCode = appendFortran_recursive workItem_reductionCode workItem_loopInitialiserCode 
           workItem_loop = generateLoop reductionIterator startPosition workItem_loopEnd workItem_loopCode
           workItem_reductionCode = applyGeneratedSrcSpans (replaceAllOccurences_varnamePairs fortran reductionVarNames local_reductionVars)

           workItem_loopInitialisers = generateLoopInitialisers l (generateVar reductionIterator) Nothing
           workItem_loopInitialiserCode = case workItem_loopInitialisers of
                                           [] -> error "synthesiseOpenCLReduce: workItem_loopInitialiserCode - empty list"
                                           _ -> foldl1 (\accum item -> appendFortran_recursive item accum) workItem_loopInitialisers

           workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
           workGroup_reductionArraysDecl = map (\x -> declareLocalReductionArray x (nthVar) prog) reductionVarNames
           workGroup_reductionArraysDeclStr = synthesiseDecls_Acc tabs workGroup_reductionArraysDecl localMemSpaceAcc
           workGroup_reductionArraysInitStr = foldl (generateReductionArrayAssignment tabs localIdFortranVar) "" (zip workGroup_reductionArrays local_reductionVars)
           workGroup_reductionCode = generateWorkGroupReduction reductionVarNames reductionIterator fortran
           workGroup_loop = generateLoop reductionIterator (generateIntConstant 1) nthVar workGroup_reductionCode

           global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
           global_reductionArraysAssignmentStr = foldl (generateReductionArrayAssignment tabs groupIdFortranVar) "" (zip global_reductionArrays local_reductionVars)
           
-- WV
removeIntent :: String -> String
removeIntent line = let
            chunks' = words line 
      in
        if length chunks' < 3 then line
        else
            let
                line' = unwords (init (init chunks')) -- stripped "::","var"
                var = last chunks'
                chunks = split ',' line'
                chunksNoIntent = filter (\chunk -> length chunk < 10 || take 6 chunk /= "intent") chunks
                lineNoIntent = (intercalate "," chunksNoIntent) ++ " :: "++var
--        chunksNoIntent' = map (\chunk -> if length chunk > 11 && take 9 chunk == "dimension" then init chunk else chunk) chunksNoIntent
            in
--        unwords chunksNoIntent'
                lineNoIntent

--                   if chunks !! 1 == "parameter" && (chunks !! 3) !! 0 == 'u' then error line  else
--                      chunks !! (length chunks - 2) == "::"
-- WV
matchVarNameInDecl :: String -> String -> Bool
matchVarNameInDecl var_name line = let 
        decl_var_name = last (words line) 
    in 
        decl_var_name == var_name
 
matchParameterDecl :: String -> Bool
matchParameterDecl line  = let
        chunks = words $ filter (/=',') line
    in
        "parameter" `elem` chunks
        
--    Function used during host code generation to produce call to OpenCL kernel.
synthesiseKernelCall :: (Program Anno, String) -> String -> Fortran Anno -> String
synthesiseKernelCall ([], _) _  _ = "DUMMY synthesiseKernelCall"
synthesiseKernelCall (progAst, filename) tabs (OpenCLMap anno src r w l il fortran) = (commentSeparator ("BEGIN " ++ kernelName)) -- WV20170426
                                                        ++ tabs ++ "oclGlobalRange = " ++ outputExprFormatting globalWorkItems ++ "\n"
                                                        ++ tabs ++ "oclLocalRange = 0\n" -- means NullRange
                                                        ++ tabs ++ (varNameStr statePtrVarName) ++ "(1) = " ++ stateName ++ "\n"
                                                        ++ tabs ++ bufferWrites ++ "\n"
                                                        ++ tabs ++ "call runOcl(oclGlobalRange,oclLocalRange,exectime)\n"
                                                        ++ tabs ++ "! call " ++ kernelName ++ "\n"
                                                        ++ bufferReads ++ "\n"
            where
                readArgs = map (varNameStr) (listSubtract r w)
                writtenArgs = map (varNameStr) (listSubtract w r)
                readWriteArgs = map (varNameStr) (listIntersection w r)

                allArguments = readArgs ++ writtenArgs ++ readWriteArgs
                allArgumentsStr = case allArguments of
                                    [] -> "NO ARGS"
                                    _ -> (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

                globalWorkItems = generateGlobalWorkItemsExpr l

                kernelName = generateKernelName "map" src
                stateName = generateStateName kernelName

                bufferWrites = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Write") (readDecls ++ readWriteDecls ++ [statePtrDecl]))
                bufferReads = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Read") (writtenDecls ++ readWriteDecls))

                (readDecls, writtenDecls, readWriteDecls) = generateKernelDeclarations progAst (OpenCLMap anno src r w l il fortran) -- WV20170426


{-
! ---- BEGIN press_reduce_103 -------------------------------------------------------------------------------------------------
              oclGlobalRange = (NTH * NUNITS) ! OK
              oclLocalRange = NTH ! OK
              ngroups = NUNITS ! OK
              state_ptr(1) = ST_PRESS_REDUCE_103 ! OK
              
              call oclWrite1DFloatArrayBuffer(global_sor_array_buf,global_sor_array_sz,global_sor_array) ! OK
              call oclWrite1DIntArrayBuffer(state_ptr_buf,state_ptr_sz,state_ptr) ! OK

              call runOcl(oclGlobalRange,oclLocalRange,exectime) ! OK
              ! call press_reduce_103 ! OK
              call oclRead1DFloatArrayBuffer(global_sor_array_buf,global_sor_array_sz,global_sor_array)
              call oclRead1DFloatArrayBuffer(global_sor_array_buf,global_sor_array_sz,global_sor_array)
              do r_iter=1, NUNITS
                  sor = (sor + global_sor_array(r_iter))
              end do

! ---- END --------------------------------------------------------------------------------------------------------------------
-}      
synthesiseKernelCall (progAst, filename) tabs (OpenCLReduce anno src r w l il rv fortran) = (commentSeparator ("BEGIN " ++ kernelName)) -- WV20170426
                                                            ++ tabs ++ "oclGlobalRange = " ++ outputExprFormatting reductionWorkItemsExpr ++ "\n"
                                                            ++ tabs ++ "oclLocalRange = " ++ outputExprFormatting nthVar ++ "\n"
-- UNUSED AND UNDECLARED!                                                            ++ tabs ++ "ngroups = " ++ outputExprFormatting nunitsVar ++ "\n"
                                                            ++ tabs ++ (varNameStr statePtrVarName) ++ "(1) = " ++ stateName ++ "\n"
                                                            ++ tabs ++ bufferWrites ++ "\n\n"
                                                            ++ tabs ++ "call runOcl(oclGlobalRange,oclLocalRange,exectime)\n"
                                                            ++ tabs ++ "! call " ++ kernelName++"\n"
                                                            ++ bufferReads -- WV clearly one of these is redundant
--                                                            ++ bufferReads_rv 
                                                            ++ "\n"
            where 

                reductionVarNames = map (\(varname, expr) -> varname) rv
                workGroup_reductionArrays = map (generateLocalReductionArray) reductionVarNames
                global_reductionArrays = map (generateGlobalReductionArray) reductionVarNames
                readArgs = listSubtract r w
                writtenArgs = listSubtract w r
                readWriteArgs = listIntersection w r

                allArguments =     (map (varNameStr) 
                                    (listSubtract 
                                        (readArgs ++ writtenArgs ++ readWriteArgs ++ global_reductionArrays) 
                                        reductionVarNames)) 
                allArgumentsStr = case allArguments of
                                    [] -> "NO ARGS"
                                    _ -> (head allArguments) ++ foldl (\accum item -> accum ++ "," ++ item) "" (tail allArguments)

                reductionWorkItemsExpr = generateProductExpr nthVar nunitsVar

                kernelName = generateKernelName "reduce" src 
                stateName = generateStateName kernelName

                bufferWrites = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Write") (readDecls ++ readWriteDecls ++ [statePtrDecl]))
                -- bufferReads = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Read") (writtenDecls ++ readWriteDecls))
                -- bufferReads_rv = foldl (\accum item -> accum ++ "\n" ++ item) "" (map (synthesiseBufferAccess tabs "Read") (global_reductionArraysDecls))
                -- make the declarations unique
                uniqueDecls = nub (writtenDecls ++ readWriteDecls ++ global_reductionArraysDecls)
                bufferReads = unlines (map (synthesiseBufferAccess tabs "Read") uniqueDecls)

                (readDecls, writtenDecls, readWriteDecls) = generateKernelDeclarations progAst (OpenCLReduce anno src r w l il rv fortran) -- WV20170426

                global_reductionArraysDecls = map (\x -> declareGlobalReductionArray x (nunitsVar) progAst) reductionVarNames

--    Produce code for a buffer read or write. If a scalar variable declaration is supplied, it is automatically converted to an array to be
--    used in the buffer access.
synthesiseBufferAccess :: String -> String -> Decl Anno -> String
synthesiseBufferAccess tabs method (Decl anno src lst typ) = case baseType of
                                                        Integer _ -> prefix ++ "Int" ++ suffix
                                                        Real _ -> prefix ++ "Float" ++ suffix
                                                        _ -> ""
            where
                assignee = extractAssigneeFromDecl (Decl anno src lst typ)
                varStr = varNameStr assignee
                varBufStr = varNameStr (varBufVarName assignee)

                baseType = extractBaseType typ
                declRank = getDeclRank (Decl anno src lst typ)

                isScalar = declRank == 0
                dimensions = if isScalar then 1 else declRank
                scalarPointerConversion = if isScalar then tabs ++ (varNameStr (scalarPointerVarName assignee)) ++ "(1) = " ++  varStr ++ "\n" else ""
                varSzStr = if isScalar then (varNameStr (varSizeVarName (scalarPointerVarName assignee))) else varNameStr (varSizeVarName assignee)
                bufferAccessSubjectVarStr = if isScalar then (varNameStr (scalarPointerVarName assignee)) else varStr
                

                prefix = scalarPointerConversion ++ tabs ++ "call ocl" ++ method ++ (show dimensions) ++ "D"
                suffix = "ArrayBuffer(" ++ varBufStr ++ "," ++ varSzStr ++ "," ++ bufferAccessSubjectVarStr ++ ")" ++ (if isScalar then "! Automatic conversion to array" else "")
synthesiseBufferAccess _ _ _ = error "synthesiseBufferAccess"

synthesiseBufferMake :: String -> Decl Anno -> String
synthesiseBufferMake tabs (Decl anno src lst typ) = case baseType of
                                                        Integer _ -> prefix ++ "Int" ++ suffix
                                                        Real _ -> prefix ++ "Float" ++ suffix
                                                        _ -> ""
            where
                assignee = extractAssigneeFromDecl (Decl anno src lst typ)
                varStr = varNameStr assignee
                varBufStr = varNameStr (varBufVarName assignee)

                baseType = extractBaseType typ
                declRank = getDeclRank (Decl anno src lst typ)

                isScalar = declRank == 0
                dimensions = if isScalar then 1 else declRank
                scalarPointerConversion = -- if isScalar then tabs ++ (varNameStr (scalarPointerVarName assignee)) ++ "(1) = " ++  varStr ++ "\n" else 
                                            ""
                varSzStr = if isScalar then (varNameStr (varSizeVarName (scalarPointerVarName assignee))) else varNameStr (varSizeVarName assignee)
                bufferAccessSubjectVarStr = if isScalar then (varNameStr (scalarPointerVarName assignee)) else varStr
                

                prefix = scalarPointerConversion ++ tabs ++ "call oclMake" ++ (show dimensions) ++ "D"
                suffix = "ArrayReadWriteBuffer(" ++ varBufStr ++ "," ++ varSzStr ++ "," ++ bufferAccessSubjectVarStr ++ ")" ++ (if isScalar then "! Automatic conversion to array" else "")
synthesiseBufferMake _ _ = error "synthesiseBufferMake"


