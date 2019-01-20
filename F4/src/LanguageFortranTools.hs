module LanguageFortranTools where

--    This module contains a set of functions that are used all over the source for the compiler. Essentially a utility module.

import           Data.Char
import           Data.Generics           (Data, Typeable, everything,
                                          everywhere, gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.List.Split         as LS
import qualified Data.Map                as DMap
import           Data.Maybe
import           Data.Typeable
import           Debug.Trace             (traceId)
import           Language.Fortran
import           Language.Fortran.Parser (parse)
import           System.Directory
import           System.Process
import           Text.Read
import           Warning                 (warning)

import           F95IntrinsicFunctions   (f95IntrinsicFunctions)
import           PreProcessor            (preProcess, removeBlankLines)

type ModuleVarsTable = DMap.Map String String
--  the Int is a label in the source code, to be replaced with the stashed code
type CodeStash = DMap.Map Int [String]
type Anno = DMap.Map (String) [String]

--    Type used when determining allowed values for iterator variables. Holds the currently chosen values
--    of nested loops whose bounds depends on previous iterator variables.
--    Also used during constant folding to hold current constants
type ValueTable = DMap.Map String (Float, BaseType Anno)

nullAnno :: Anno
nullAnno = DMap.empty

nullSrcSpan = (NoSrcLoc, NoSrcLoc)


--    Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile :: [String] -> [String] -> Bool -> String -> String -> IO ( (Program Anno, [String]) , (String, CodeStash), ModuleVarsTable)
parseFile cppDArgs cppXArgs fixedForm dir filename = do
    (preproc_inp, stash,moduleVarTable) <- preProcessingHelper cppDArgs cppXArgs fixedForm True dir filename
    let
        preproc_inp_lines = lines preproc_inp
        path = dir ++ "/" ++ filename
    return ((warning (parse preproc_inp) ("Parsing " ++ path), preproc_inp_lines),(filename, stash),moduleVarTable)
    --return (parse  preproc_inp, preproc_inp_lines) -- ,(filename, stash))
    -- return ()

runCpp :: [String] -> [String] -> Bool -> String -> String -> IO String
runCpp cppDArgs cppXArgs fixedForm dir filename = do
    (preproc_inp, _, _) <- preProcessingHelper cppDArgs cppXArgs fixedForm False dir filename
    return preproc_inp

preProcessingHelper :: [String] -> [String] -> Bool -> Bool -> String -> String -> IO (String, CodeStash, ModuleVarsTable)
preProcessingHelper cppDArgs cppXArgs fixedForm inlineModules dir filename = do
    let
        filePrefix = case dir of
            "" -> "./"
            _  -> dir ++ "/"
        dFlagList = if length cppDArgs > 0
            then foldl (\accum item -> accum ++ ["-D"++item]) [] cppDArgs
            else  []
        dFlagStr = if length dFlagList == 0
            then  ""
            else unwords dFlagList
    inp <- readFile (filePrefix ++ filename)

    let
        filename_no_dot
            | head filename == '.' = tail $ tail filename
            | otherwise = filename
        filename_noext = head $ split '.' filename_no_dot

    -- writeFile (filePrefix ++ filename_noext ++ "_just_read.f95") inp

    let
        contentLines = lines inp
    let
    -- Then we split out var decls into one per line
        inp' = unlines contentLines -- $ oneVarDeclPerVarDeclLine contentLines

    -- Then we preprocess, which should remove blank lines
        (preproc_inp, stash) = preProcess fixedForm cppXArgs inp'
    -- Write this out to a file so we can call cpp on it via the shell
    let tempFileName = (filePrefix ++ filename_noext ++ "_tmp.f95")
    writeFile  preproc_inp
    -- Apply the C preprocessor on the temporary file and remove the blank lines
    let cpp_cmd = "cpp -Wno-invalid-pp-token -P "++dFlagStr++ " " ++ filePrefix ++filename_noext++"_tmp.f95 | grep -v -E '^\\s*$' "
    -- putStrLn cpp_cmd
    putStrLn cpp_cmd
    preproc_inp' <- readCreateProcess (shell cpp_cmd) ""

    removeFile
    -- writeFile (filePrefix ++ filename_noext ++ "_cpp_output.f95") preproc_inp'
    let
    -- Remove all comments
    -- Language.Fortran.Parser borks on lines starting with !# and on trailing comments
        exp_inp_lines_no_comments = map (takeWhile (/= '!')) (lines preproc_inp')
    -- I also skip any blank lines
        exp_inp_lines'' = removeBlankLines exp_inp_lines_no_comments

    -- writeFile (filePrefix ++ filename_noext ++ "_blank_lines_removed.f95") $ unlines exp_inp_lines''
        -- exp_inp_lines'' = filter ( /= "") exp_inp_lines_no_comments
    -- First declarations from used modules are inlined. Why first? Surely it would be better to do that *after* running CPP?
    (exp_inp_lines',moduleVarTable) <- inlineDeclsFromUsedModules dir True exp_inp_lines'' cppDArgs cppXArgs fixedForm -- FIXME: should this not be inlineModules instead of True?
    -- writeFile (filePrefix ++ filename_noext ++ "_inline_decls.f95") $ unlines exp_inp_lines'
    let
        preproc_inp'' = unlines exp_inp_lines'
    -- writeFile (filePrefix ++ filename_noext ++ "_after_inling.f95") preproc_inp''
    return (preproc_inp'', stash,moduleVarTable)



--    Used by analyseLoop_map to format the information on the position of a particular piece of code that is used as the information
--    output to the user
errorLocationFormatting :: SrcSpan -> String
errorLocationFormatting ((SrcLoc filename line column), srcEnd) = show line ++ ":" ++ show column --"line " ++ show line ++ ", column " ++ show column

errorLocationRangeFormatting :: SrcSpan -> String
errorLocationRangeFormatting ((SrcLoc _ line_start _), (SrcLoc _ line_end _)) = "line " ++ show line_start ++ " and line " ++ show line_end -- ++ ", column " ++ show column

outputExprFormatting :: Expr Anno -> String
outputExprFormatting (Var _ _ list) = foldl (++) "" (map (\(varname, exprList) -> ((\(VarName _ str) -> str) varname) ++
                                                            (if exprList /= [] then "(" ++ (foldl (\accum item -> (if accum /= "" then accum ++ "," else "")
                                                                ++ item) "" (map (outputExprFormatting) exprList)) ++ ")" else "")) list)
outputExprFormatting (Con _ _ str) = if takeLast 2 str == ".0" then take ((length str) - 2) str else str
outputExprFormatting (Bin _ _ op expr1 expr2) = "(" ++ outputExprFormatting expr1 ++ " " ++ op_str ++ " " ++ outputExprFormatting expr2 ++ ")"
                            where
                                op_str = case op of
                                    Plus p   -> "+"
                                    Minus p  -> "-"
                                    Mul p    -> "*"
                                    Div p    -> "/"
                                    Or p     -> ".OR."
                                    And p    -> ".AND."
                                    Concat p -> "//"
                                    Power p  -> "**"
                                    RelEQ p  -> "=="
                                    RelNE p  -> "/="
                                    RelLT p  -> "<"
                                    RelLE p  -> "<="
                                    RelGT p  -> ">"
                                    RelGE p  -> ">="
outputExprFormatting (NullExpr _ _) = ""
outputExprFormatting (Null _ _) = "null"
outputExprFormatting (Unary _ _ unOp expr) = "(" ++ op_str ++ outputExprFormatting expr ++ ")"
                            where
                                op_str = case unOp of
                                    UMinus p -> "-"
                                    Not p    -> ".NOT."
outputExprFormatting codeSeg = show codeSeg

takeLast :: Int -> [a] -> [a]
takeLast n lst = reverse (take n (reverse lst))

orElem :: Eq a => a -> [[a]] -> Bool
orElem item []                = False
orElem item (firstList:lists) = elem item firstList || orElem item lists

listExtractSingleAppearances :: Eq a => [a] -> [a]
listExtractSingleAppearances list = listExtractSingleAppearances' list list

listExtractSingleAppearances' :: Eq a => [a] -> [a] -> [a]
listExtractSingleAppearances' (x:[]) wholeList = if appearances == 1 then [x] else []
        where
            appearances = listCountAppearances x wholeList
listExtractSingleAppearances' (x:tailList) wholeList = if appearances == 1 then [x] ++ listExtractSingleAppearances' tailList wholeList else listExtractSingleAppearances' tailList wholeList
        where
            appearances = listCountAppearances x wholeList


-- WV: this seems rather roundabout. Why not say
-- listCountAppearances value xs = length (filter (==value) xs)
listCountAppearances :: Eq a => a -> [a] -> Int
listCountAppearances value (x:[])     |     value == x = 1
                                    |     otherwise = 0
listCountAppearances value (x:list)     |     value == x = 1 + listCountAppearances value list
                                    |     otherwise = 0 + listCountAppearances value list

--    Generic function that removes all duplicate elements from a list.
listRemoveDuplications :: Eq a => [a] -> [a]
listRemoveDuplications a = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) [] a

listConcatUnique :: Eq a => [a] -> [a] -> [a]
listConcatUnique a b = foldl (\accum item -> if notElem item accum then accum ++ [item] else accum) b a

--    Used by SYB query to extract expressions
extractExpr :: Expr Anno -> Expr Anno
extractExpr expr = expr

extractExpr_list :: Expr Anno -> [Expr Anno]
extractExpr_list (ESeq _ _ _ _) = []
extractExpr_list expr           = [expr]

extractArgName :: ArgName Anno -> [ArgName Anno]
extractArgName codeSeg = case codeSeg of
                            ArgName _ _ -> [codeSeg]
                            _           -> []

extractReductionVarNames :: Fortran Anno -> [VarName Anno]
extractReductionVarNames (OpenCLReduce _ _ _ _ _ _ redVars _) = map (fst) redVars -- WV20170426
extractReductionVarNames _ = []

extractOpenCLReduces ast = everything (++) (mkQ [] (extractOpenCLReduces')) ast

extractOpenCLReduces' :: Fortran Anno -> [Fortran Anno]
extractOpenCLReduces' codeSeg = case codeSeg of
                            OpenCLReduce _ _ _ _ _ _ _ _ -> [codeSeg] -- WV20170426
                            _                            -> []

extractLoopIters :: Fortran Anno -> [String]
extractLoopIters  ast = everything (++) (mkQ [] (extractLoopIters')) ast

extractLoopIters' :: Fortran Anno -> [String]
extractLoopIters' codeSeg = case codeSeg of
                            For _ _ (VarName _ idx) _ _ _ _ -> [idx]
                            _                               -> []

extractLoops  ast = everything (++) (mkQ [] (extractLoops')) ast

extractLoops' :: Fortran Anno -> [Fortran Anno]
extractLoops' codeSeg = case codeSeg of
                            For _ _ _ _ _ _ _ -> [codeSeg]
                            _                 -> []

-- extractKernels :: Program Anno -> [Fortran Anno]
extractKernels ast = everything (++) (mkQ [] (extractKernels')) ast

extractKernels' :: Fortran Anno -> [Fortran Anno]
extractKernels' codeSeg = case codeSeg of
                            OpenCLMap _ _ _ _ _ _ _      -> [codeSeg] -- WV20170426
                            OpenCLReduce _ _ _ _ _ _ _ _ -> [codeSeg] -- WV20170426
                            _                            -> []


getWrittenArgs  ast = everything (++) (mkQ [] (getWrittenArgs')) ast

getWrittenArgs' codeSeg = case codeSeg of
                            OpenCLMap _ _ _ vws _ _ _      -> vws
                            OpenCLReduce _ _ _ vws _ _ _ _ -> vws
                            _                              -> []



getReadArgs  ast = everything (++) (mkQ [] (getReadArgs')) ast

getReadArgs' codeSeg = case codeSeg of
                            OpenCLMap _ _ vrs vws _ _ _      -> vrs
                            OpenCLReduce _ _ vrs vws _ _ _ _ -> vrs
                            _                                -> []

extractBufferWrites ast = everything (++) (mkQ [] (extractBufferWrites')) ast

extractBufferWrites' :: Fortran Anno -> [Fortran Anno]
extractBufferWrites' codeSeg = case codeSeg of
                            OpenCLBufferWrite _ _ _ -> [codeSeg]
                            _                       -> []

extractBufferReads ast = everything (++) (mkQ [] (extractBufferReads')) ast

extractBufferReads' :: Fortran Anno -> [Fortran Anno]
extractBufferReads' codeSeg = case codeSeg of
                            OpenCLBufferRead _ _ _ -> [codeSeg]
                            _                      -> []

--    Used to break down a tree of expressions that might form a calculation into a list of expressions for analysis.
extractOperands :: (Typeable p, Data p) => Expr p -> [Expr p]
extractOperands (Bin _ _ _ expr1 expr2) = extractOperands expr1 ++ extractOperands expr2
extractOperands (Unary _ _ _ expr) = [expr]
extractOperands (ESeq _ _ expr1 expr2) = extractOperands expr1 ++ extractOperands expr2
extractOperands (Bound _ _ expr1 expr2) = extractOperands expr1 ++ extractOperands expr2
extractOperands (ArrayCon _ _ es) = es
-- extractOperands (Var _ _ varlst) = foldl (++) [] (map (\(vm, es) -> (foldl (++) [] (map extractOperands es))) varlst)
extractOperands expr = [expr]


--    Used to extract the name of a Var in a particular expression
--    WV: So if the Expr is not a VarName it returns an empty list. Otherwise it returns a list which I think only ever has 1 elt
-- extractVarNames :: (Typeable p, Data p) => Expr p -> [VarName p]
extractVarNames :: Expr Anno -> [VarName Anno]
extractVarNames (Var _ _ lst) = let
        vs' = filter (\((VarName _ v),args) -> not (v `elem` f95IntrinsicFunctions && length args > 0) ) lst
        vs'' = map (\(x, _) -> x) vs'
    in
        vs'' -- if length vs'' == 0 then [VarName nullAnno "DUMMY"] else vs''
extractVarNames _ = []

extractMaybeVarNames :: Expr Anno -> Maybe [VarName Anno]
extractMaybeVarNames (Var _ _ lst) = let
        vs' = filter (\((VarName _ v),args) -> not (v `elem` f95IntrinsicFunctions && length args > 0) ) lst
        vs'' = map (\(x, _) -> x) vs'
    in
        Just vs'' -- if length vs'' == 0 then [VarName nullAnno "DUMMY"] else vs''
extractMaybeVarNames _ = Nothing

extractAllVarNames ::(Data (a Anno)) => a Anno -> [VarName Anno]
-- extractAllVarNames :: Expr Anno -> [VarName Anno]
extractAllVarNames = everything (++) (mkQ [] (extractVarNames))


-- Used to extract array index expressions and function call arguments.
extractContainedVars :: (Typeable p, Data p) => Expr p -> [Expr p]
extractContainedVars (Var _ _ lst) = foldl (\accumExprs (itemVar, itemExprs) -> accumExprs ++ itemExprs) [] lst
extractContainedVars _ = []
-- WV
extractContainedVarsWV :: (Typeable p, Data p) => Expr p -> [Expr p]
extractContainedVarsWV expr = case expr of
    (Var _ _ lst) ->
        let
            exprs = foldl (\accumExprs (itemVar, itemExprs) -> accumExprs ++ itemExprs) [] lst
        in
            foldl (++) exprs (map extractContainedVarsWV exprs)
    _ -> []



extractContainedOperands :: (Typeable p, Data p) => Expr p -> [Expr p]
extractContainedOperands expr =  foldl (\accum item -> accum ++ (extractOperands item)) [] containedVars
                where
                    containedVars = extractContainedVars expr

extractAssignments :: Fortran Anno -> [Fortran Anno]
extractAssignments codeSeg = case codeSeg of
                                Assg _ _ _ _ -> [codeSeg]
                                _            -> []

extractFortran :: Fortran Anno -> [Fortran Anno]
extractFortran fort = [fort]

extractDecl :: Decl Anno -> [Decl Anno]
extractDecl decl = [decl]

extractAssigneeFromDecl :: Decl Anno -> VarName Anno
extractAssigneeFromDecl (Decl anno src lst typ) = head (extractVarNames assignee)
            where
                assignee = (\(x, _, _) -> x) (head lst)
extractAssigneeFromDecl    _ = error "extractAssigneeFromDecl"

extractBaseType :: Type Anno -> BaseType Anno
extractBaseType (BaseType _ bt _ _ _) = bt
extractBaseType (ArrayT _ _ bt _ _ _) = bt

extractBlock :: Block Anno -> [Block Anno]
extractBlock block = [block]

extractVarNames_loopVars :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> [VarName Anno]
extractVarNames_loopVars = map (\(x,_,_,_) -> x)

--    Generates a SrcSpan that is attached to nodes that have been generated by this program
-- nullSrcSpan :: SrcSpan
-- nullSrcSpan = (nullSrcLoc, nullSrcLoc)

-- nullSrcLoc :: SrcLoc
-- nullSrcLoc = SrcLoc {srcFilename = "generated", srcLine = -1, srcColumn = -1}

generateSrcSpan :: String -> SrcSpan -> SrcSpan
generateSrcSpan [] ((SrcLoc sFile sLine sCol), (SrcLoc eFile eLine eCol)) = (SrcLoc {srcFilename = "generated", srcLine = sLine, srcColumn = sCol}, SrcLoc {srcFilename = "generated", srcLine = eLine, srcColumn = eCol})
generateSrcSpan filename ((SrcLoc sFile sLine sCol), (SrcLoc eFile eLine eCol)) = (SrcLoc {srcFilename = filename, srcLine = sLine, srcColumn = sCol}, SrcLoc {srcFilename = filename, srcLine = eLine, srcColumn = eCol})

applySrcSpan :: SrcSpan -> Fortran Anno -> Fortran Anno
applySrcSpan src (OpenCLMap anno _ read written loopvs iterloopvs fortran) = OpenCLMap anno src read written loopvs iterloopvs fortran -- WV20170426
applySrcSpan src (OpenCLReduce anno _ read written loopvs iterloopvs rvs fortran) = OpenCLReduce anno src read written loopvs iterloopvs rvs fortran -- WV20170426
applySrcSpan src _ = error "applySrcSpan: unnsupported AST node"

applyGlobalSrcSpan :: (Data (a Anno)) => SrcSpan -> a Anno -> a Anno
applyGlobalSrcSpan srcSpan codeSeg = everywhere (mkT (replaceSrSpan srcSpan)) codeSeg

shiftSrcSpanLineGlobal :: (Data (a Anno)) => Int -> a Anno -> a Anno
shiftSrcSpanLineGlobal inc codeSeg =  everywhere (mkT (shiftSrcSpan inc)) codeSeg

shiftSrcSpan :: Int -> SrcSpan -> SrcSpan
shiftSrcSpan inc ((SrcLoc fs ls cs), (SrcLoc fe le ce)) = ((SrcLoc fs (ls + inc) cs), (SrcLoc fe (le + inc) ce))

stretchSrcSpanLine :: (Data (a Anno)) => Int -> a Anno -> a Anno
stretchSrcSpanLine inc codeSeg =  gmapT (mkT (stretchSrc inc)) codeSeg

stretchSrc :: Int -> SrcSpan -> SrcSpan
stretchSrc inc (srcLoc, (SrcLoc fe le ce)) = (srcLoc, (SrcLoc fe (le + inc) ce))
-- stretchSrc inc (srcLoc, (SrcLoc fe le ce)) = (srcLoc, (SrcLoc fe (le + inc) ce))

srcSpanLineCount :: SrcSpan -> Int
srcSpanLineCount ((SrcLoc fs ls cs), (SrcLoc fe le ce)) = le - ls

normaliseSrcSpan correctSpanned badSpanned = shiftSrcSpanLineGlobal lineDifference badSpanned
        where
            ((SrcLoc _ correctLine _), _) = srcSpan correctSpanned
            ((SrcLoc _ badLine _), _) = srcSpan badSpanned
            lineDifference = correctLine - badLine

--    Used to standardise SrcSpans so that nodes of an AST may be matched up even if they appear in completely different
--    parts of a program. Also used to signify that a node has been changed and cannot be copied from the orignal source during code
--    generation
applyGeneratedSrcSpans :: (Data (a Anno)) => a Anno -> a Anno
applyGeneratedSrcSpans = everywhere (mkT (standardiseSrcSpan))

replaceSrSpan :: SrcSpan -> SrcSpan -> SrcSpan
replaceSrSpan input current = input

standardiseSrcSpan :: SrcSpan -> SrcSpan
standardiseSrcSpan src = nullSrcSpan

srcSpanInSrcSpanRange :: SrcSpan -> SrcSpan -> SrcSpan -> Bool
srcSpanInSrcSpanRange start finish inside = srcSpanInSrcSpan (fst start, snd finish) inside

srcSpanInSrcSpan :: SrcSpan -> SrcSpan -> Bool
srcSpanInSrcSpan ((SrcLoc _ rSLine rSCol), (SrcLoc _ rELine rECol)) ((SrcLoc _ srcSLine srcSCol), (SrcLoc _ srcELine srcECol)) = startAfterRange && endsBeforeRange
        where
            startAfterRange = srcSLine > rSLine || srcSLine == rSLine && srcSCol >= rSCol
            endsBeforeRange = srcELine < rELine || srcELine == rELine && srcECol <= rECol

generateAssgCode :: Expr Anno -> Expr Anno -> Fortran Anno
generateAssgCode expr1 expr2 = Assg nullAnno nullSrcSpan expr1 expr2

generateLTExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateLTExpr expr1 expr2 = Bin nullAnno nullSrcSpan (RelLT nullAnno) expr1 expr2

generateAndExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateAndExpr expr1 expr2 = Bin nullAnno nullSrcSpan (And nullAnno) expr1 expr2

generateAndExprFromList :: [Expr Anno] -> Expr Anno
generateAndExprFromList list = foldl1 (generateAndExpr) list

generateVar :: VarName Anno -> Expr Anno
generateVar varname = Var nullAnno nullSrcSpan [(varname, [])]

generateArrayVar :: VarName Anno -> [Expr Anno] -> Expr Anno
generateArrayVar varname exprs = Var nullAnno nullSrcSpan [(varname, exprs)]

generateConstant :: String -> Expr Anno
generateConstant value = Con nullAnno nullSrcSpan value

-- generateIntConstant :: Int -> Expr Anno
-- generateIntConstant value = Con nullAnno nullSrcSpan (show value)

-- generateFloatConstant :: Float -> Expr Anno
-- generateFloatConstant value = Con nullAnno nullSrcSpan (show value)

-- generateArrayVar :: VarName Anno -> Expr Anno -> Expr Anno
-- generateArrayVar varname access = Var nullAnno nullSrcSpan [(varname, [access])]

generateIf :: Expr Anno -> Fortran Anno -> Fortran Anno
generateIf expr fortran = If nullAnno nullSrcSpan expr fortran [] Nothing

generateAdditionExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateAdditionExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Plus nullAnno) expr1 expr2

generateProductExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateProductExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Mul nullAnno) expr1 expr2

generateSubtractionExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateSubtractionExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Minus nullAnno) expr1 expr2

generateDivisionExpr :: Expr Anno -> Expr Anno -> Expr Anno
generateDivisionExpr expr1 expr2 = Bin nullAnno nullSrcSpan (Div nullAnno) expr1 expr2

getUses :: Uses Anno -> [Uses Anno]
getUses uses = case uses of
                    (Use _ _ _ _) -> [uses]
                    _             -> []

getSubNames :: SubName Anno -> [SubName Anno]
getSubNames sub = case sub of
                    SubName _ _ -> [sub]
                    _           -> []

getUnitName :: ProgUnit Anno -> String
getUnitName progunit = foldl (++) [] (gmapQ (mkQ [] getUnitName') progunit)

getUnitName' :: SubName Anno -> String
getUnitName' (SubName _ str) = str
getUnitName' _               = ""

--     Function returns the loop variable for an AST representing a for loop
getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _                     = Nothing

hasOperand :: Expr Anno -> Expr Anno -> Bool
hasOperand container contains = all (== True) $ map (\x -> elem x (extractOperands $ applyGeneratedSrcSpans container)) (extractOperands $ applyGeneratedSrcSpans contains)

--    Appends a new item to the list of annotations already associated to a particular node
appendAnnotation :: Fortran Anno -> String -> String -> Fortran Anno
appendAnnotation original key appendage = case original of
        For anno f2 f3 f4 f5 f6 f7 -> For (appendToMap key appendage anno) f2 f3 f4 f5 f6 f7
        OpenCLMap anno f2 f3 f4 f5 f6 f7 -> OpenCLMap (appendToMap key appendage anno) f2 f3 f4 f5 f6 f7 -- WV20170426
        OpenCLReduce anno f2 f3 f4 f5 f6 f7 f8 -> OpenCLReduce (appendToMap key appendage anno) f2 f3 f4 f5 f6 f7 f8 -- WV20170426
        _ -> original

        -- appendToMap

appendAnnotationList :: Fortran Anno -> String -> [String] -> Fortran Anno
appendAnnotationList original key appendage = foldl (\accum item -> appendAnnotation accum key item) original appendage

appendAnnotationMap :: Fortran Anno -> Anno -> Fortran Anno
appendAnnotationMap codeSeg newMap = case codeSeg of
        For anno f2 f3 f4 f5 f6 f7 -> For (combineAnnotations newMap anno) f2 f3 f4 f5 f6 f7
        OpenCLMap anno f2 f3 f4 f5 f6 f7 -> OpenCLMap (combineAnnotations newMap anno) f2 f3 f4 f5 f6 f7 -- WV20170426
        OpenCLReduce anno f2 f3 f4 f5 f6 f7 f8 -> OpenCLReduce (combineAnnotations newMap anno) f2 f3 f4 f5 f6 f7 f8 -- WV20170426

-- appendAnnotation original appendage = original

--    Prepends a new item to the list of annotations already associated to a particular node
-- prependAnnotation :: Fortran Anno -> String -> Fortran Anno
-- prependAnnotation original appendage = case original of
--         _ -> original

removeAllAnnotations original = everywhere (mkT removeAnnotations) original

removeAnnotations :: Fortran Anno -> Fortran Anno
removeAnnotations original = case original of
        For anno f2 f3 f4 f5 f6 f7 -> For nullAnno f2 f3 f4 f5 f6 f7
        OpenCLMap anno f2 f3 f4 f5 f6 f7 -> OpenCLMap nullAnno f2 f3 f4 f5 f6 f7  -- WV20170426
        OpenCLReduce anno f2 f3 f4 f5 f6 f7 f8 -> OpenCLReduce nullAnno f2 f3 f4 f5 f6 f7 f8 -- WV20170426
        _ -> original

combineAnnotations :: Anno -> Anno -> Anno
combineAnnotations a b = combineMaps a b

-- WV This checks if one of the varNames is used in the expression, where the expression can be a variable or a binary operation.
-- WV So unary operations are ignored, but worse, function calls too, and it is not clear to me why array indices are ignored
{-
data Expr  p =
             | Var p SrcSpan  [(VarName p, [Expr p])]
             | Bin p SrcSpan  (BinOp p) (Expr p) (Expr p)
             | Unary p SrcSpan (UnaryOp p) (Expr p)
             | ESeq p SrcSpan (Expr p) (Expr p)
             | AssgExpr p SrcSpan Variable (Expr p)
-}
usesVarName_list :: [VarName Anno] -> Expr Anno -> Bool
usesVarName_list varNames (Var _ _ list) = foldl (||) False $ map (\(varname, exprs) -> elem varname varNames) list -- so any variable used as an array index or argument of a function call is ignored!
usesVarName_list varNames (Bin _ _ _ expr1 expr2) = (usesVarName_list varNames expr1) || (usesVarName_list varNames expr2)
usesVarName_list varNames _ = False

usesVarName :: VarName Anno -> Expr Anno -> Bool
usesVarName varnameInp (Var _ _ list) = foldl (||) False $ map (\(varname, exprs) -> varname == varnameInp) list

isVar :: Expr Anno -> Bool
isVar (Var _ _ _) = True
isVar _           = False

extractLoopVars :: Fortran Anno -> [VarName Anno]
extractLoopVars codeSeg = everything (++) (mkQ [] extractLoopVars') codeSeg

extractLoopVars' :: Fortran Anno -> [VarName Anno]
extractLoopVars' (For _ _ var _ _ _ _) = [var]
extractLoopVars' _                     = []

extractUsedVarName :: Expr Anno -> [VarName Anno]
extractUsedVarName (Var _ _ list) = map (\(varname, exprs) -> varname) list
extractUsedVarName _              = []

replaceAllOccurences_varnamePairs :: Fortran Anno -> [VarName Anno] -> [VarName Anno] -> Fortran Anno
replaceAllOccurences_varnamePairs codeSeg originals replacements = foldl (\accum (v1, v2) -> replaceAllOccurences_varname accum v1 v2) codeSeg pairs
                    where
                        pairs = zip originals replacements

replaceAllOccurences_varname :: (Data (a Anno)) => a Anno -> VarName Anno -> VarName Anno -> a Anno
replaceAllOccurences_varname codeSeg original replacement = everywhere (mkT (replaceVarname original replacement)) codeSeg

replaceVarname :: VarName Anno -> VarName Anno -> VarName Anno -> VarName Anno
replaceVarname original replacement inp     |     original == inp = replacement
                                            |    otherwise = inp

varNameStr :: VarName Anno -> String
varNameStr (VarName _ str) = str

varNameListStr :: [VarName Anno] -> String
varNameListStr []         =  ""
varNameListStr (var:[])   = varNameStr var
varNameListStr (var:vars) = (varNameStr var) ++ "," ++ (varNameListStr vars)

replaceFortran progAst oldFortran newFortran = everywhere (mkT (replaceFortran' oldFortran newFortran)) progAst

replaceFortran' :: Fortran Anno -> Fortran Anno -> Fortran Anno -> Fortran Anno
replaceFortran' oldFortran newFortran currentFortran     |    (applyGeneratedSrcSpans oldFortran) == (applyGeneratedSrcSpans currentFortran) = normaliseSrcSpan currentFortran newFortran
                                                        |    otherwise = currentFortran

replaceProgUnit ast oldProgUnit newProgUnit = everywhere (mkT (replaceProgUnit' oldProgUnit newProgUnit)) ast

replaceProgUnit' :: ProgUnit Anno -> ProgUnit Anno -> ProgUnit Anno -> ProgUnit Anno
replaceProgUnit' oldProgUnit newProgUnit currentProgUnit     |     (applyGeneratedSrcSpans oldProgUnit) == (applyGeneratedSrcSpans currentProgUnit) = normaliseSrcSpan currentProgUnit newProgUnit
                                                            |    otherwise = currentProgUnit

--    Takes two ASTs and appends one onto the other so that the resulting AST is in the correct format
appendFortran_recursive :: Fortran Anno -> Fortran Anno -> Fortran Anno
appendFortran_recursive newFortran (FSeq anno1 src1 (NullStmt _ _) fortran2) = FSeq anno1 src1 fortran2 newFortran
appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 (NullStmt _ _)) = FSeq anno1 src1 fortran1 newFortran
appendFortran_recursive newFortran (FSeq anno1 src1 fortran1 fortran2) = FSeq anno1 src1 fortran1 (appendFortran_recursive newFortran fortran2)
appendFortran_recursive (NullStmt _ _) codeSeg = codeSeg
appendFortran_recursive newFortran (NullStmt _ _) = newFortran
appendFortran_recursive newFortran codeSeg = FSeq nullAnno nullSrcSpan codeSeg newFortran

--    Takes an AST and removes the loop statements from the node and joins up the rest of the code so that is it represented in the
--    format that language-fortran uses.
removeLoopConstructs_recursive :: Fortran Anno -> Fortran Anno
removeLoopConstructs_recursive (FSeq anno _ (For _ _ _ _ _ _ fortran1) fortran2) = removeLoopConstructs_recursive $ appendFortran_recursive fortran2 fortran1
removeLoopConstructs_recursive (For _ _ _ _ _ _ fortran) = removeLoopConstructs_recursive fortran
removeLoopConstructs_recursive (FSeq _ _ fortran (NullStmt _ _)) = removeLoopConstructs_recursive fortran
removeLoopConstructs_recursive codeSeg = codeSeg

extractFirstFortran codeSeg = head (everything (++) (mkQ [] extractFortran) codeSeg)

extractFirstChildFor :: Fortran Anno -> Maybe(Fortran Anno, Fortran Anno, Fortran Anno)
extractFirstChildFor (For _ _ _ _ _ _ fortran)     |    forFound = Just(priorFortran, firstFor, followingFortran)
                                                |    otherwise = Nothing
        where
            allFors = everything (++) (mkQ [] extractForWithFollowing) fortran
            forFound = case extractedFor of
                            Nothing -> False
                            Just a  -> True
            extractedFor = extractForWithFollowing_beta fortran
            (firstFor, followingFortran) = fromMaybe (error "extractFirstChildFor") extractedFor
            -- (firstFor, followingFortran) = head allFors
            priorFortran = extractPriorToFor fortran

extractForWithFollowing :: Fortran Anno -> [(Fortran Anno, Fortran Anno)]
extractForWithFollowing (FSeq _ _ fortran1 fortran2) = case fortran1 of
                                                            For _ _ _ _ _ _ _  -> [(fortran1, fortran2)]
                                                            _ -> []
extractForWithFollowing _ = []

extractForWithFollowing_beta :: Fortran Anno -> Maybe(Fortran Anno, Fortran Anno)
extractForWithFollowing_beta codeSeg = case codeSeg of
                                            FSeq _ _ fortran1 fortran2 -> case fortran1 of
                                                                                For _ _ _ _ _ _ _ -> Just(fortran1, fortran2)
                                                                                _ -> recursiveCheck_maybe
                                            _ -> recursiveCheck_maybe
        where
            recursiveCheck = (gmapQ (mkQ Nothing extractForWithFollowing_beta) codeSeg)
            recursiveCheck_maybe = case recursiveCheck of
                                        [] -> Nothing
                                        _  -> head recursiveCheck

extractPriorToFor :: Fortran Anno -> Fortran Anno
extractPriorToFor codeSeg = case codeSeg of
                                For _ _ _ _ _ _ _ -> NullStmt nullAnno nullSrcSpan
                                FSeq _ _ (For _ _ _ _ _ _ _) _ -> NullStmt nullAnno nullSrcSpan
                                _ -> gmapT (mkT extractPriorToFor) codeSeg

extractFor :: Fortran Anno -> [Fortran Anno]
extractFor codeSeg = case codeSeg of
                        For _ _ _ _ _ _ _ -> [codeSeg]
                        _                 -> []

extractLineNumber :: SrcSpan -> Int
extractLineNumber ((SrcLoc _ line _), _) = line

generateESeq :: [VarName Anno] -> Expr Anno
generateESeq (var:[]) = generateVar var
generateESeq (var:vars) = ESeq nullAnno nullSrcSpan (generateESeq vars) (generateVar var)

generateFSeq :: [Fortran Anno] -> Fortran Anno
generateFSeq [] = NullStmt nullAnno nullSrcSpan
generateFSeq (statement:[]) = statement
generateFSeq (statement:statements) = FSeq nullAnno nullSrcSpan statement (generateFSeq statements)

generateSrcSpanMerge :: SrcSpan -> SrcSpan -> SrcSpan
generateSrcSpanMerge src1 src2 = (src1_s, src2_e)
                    where
                        (src1_s, src1_e) = src1
                        (src2_s, src2_e) = src2

getSrcSpanNonIntersection :: SrcSpan -> SrcSpan -> (SrcSpan, SrcSpan)
getSrcSpanNonIntersection src1 src2 = (firstSrc, secondSrc)
                    where
                        (src1_s, src1_e) = src1
                        (src2_s, src2_e) = src2

                        firstSrc = (src1_s, src2_s)
                        secondSrc = (src2_e, src1_e)

getEarliestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getEarliestSrcSpan [] = Nothing
getEarliestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then item else accum) (spans!!0) spans)

getLatestSrcSpan :: [SrcSpan] -> Maybe(SrcSpan)
getLatestSrcSpan [] = Nothing
getLatestSrcSpan spans = Just (foldl (\accum item -> if checkSrcSpanBefore item accum then accum else item) (spans!!0) spans)

checkSrcLocEqualLines :: SrcLoc -> SrcLoc -> Bool
checkSrcLocEqualLines (SrcLoc _ l1 _) (SrcLoc _ l2 _) = l1 == l2

getEarliestSrcLoc :: [SrcLoc] -> Maybe(SrcLoc)
getEarliestSrcLoc [] = Nothing
getEarliestSrcLoc locs = Just (foldl (\accum item -> if checkSrcLocBefore item accum then item else accum) (locs!!0) locs)

checkSrcLocBefore :: SrcLoc -> SrcLoc -> Bool
checkSrcLocBefore (SrcLoc file_before line_before column_before) (SrcLoc file_after line_after column_after) =  (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))

checkSrcSpanAfter :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanAfter ((SrcLoc file_before line_before column_before), _) (_, (SrcLoc file_after line_after column_after)) = (line_before > line_after) || ((line_before == line_after) && (column_before > column_after))
-- checkSrcSpanAfter ((SrcLoc file_before line_before column_before), _) ((SrcLoc file_after line_after column_after), _) = (line_before > line_after) || ((line_before == line_after) && (column_before > column_after))

checkSrcSpanBefore :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore (_, (SrcLoc file_before line_before column_before)) ((SrcLoc file_after line_after column_after), _) = (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))
-- checkSrcSpanBefore ((SrcLoc file_before line_before column_before), _) ((SrcLoc file_after line_after column_after), _) = (line_before < line_after) || ((line_before == line_after) && (column_before < column_after))

checkSrcSpanBefore_line :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanBefore_line ((SrcLoc file_before line_before column_before), beforeEnd) ((SrcLoc file_after line_after column_after), afterEnd) = (line_before < line_after)

checkSrcSpanContainsSrcSpan :: SrcSpan -> SrcSpan -> Bool
checkSrcSpanContainsSrcSpan ((SrcLoc _ outerLS outerCS), (SrcLoc _ outerLE outerCE)) ((SrcLoc _ innerLS innerCS), (SrcLoc _ innerLE innerCE)) = outerStartsBefore && outerEndsAfter
        where
            outerStartsBefore = (outerLS < innerLS)|| (outerLS == innerLS && outerCS < innerCS)
            outerEndsAfter = (outerLE > innerLE)|| (outerLE == innerLE && outerCE > innerCE)

listCartesianProduct :: [a] -> [a] -> [(a,a)]
listCartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

--    Generic function that takes two lists a and b and returns a +list c that is all of the elements of a that do not appear in b.
--    WV: all elts of a that are not in b
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

listSubtractWithExemption :: Eq a => [a] -> [a] -> [a] -> [a]
listSubtractWithExemption exempt a b = filter (\x -> (elem x exempt) || (notElem x b)) a

listIntersection :: Eq a => [a] -> [a] -> [a]
listIntersection a b = filter (\x -> elem x b) a

listUnion :: Eq a => [a] -> [a] -> [a]
listUnion a b = nub (a++b)

combineMaps :: Ord k => DMap.Map k [a] -> DMap.Map k [a] -> DMap.Map k [a]
combineMaps map1 map2 = resultantAnalysis
                        where
                            map2List = DMap.toList map2
                            resultantAnalysis = foldl (\accum (key, value) -> DMap.insert key ((DMap.findWithDefault [] key accum) ++ value) accum) map1 map2List

appendToMap :: Ord k => k -> a -> DMap.Map k [a] -> DMap.Map k [a]
appendToMap key item map = DMap.insert key ((DMap.findWithDefault [] key map) ++ [item]) map

extractPrimaryReductionOp :: Expr Anno -> Expr Anno -> Maybe(BinOp Anno)
extractPrimaryReductionOp assignee (Bin _ _ op expr1 expr2) = case assigneePresent of
                                        True  -> Just op
                                        False -> childOp
                        where
                            primaryOp1 = extractPrimaryReductionOp assignee expr1
                            primaryOp2 = extractPrimaryReductionOp assignee expr2
                            childOp = case primaryOp1 of
                                        Just a  -> primaryOp1
                                        Nothing ->  primaryOp2
                            assigneePresent = applyGeneratedSrcSpans expr1 == applyGeneratedSrcSpans assignee ||
                                                applyGeneratedSrcSpans expr2 == applyGeneratedSrcSpans assignee
extractPrimaryReductionOp assignee assignment = Nothing

extractPrimaryReductionFunction ::  Expr Anno -> Expr Anno -> String
extractPrimaryReductionFunction assignee (Var _ _ list) = foldl assigneePresent "" standardisedList
                        where
                            assigneePresent = (\accum (var, exprList) -> if elem (applyGeneratedSrcSpans assignee) exprList then varNameStr var else accum)
                            standardisedList = map (\(var, exprList) -> (var, map (applyGeneratedSrcSpans) exprList)) list
extractPrimaryReductionFunction assignee expr = "" -- error ("Error: extractPrimaryReductionFunction\nType: " ++ (show $ typeOf expr) ++ "\nShow: " ++ (show expr))

-- evaluateRange_int :: ValueTable -> Expr Anno -> Expr Anno -> Expr Anno -> [Int]
-- evaluateRange_int vt startExpr endExpr stepExpr = map (round) (evaluateRange vt startExpr endExpr stepExpr)

evaluateRange :: ValueTable -> Expr Anno -> Expr Anno -> Expr Anno -> Maybe([Float])
evaluateRange vt startExpr endExpr stepExpr = range
        where
            startInt = evaluateExpr vt startExpr
            endInt = evaluateExpr vt endExpr
            stepInt = evaluateExpr vt stepExpr

            range = case startInt of
                        Nothing -> Nothing
                        Just start -> case endInt of
                                        Nothing -> Nothing
                                        Just end -> case stepInt of
                                                        Nothing -> Nothing
                                                        Just step -> Just (map (fromIntegral) [round start :: Int,(round start :: Int)+(round step :: Int) ..round end :: Int])
-- evaluateExpr_int :: ValueTable -> Expr Anno -> Maybe(Int)
-- evaluateExpr_int vt expr = case evaluateExpr vt expr of
--                                 Nothing -> Nothing
--                                 Just result -> Just (round result)

evaluateExpr :: ValueTable -> Expr Anno -> Maybe(Float)
evaluateExpr vt expr = extractEvaluatedValue (evaluateExpr_type vt expr)

extractEvaluatedValue :: Maybe(Float, BaseType Anno) -> Maybe(Float)
extractEvaluatedValue expr = case expr of
                            Nothing       -> Nothing
                            Just (val, _) -> Just val

extractEvaluatedType :: Maybe(Float, BaseType Anno) -> Maybe(BaseType Anno)
extractEvaluatedType expr = case expr of
                            Nothing       -> Nothing
                            Just (_, typ) -> Just typ

evaluateExpr_type :: ValueTable -> Expr Anno -> Maybe(Float, BaseType Anno)
evaluateExpr_type vt (Bin _ _ binOp expr1 expr2) = case binOp of
                                                Plus _ ->     maybeBinOp             expr1_eval expr2_eval (+)
                                                Minus _ ->     maybeBinOp             expr1_eval expr2_eval (-)
                                                Mul _ ->     maybeBinOp             expr1_eval expr2_eval (*)
                                                Div _ ->     case extractEvaluatedType expr1_eval of
                                                                Just (Real _) ->     maybeBinOp expr1_eval expr2_eval (/)
                                                                Just (Integer _) -> case extractEvaluatedType expr2_eval of
                                                                                        Just (Real _) ->     maybeBinOp expr1_eval expr2_eval (/)
                                                                                        Just (Integer _) -> maybeBinOp_integral expr1_eval expr2_eval (quot)
                                                                                        Nothing -> Nothing
                                                                Nothing -> Nothing
                                                Power _ ->     maybeBinOp_float expr1_eval expr2_eval (**) -- WV: was ^
                                                _ -> Nothing
            where
                expr1_eval = evaluateExpr_type vt expr1
                expr2_eval = evaluateExpr_type vt expr2
evaluateExpr_type vt (Unary _ _ unOp expr) = case unOp of
                                                UMinus _ -> maybeNegative (evaluateExpr_type vt expr)
                                                Not _ -> Nothing
evaluateExpr_type vt (Var p src lst)       | varString == "mod" = maybeBinOp_integral (evaluateExpr_type vt expr1) (evaluateExpr_type vt expr2) (mod)
                                        | otherwise = lookupValueTable_type varString vt--DMap.lookup varString vt
            where
                varString = varNameStr $ head $ extractUsedVarName (Var p src lst)
                headExprList = snd (head lst)
                expr1 = head headExprList
                expr2 = head $ tail headExprList
evaluateExpr_type _ (Con _ _ str)    |    head str == '.' = Just ((read $ tail str) :: Float, Real nullAnno)
                                    |    last str == '.' = Just ((read $ take (length str - 1) str) :: Float, Real nullAnno)
                                    |    elem '.' str = Just(read str :: Float, Real nullAnno)
                                    |    otherwise = Just(read str :: Float, Integer nullAnno)
evaluateExpr_type _ _ = Nothing

lookupValueTable :: String -> ValueTable -> Maybe(Float)
lookupValueTable str table = value
            where
                (value, typ) =  case lookupValueTable_type str table of
                                    Nothing     -> (Nothing, Nothing)
                                    Just (v, t) -> (Just v, Just t)


lookupValueTableToConstantString :: String -> ValueTable -> Maybe String
lookupValueTableToConstantString str table = constString
            where
                constString = case tableValue of
                                (Just val, Just (Integer _)) -> Just $ ((show . round) val)
                                (Just val, Just (Real _   )) -> Just $ show val
                                _         -> Nothing -- "Error getting constant value"
                tableValue =  case lookupValueTable_type str table of
                                    Nothing     -> (Nothing, Nothing)
                                    Just (v, t) -> (Just v, Just t)

lookupValueTable_type :: String -> ValueTable -> Maybe(Float, BaseType Anno)
lookupValueTable_type str table =  DMap.lookup str table

addToValueTable :: VarName Anno -> Float -> ValueTable -> ValueTable
addToValueTable var value table = addToValueTable_type var value (Real nullAnno) table -- DMap.insert (varNameStr var) value table

addToValueTable_type :: VarName Anno -> Float -> BaseType Anno -> ValueTable -> ValueTable
addToValueTable_type var value typ table = DMap.insert (varNameStr var) (value, typ) table

deleteValueFromTable :: VarName Anno -> ValueTable -> ValueTable
deleteValueFromTable var table = DMap.delete (varNameStr var) table

-- maybeQuotOp :: Maybe(Float) -> Maybe(Float) -> Float
-- maybeQuotOp maybeFloat1 maybeFloat2 = case maybeFloat1 of
--                                             Nothing -> 0.0
--                                             Just float1 -> case maybeFloat2 of
--                                                             Nothing -> 0.0
--                                                             Just float2 -> fromIntegral (quot (round float1) (round float2)) :: Float

--maybeBinOp_float :: Floating a => Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno) -> (a -> a -> a) -> Maybe(Float, BaseType Anno)
maybeBinOp_float maybeFloat1 maybeFloat2 op = resultValue
            where
                resultValue = case maybeFloat1 of
                                            Nothing -> Nothing
                                            Just (float1, typ1) -> case maybeFloat2 of
                                                            Nothing -> Nothing
                                                            Just (float2, typ2) -> Just (op float1 float2, Integer nullAnno)

maybeBinOp_integral :: Integral a => Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno) -> (a -> a -> a) -> Maybe(Float, BaseType Anno)
maybeBinOp_integral maybeFloat1 maybeFloat2 op = resultValue
            where
                resultValue = case maybeFloat1 of
                                            Nothing -> Nothing
                                            Just (float1, typ1) -> case maybeFloat2 of
                                                            Nothing -> Nothing
                                                            Just (float2, typ2) -> Just(fromIntegral (op (round float1) (round float2)) :: Float, Integer nullAnno)

maybeBinOp :: Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno) -> (Float -> Float -> Float) -> Maybe(Float, BaseType Anno)
maybeBinOp maybeFloat1 maybeFloat2 op = case maybeFloat1 of
                                            Nothing -> Nothing
                                            Just (float1, typ1) -> case maybeFloat2 of
                                                            Nothing -> Nothing
                                                            Just (float2, typ2) -> Just(op float1 float2, resolveType typ1 typ2)

--    ASSUME USE OF ONLY TWO TYPES (INTEGER AND REAL) IN EXPRESSION EVALUATION
resolveType :: BaseType Anno -> BaseType Anno -> BaseType Anno
resolveType type1 type2 |     type1 == type2 = type1
                        |    otherwise = Real nullAnno

maybeNegative :: Maybe(Float, BaseType Anno) -> Maybe(Float, BaseType Anno)
maybeNegative (Just(int, typ)) = Just(-int, typ)
maybeNegative Nothing          = Nothing

trimFront :: String -> String
trimFront inp = filter (\x -> x /= ' ' && x /= '\t') inp

--    Value used as a global spacing measure. Used for output formatting.
outputTab :: String
outputTab = "  "

tabInc :: String
tabInc = "    "

compilerName :: String
compilerName = "ParallelFortran"

commentSeparator :: String -> String
commentSeparator str = prefix ++ (commentSeparator' comment body) ++ suffix
    where
        prefix = "! ----"
        suffix = "\n"
        body = (take 122 ['-','-'..'-'])
        comment = case str of
                    [] -> ""
                    _  -> (" " ++ str ++ " ")

commentSeparator' :: String -> String -> String
commentSeparator' [] (_:seps)       = seps
commentSeparator' comment []        = comment
commentSeparator' (x:xs) (sep:seps) = [x] ++ (commentSeparator' xs seps)

extractIndent :: String -> String
extractIndent (' ':str) = " " ++ extractIndent str
extractIndent _         = ""

-- WV
split :: Char -> [Char] -> [[Char]]
split delim str = map (\w -> map (\c -> if c==delim then ' ' else c) w) $ words (map (\c-> (if delim==c then ' ' else if c==' ' then delim else c)) str)


-- "integer,", "parameter", "::", "ip=150"
-- WV
findDeclLine :: String -> Bool
findDeclLine line  = Data.List.isInfixOf "::" line && head ( filter (/=' ') line) /= '!'
{-        let
            line_no_comments = head $ split '!' line
            -- chunks = words line_no_comments -- splits on spaces
            chunks = split ':' line_no_comments -- splits on ':' so should
        in
                if length chunks < 2 then False else chunks !! (length chunks - 2) == "::"
-}

findDeclLineVars :: String -> [String]
findDeclLineVars line  = if Data.List.isInfixOf "::" line && head ( filter (/=' ') line) /= '!'
        then
            -- get the vars:
            let
                line_no_comments =  head $ splitDelim "!" line -- (warning line ("<OL:"++line++">"))
                line_no_spaces = filter (/=' ') line_no_comments
                lhs:rhs:[] = splitDelim "::" line_no_spaces -- (warning line_no_spaces ("<L:"++line_no_spaces++">"))
                rhs_mvars = splitDelim "," rhs -- (warning rhs ("<"++rhs++">"))
                rhs_vars = map (head . (splitDelim "=")) rhs_mvars
            in
                rhs_vars
        else
            []


isImplicitNoneDecl line =  Data.List.isInfixOf "implicit none" line
-- WV
isUseDecl line = let
    chunks = filter (not . null) $ words line
    in
        if ((not (null chunks)) && (head chunks == "use")) then True else False -- init (tail (chunks !! 1)) else ""

-- WV This is weak because I assume the module name is the file name. I should at least try and remove "module_" from the name TODO
-- WV But even then, this should only really be done for modules that only contain declarations, so I should check that TODO
readUsedModuleDecls :: String -> String -> [String] -> [String] -> Bool -> IO ([String],(String,[String]))
readUsedModuleDecls sourceDir line cppDArgs cppXArgs fixedForm =
    let
        chunks = filter (not . null) $ words line
        module_name_maybe_commment = chunks !! 1
        module_name_chunks = split '!' module_name_maybe_commment
        module_name = module_name_chunks !! 0
        file_name_root
            | take  (length "module_") module_name == "module_" = drop (length "module_") module_name
            | otherwise = module_name
--        module_name = chunks !! 1
    in
        do
            -- putStrLn $ "readUsedModuleDecls: " ++ line
            test1 <- doesFileExist (sourceDir ++ "/" ++ module_name ++ ".f95")
            test2 <- doesFileExist (sourceDir ++ "/" ++ file_name_root ++ ".f95")
            -- putStrLn $ "Test1: "++ sourceDir ++ "/" ++ module_name ++ ".f95 "++(show test1)
            -- putStrLn $ "Test2: "++ sourceDir ++ "/" ++ file_name_root ++ ".f95 "++(show test2)
            -- FIXME: I am simply ignoring stash and moduleVarTable for now!
            (module_content_str, stash,moduleVarTable) <- if test1
                then
                        -- readFile (module_name ++ ".f95")
                        preProcessingHelper cppDArgs cppXArgs fixedForm True sourceDir (module_name ++ ".f95")
                else
                    if test2
                        then
                            do
                                -- putStrLn "passed test 2"
                            -- OK, this file exists. read it and check if it is decl-only
                            -- we do this by checking for "contains" and "subroutine "
                            -- readFile (file_name_root ++ ".f95")
                                preProcessingHelper cppDArgs cppXArgs fixedForm True sourceDir (file_name_root ++ ".f95")
                        else
                            do
                                -- putStrLn "failed test 2"
                                return ("",DMap.empty,DMap.empty)
            let test3 = isDeclOnly module_content_str
            -- putStrLn $ "Module content " ++ module_content_str
            -- putStrLn $ "Test3: "++(show test3)++" "++line
            if test3
                then
                    do
                        -- putStrLn "passed test 3"
                        let
                            module_lines = lines module_content_str
                            (decl_lines, other_lines) = partition findDeclLine module_lines
                            vars_per_line = map findDeclLineVars decl_lines
                            vars = foldl (++) [] vars_per_line
                        return (decl_lines,(module_name,vars)) -- here we should also return the module, and preferably a list of all declared vars in the module
                else
                    do
                        -- putStrLn "failed test 3"
                        return ([line],("",[])) -- ++" ! "++(show (test1,test2,test3))]

isDeclOnly module_content_str = let
    module_lines = lines module_content_str
    module_lines_no_comments = map (takeWhile (/= '!')) module_lines
    module_lines_no_comments_no_blanks = removeBlankLines module_lines_no_comments
    relevant_module_lines = filter isRelevantModuleLine module_lines_no_comments_no_blanks
    non_decl_lines = filter (not . findDeclLine) relevant_module_lines
    in
        null non_decl_lines
        -- (warning (null non_decl_lines) (show non_decl_lines) )

-- This removes the module declaration as well as "use", "contains" and "implicit". Rather ad-hoc
isRelevantModuleLine line
    | null chunks = False
    | otherwise =
        let
            w1 = head chunks
            w2 = if length chunks > 1 then chunks !! 1 else ""
            res
                | (head w1) == '!' = False
                | w1 `elem` ["module","contains","use","implicit"] = False
                | w1 == "end" && w2 `elem` ["module"] = False
                | otherwise = True
        in
            res
  where
    chunks = words line
-- WV: refined this: any "implicit none" after the "use" should come before the inline
-- The proper way is to check for the presence of such a line, remove it, and in a second pass add it before the first decl / after the last use
-- Also, we need to build a database of the modules and the declarations taken from each module
inlineDeclsFromUsedModules :: String -> Bool -> [String] ->[String] ->[String] -> Bool -> IO ([String], ModuleVarsTable)
inlineDeclsFromUsedModules sourceDir False contentLines _ _ _ = return (contentLines, DMap.empty)
inlineDeclsFromUsedModules sourceDir True contentLines cppDArgs cppXArgs fixedForm = do
                let
                    hasImplicitNone = length (filter isImplicitNoneDecl contentLines) > 0
                expandedContentLines' <- mapM (\line -> if (isUseDecl line) then (readUsedModuleDecls sourceDir line cppDArgs cppXArgs fixedForm) else return ([ line ],("",[]))) (filter (not . isImplicitNoneDecl) contentLines)
                let
                    (expandedContentLines,moduleVarTupleList) = unzip expandedContentLines'
                let
                    -- now we must reverse this: we want per variable the module
                    -- we assume vars are unique, so no conflicts across modules
                    vmtuplst = foldl (++) [] $ map (\(mod_name, vars) -> map (\var -> (var,mod_name)) vars) moduleVarTupleList -- [ (mod,[vars]) ,...]
                    moduleVarTable = DMap.fromList vmtuplst
                let expandedContentLines' = foldl (++) [] expandedContentLines
                let
                    expandedContentLines''
                        | hasImplicitNone = addImplicitNone expandedContentLines'
                        | otherwise = expandedContentLines'
                return (expandedContentLines'', moduleVarTable)

addImplicitNone :: [String] -> [String]
addImplicitNone contentLines =
    let
        (beforeImplicitNoneDecl,afterImplicitNoneDecl, _) = foldl (\ (bls,als,beforeDecl) line ->
                let
                    beforeDecl' = if beforeDecl && (Data.List.isInfixOf "::" line)  then False else beforeDecl
                in
                    if beforeDecl'
                        then (bls++[line], als, beforeDecl')
                        else  (bls, als++[line], beforeDecl')
                ) ([],[],True) contentLines
    in
         beforeImplicitNoneDecl ++ ["      implicit none"] ++ afterImplicitNoneDecl



splitDelim :: String -> String -> [String]
splitDelim patt line =
    let
        (chunks,last_chunk,_) = foldl (\ (chunks,current_chunk,rest_of_line) ch ->
                    if Data.List.isPrefixOf patt rest_of_line
                        then
                            let
                                 rest_of_line' = case Data.List.stripPrefix patt rest_of_line of
                                    Just r  -> r
                                    Nothing -> rest_of_line
                            in
                                (chunks++[current_chunk],"",rest_of_line')
                        else
                            if length rest_of_line > 0
                            then
                                let
                                    ch':rest_of_line' = rest_of_line
                                    current_chunk' = current_chunk++[ch']
                                in
                                    (chunks, current_chunk', rest_of_line')
                            else
                                 (chunks, current_chunk, rest_of_line)
                ) ([],"",line) line
    in
        chunks++[ last_chunk ]

-- splitDelim :: String -> String -> [String]
-- splitDelim delim str = LS.splitOn delim str -- (find_delim delim str [])

{-

find_delim :: String -> String -> String -> String
find_delim delim str str'
    | length str == 0 =  str'
    | otherwise =
        let
            nchars = length delim
            maybe_delim = take nchars str
        in
            if maybe_delim == delim
                then
                    find_delim delim (drop nchars str) (str'++" ")
                else
                    find_delim delim (tail str) (str' ++[head str])

-}

oneVarDeclPerVarDeclLine  :: [String] -> [String]
oneVarDeclPerVarDeclLine contentLines =
    let
        contentLines' :: [[String]]
        contentLines' = map (\line -> if isVarDeclWithMultipleVars line then splitOutVarDecls line else [ line ] ) contentLines
    in
        foldl (++) [] contentLines'

isVarDeclWithMultipleVars :: String -> Bool
isVarDeclWithMultipleVars line =
    let
        line_no_comments = head $ splitDelim "!" line
        chunks = splitDelim "::" line_no_comments
    in
        (findDeclLine line_no_comments ) && (length chunks ==2) && (Data.List.isInfixOf "," (chunks !! 1))

--isVarDecl
--commas after ::
splitOutVarDecls :: String -> [String]
splitOutVarDecls line =
    let
        line_no_comments =  head $ splitDelim "!" line
        lhs:rhs:[] = splitDelim "::" line_no_comments
        rhs_vars = splitDelim "," rhs
        new_lines = map (\var -> lhs++" :: "++var) rhs_vars
    in
        new_lines
-- split on "::"
-- split RHS on ','


-- paralleliseProgUnit_foldl :: SubroutineTable -> (SubroutineTable, [(String, String)]) -> String -> (SubroutineTable, [(String, String)])
-- paralleliseProgUnit :: SubroutineTable ->  [(String, String)] -> (SubroutineTable, [(String, String)])

-- stateful_pass :: SubroutineTable -> SubName -> Action -> a -> [String] -> (SubroutineTable, a)
-- type Action = b -> a -> (b, a)
--statefullPass :: (SubroutineTable -> a -> (SubroutineTable,a)) -> a -> SubroutineTable -> (SubroutineTable,a)
--statefullPass statefull_action state subtable = statefull_action subtable state

getLoopVars (OpenCLMap _ _ vrs vws lvars ilvars stmt1) = map (\(v,_,_,_) -> v) lvars
getLoopVars (OpenCLReduce _ _ vrs vws lvars ilvars rvarexprs stmt1) =  map (\(v,_,_,_) -> v) lvars
getLoopVars _ = []

getIterLoopVars (OpenCLMap _ _ vrs vws lvars ilvars stmt1) = ilvars
getIterLoopVars (OpenCLReduce _ _ vrs vws lvars ilvars rvarexprs stmt1) = ilvars
getIterLoopVars _ = []
