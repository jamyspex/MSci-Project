module LoopAnalysis                 (analyseLoop_map, analyseLoop_reduce, getErrorAnnotations, getReductionVarNames, getReads, getWrites)

where

--    This module holds functions that perform the top level of analysis. They look to see whether each of the loops in the source
--    can be parallelised or not. Calls to 'VarAccessAnalysis' and 'VarDependencyAnalysis' are made from here and the information
--    produce by those other modules is used to deteremine whether the conditions for parallelism are met. This module also handles
--    producing parallelism errors that are later attached to AST nodes.
import           MiniPP                (miniPP)
import           Warning

import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Language.Fortran
--import Language.Fortran.Pretty
import           Data.Char
import           Data.List
import qualified Data.Map              as DMap
import           Utils

import           F95IntrinsicFunctions (f95IntrinsicFunctions)
import           LanguageFortranTools
import           Parser                (SubRec (..), SubroutineTable)
import           VarAccessAnalysis     (VarAccessAnalysis, isFunctionCall)
import           VarDependencyAnalysis (VarDependencyAnalysis,
                                        isIndirectlyDependentOn)

--    Type used to standardise loop analysis functions
--    Functions below are used to manupulate and access the AnalysisInfo.
--                        errors         reduction variables read variables        written variables
type AnalysisInfo =     (Anno,         [Expr Anno],         [Expr Anno],         [Expr Anno])

analysisInfoBaseCase :: AnalysisInfo
analysisInfoBaseCase = (nullAnno,[],[],[])

combineAnalysisInfo :: AnalysisInfo -> AnalysisInfo -> AnalysisInfo
combineAnalysisInfo accum item = (combineMaps accumErrors itemErrors, accumReductionVars ++ itemReductionVars, accumReads ++ itemReads, accumWrites ++ itemWrites)
                                where
                                    (accumErrors, accumReductionVars, accumReads, accumWrites) = accum
                                    (itemErrors, itemReductionVars, itemReads, itemWrites)     = item

getErrorAnnotations :: AnalysisInfo -> Anno
getErrorAnnotations (errors, _, _, _) = errors

getReductionVarNames :: AnalysisInfo -> [Expr Anno]
getReductionVarNames (_, reductionVars, _, _) = reductionVars

getReads :: AnalysisInfo -> [Expr Anno]
getReads (_, _, reads, _) = reads

getWrites :: AnalysisInfo -> [Expr Anno]
getWrites (_, _, _, writes) = writes



--    Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--    cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: String -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> VarDependencyAnalysis -> SubroutineTable -> Fortran Anno -> AnalysisInfo
analyseLoop_map comment loopVars loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable codeSeg = case codeSeg of
        -- If _ _ condExpr ifTrue elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase ([condExprAnalysis] ++ readWriteAnalysis ++ [ifTrueAnalysis] ++ elifCondAnalysis ++ elifBodyAnalysis ++ [elseAnalysis]  )
        If _ _ condExpr ifTrue elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase ([condExprAnalysis, ifTrueAnalysis] ++ elifCondAnalysis ++ elifBodyAnalysis ++ [elseAnalysis]  )
            where
                recursiveCall = analyseLoop_map comment loopVars loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable
                -- readWriteAnalysis = gmapQ (mkQ analysisInfoBaseCase recursiveCall) codeSeg -- so this should call recursiveCall on all nodes of codeSeg, why?
                condExprAnalysis = (nullAnno, [], extractOperands condExpr, []) -- AnalysisInfo tuple from the 'if' condition
                ifTrueAnalysis = recursiveCall ifTrue
                elifBodyAnalysis = map (\(_, elif_fortran) ->  recursiveCall elif_fortran) elifList -- list of AnalysisInfo tuples from the body of each 'else if' branch
                elifCondAnalysis = map (\(elif_expr, _) -> (nullAnno, [], extractOperands elif_expr, [])) elifList -- list of AnalysisInfo tuples from the condition of each 'else if' branch
                elseAnalysis = case maybeElse of
                                    Just else_fortran ->  recursiveCall else_fortran
                                    Nothing -> analysisInfoBaseCase

        Assg _ srcspan lhsExpr rhsExpr -> foldl (combineAnalysisInfo) analysisInfoBaseCase [lhsExprAnalysis,
                                                                                                (DMap.empty,[],
                                                                                                prexistingReadExprs,
--                                                                                                (warning prexistingReadExprs ("MAP: PRE-EXISTING READ EXPRS: "++(unwords (map miniPP prexistingReadExprs))++"\n") ),
                                                                                                if isNonTempAssignment then [lhsExpr] else [])]
            where
                lhsExprAnalysis = analyseLoopIteratorUsage comment loopVars loopWrites nonTempVars accessAnalysis lhsExpr
                isNonTempAssignment = usesVarName_list nonTempVars lhsExpr

                readOperands = extractOperands rhsExpr
                -- WV: not sure if this should not be the same as for the Reduction
                readExprs = foldl (\accum item -> accum ++ (extractContainedVars item) ++ [item]) [] readOperands
                prexistingReadExprs = filter (usesVarName_list prexistingVars ) readExprs
--                prexistingReadExprs = filter (usesVarName_list  (warning prexistingVars ("MAP: PRE-EXISTING VARS: "++(show (map (\(VarName _ v)->v) prexistingVars) )++"\nRHS FULL: "++(miniPP rhsExpr)++"\n"++ ("READ EXPRS: "++(show (map miniPP readExprs))++"\n")  ) )) readExprs
                -- prexistingReadExprs = filter (usesVarName_list  (warning prexistingVars ("PRE: "++(show (map (\(VarName _ v)->v) prexistingVars) )++"\nRHS: "++(miniPP rhsExpr)++"\n") ))  (warning readExprs ("READ OPS: "++(show (map miniPP readExprs))++"\n") )
                --prexistingReadExprs = filter (usesVarName_list  prexistingVars) readExprs
                --
        For _ _ var e1 e2 e3 _ -> foldl combineAnalysisInfo analysisInfo childrenAnalysis  -- foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
            where
                childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map comment (loopVars ++ [var]) loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable)) codeSeg)

                e1Vars = extractAllVarNames e1
                e2Vars = extractAllVarNames e2
                e3Vars = extractAllVarNames e3

                readVars = map (generateVar) (listRemoveDuplications (e1Vars ++ e2Vars ++ e3Vars))
                analysisInfo = (nullAnno, [], readVars, [])

        Call _ srcspan callExpr arglist -> callAnalysis
            where
                --    If the called subroutine exists in a file that was supplied to the compiler, analyse it. If the subroutine is parallelisable,
                --    it is not parallelised internally but is instead included as part of some externel parallelism. If the subroutine was not parsed
                --    then add a parallelism error to the annotations.
                recursiveCall = analyseLoop_map comment loopVars loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable

                subroutineName = if extractVarNames callExpr == [] then (error "analyseLoop_map: callExpr" ++ (show callExpr))  else varNameStr (head (extractVarNames callExpr))
                -- argTranslation = generateArgumentTranslation subTable codeSeg
                (subroutineParsed, subTableEntry) = case DMap.lookup subroutineName subTable of
                                        Just a -> (True, a)
                                        Nothing -> (False, error "analyseLoop_map: DMap.lookup subroutineName subTable")
                subroutineBody = subAst subTableEntry
                subCallAnalysis = recursiveCall (extractFirstFortran subroutineBody)

                callAnalysis =     if not subroutineParsed then
                                    (errorMap_call, [], [], argExprs)
                                else
                                    subCallAnalysis

                errorMap_call = DMap.insert (outputTab ++ comment ++ "Call to external function:\n")
                                            [errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting callExpr]
                                            DMap.empty
                argExprs = everything (++) (mkQ [] extractExpr_list) arglist

        FSeq _ srcspan codeSeg1 codeSeg2 -> combineAnalysisInfo codeSeg1Analysis codeSeg2Analysis
            where
                recursiveCall = analyseLoop_map comment loopVars loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable
                codeSeg1Analysis = recursiveCall codeSeg1
                codeSeg2Analysis = recursiveCall codeSeg2
        _ -> foldl combineAnalysisInfo analysisInfoBaseCase (childrenAnalysis ++ nodeAccessAnalysis)
            where
                recursiveCall = analyseLoop_map comment loopVars loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable

                nodeAccessAnalysis = gmapQ (mkQ analysisInfoBaseCase (analyseLoopIteratorUsage comment loopVars loopWrites nonTempVars accessAnalysis)) codeSeg
                childrenAnalysis = gmapQ (mkQ analysisInfoBaseCase recursiveCall) codeSeg

--    Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--    doesn't represent a reduction. If the returned string is empty, the loop represents a possible parallel reduction
--    WV:  condExprs is used for the case of an assignment where the LHS is referenced in a condition of an if that encloses the assignment. It is part of the analysis to check if a variable depends on itself, not sure how it works.
analyseLoop_reduce :: String -> [Expr Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> Fortran Anno -> AnalysisInfo
analyseLoop_reduce comment condExprs loopVars loopWrites nonTempVars prexistingVars dependencies accessAnalysis codeSeg = case codeSeg of
--        If _ _ condExpr ifTrue elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase ([condExprAnalysis] ++ (warning readWriteAnalysis (show readWriteAnalysis)) ++(warning [ifTrueAnalysis] (show ifTrueAnalysis)) ++ elifCondAnalysis ++ elifBodyAnalysis ++ [elseAnalysis]  )
--      If _ _     expr _      elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase (readWriteAnalysis ++ elifAnalysis_fortran ++ elifAnalysis_readExprs ++ [elseAnalysis])
--        If _ _ condExpr ifTrue elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase ( (warning readWriteAnalysis (show readWriteAnalysis)) ++(warning [ifTrueAnalysis] (show ifTrueAnalysis)) ++ elifBodyAnalysis ++ [elseAnalysis]  )
        If _ _ condExpr ifTrue elifList maybeElse -> foldl combineAnalysisInfo analysisInfoBaseCase ( [condExprAnalysis,ifTrueAnalysis] ++ elifCondAnalysis ++ elifBodyAnalysis ++ [elseAnalysis]  )
            where
                recursiveCall condExprs_ = analyseLoop_reduce comment condExprs_ loopVars loopWrites nonTempVars prexistingVars dependencies accessAnalysis
--                readWriteAnalysis = gmapQ (mkQ analysisInfoBaseCase (recursiveCall (condExprs ++ [condExpr]))) codeSeg -- so this should call recursiveCall on all nodes of codeSeg, why?
                condExprAnalysis = (nullAnno, [], extractOperands condExpr, []) -- AnalysisInfo tuple from the 'if' condition
                ifTrueAnalysis = (recursiveCall (condExprs++[condExpr])) ifTrue
                --elifBodyAnalysis = map (\(elif_expr, elif_fortran) ->  (recursiveCall (condExprs ++ [elif_expr])) elif_fortran) elifList -- list of AnalysisInfo tuples from the body of each 'else if' branch
                elifBodyAnalysis = map (\(elif_expr, elif_fortran) ->  (recursiveCall (condExprs++[elif_expr])) elif_fortran) elifList -- list of AnalysisInfo tuples from the body of each 'else if' branch
                elifCondAnalysis = map (\(elif_expr, _) -> (nullAnno, [], extractOperands elif_expr, [])) elifList -- list of AnalysisInfo tuples from the condition of each 'else if' branch
                elseAnalysis = case maybeElse of
                                    Just else_fortran ->  (recursiveCall condExprs) else_fortran
                                    Nothing -> analysisInfoBaseCase
        For _ _ var e1 e2 e3 _ -> foldl combineAnalysisInfo analysisInfo childrenAnalysis -- foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
            where
                childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce comment condExprs (loopVars ++ [var]) loopWrites nonTempVars prexistingVars dependencies accessAnalysis)) codeSeg)
                e1Vars = extractAllVarNames e1
                e2Vars = extractAllVarNames e2
                e3Vars = extractAllVarNames e3

                readVars = map (generateVar) (listRemoveDuplications (e1Vars ++ e2Vars ++ e3Vars))
                analysisInfo = (nullAnno, [], readVars, [])

        Assg _ srcspan lhsExpr rhsExpr -> combineAnalysisInfo
                                            (errorMap3,
                                            if potentialReductionVar then [lhsExpr] else [],
                                            -- (warning prexistingReadExprs ("PRE-EXISTING READ EXPRS: "++(unwords (map miniPP prexistingReadExprs)))),
                                            prexistingReadExprs,
                                            if isNonTempAssignment then [lhsExpr] else []
                                            )
                                            (if not potentialReductionVar then
                                                lhsExprAnalysis
                                                else analysisInfoBaseCase)
            where
                writtenExprs = extractOperands lhsExpr
                readOperands = listSubtract (extractOperands rhsExpr) (lhsExpr:(extractOperands lhsExpr))
                -- WV so for some reason here only the array index expressions are extracted. I would think we need both.
                -- WV so I concatenate readOperands with the array indices.
                -- WV: TODO: if the read operand is an intrinsic function it should not be included I guess
--                readExprsRec = foldl (\accum item -> accum ++ (extractContainedVarsRec item)) []  readOperands
                readExprs = readOperands ++ (foldl (\accum item -> accum ++ (extractContainedVars item)) [] readOperands)
--                readExprs = readOperands ++ (foldl (\accum item -> accum ++ (extractContainedVars item)) [] (warning readOperands $ "READ OPS:"++(show $ map miniPP readOperands) ) )

                topLevelReadExprs = foldl (\accum item -> if isFunctionCall f95IntrinsicFunctions accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands
                -- WV: what does prexistingVars actually mean?
                prexistingReadExprs = filter (usesVarName_list prexistingVars) readExprs
--                prexistingReadExprs = filter (usesVarName_list  (warning prexistingVars ("REDUCTION: PRE-EXISTING: "++(show (map (\(VarName _ v)->v) prexistingVars) )++"\nRHS FULL: "++(miniPP rhsExpr)++"\n"++ ("READ EXPRS: "++(show (map miniPP readExprs))++"\n")  ) )) readExprs

                dependsOnSelfOnce = length (filter (\item -> applyGeneratedSrcSpans item == applyGeneratedSrcSpans lhsExpr) topLevelReadExprs) == 1

                isNonTempAssignment = usesVarName_list nonTempVars lhsExpr

                referencedInOuterConditionalStatement = (foldl (||) False $ map (\x -> hasOperand x lhsExpr) condExprs)
                referencedSelf = (hasOperand rhsExpr lhsExpr)
                associative = isAssociativeExpr lhsExpr rhsExpr

                dependsOnSelf = referencedSelf || referencedInOuterConditionalStatement || dependsOnSelfOnce
                                    || (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies (head $ (extractVarNames x)++[VarName nullAnno "DUMMY8"]) x) writtenExprs)

                lhsExprAnalysis = (analyseLoopIteratorUsage comment loopVars loopWrites nonTempVars accessAnalysis lhsExpr)
                rhsExprAnalysis = (analyseLoopIteratorUsage comment loopVars loopWrites nonTempVars accessAnalysis rhsExpr)
                doesNotUseFullLoopVar = (\(errorMap, _, _, _) -> errorMap /= nullAnno) lhsExprAnalysis

                --    Potential reduction vars are those variables that fit the preliminary conditions for being a reduction varaiable (that is,
                --    a variable to which some higher dimension of values are reduced into)
                potentialReductionVar = isNonTempAssignment && (dependsOnSelf) && doesNotUseFullLoopVar

                errorMap1 = DMap.empty
                errorMap2 = if potentialReductionVar && (not dependsOnSelfOnce) then
                                            DMap.insert (outputTab ++ comment ++ "Possible reduction variables must only appear once on the right hand side of an assignment:\n")
                                                [errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting lhsExpr]
                                                errorMap1
                                            else errorMap1
                errorMap3 = if dependsOnSelfOnce && potentialReductionVar && (not associative) then
                                            DMap.insert (outputTab ++ comment ++ "Not associative function:\n")
                                                [errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting rhsExpr]
                                                errorMap2
                                            else errorMap2
                errorMapDebug = DMap.insert (outputTab ++ comment ++ "Debug:\n")
                                                ["lhsExpr: " ++ outputExprFormatting lhsExpr ++ "\n" ++
                                                "potentialReductionVar: " ++ show potentialReductionVar ++ "\n" ++
                                                "isNonTempAssignment: " ++ show isNonTempAssignment ++ "\n" ++
                                                "dependsOnSelf: " ++ show dependsOnSelf ++ "\n" ++
                                                "dependsOnSelfOnce: " ++ show dependsOnSelfOnce ++ "\n" ++
                                                "doesNotUseFullLoopVar: " ++ show doesNotUseFullLoopVar ++ "\n" ++
                                                "nonTempVars: " ++ show nonTempVars ++ "\n\n"
                                                ]
                                                errorMap3

        Call _ srcspan expr arglist -> (errorMap_call, [], [], argExprs)
            where
            -- WV: TODO: a function call should not mean that the reduction can't be parallelised!
                errorMap_call = DMap.insert (outputTab ++ comment ++ "Call to external function:\n")
                                                [errorLocationFormatting srcspan ++ outputTab ++ outputExprFormatting expr]
                                                DMap.empty
                argExprs = everything (++) (mkQ [] extractExpr_list) arglist
        FSeq _ srcspan codeSeg1 codeSeg2 -> combineAnalysisInfo codeSeg1Analysis codeSeg2Analysis
            where
                recursiveCall = analyseLoop_reduce comment condExprs loopVars loopWrites nonTempVars prexistingVars dependencies accessAnalysis
                codeSeg1Analysis = recursiveCall codeSeg1
                codeSeg2Analysis = recursiveCall codeSeg2
-- If it is not an If, For, Assg or Call, or (WV) FSeq
        _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce comment condExprs loopVars loopWrites nonTempVars prexistingVars dependencies accessAnalysis)) codeSeg)

--    Applied to an expression, returns an AnalysisInfo loaded with an error if it does not use all of the loop iterators in some way. As in,
--    in a nested loop over 'i' and 'j', expression 'x(i) + 12' doesn't use the iterator 'j' and so the AnalysisInfo will report that. If
--    this error occurs when looking for a map then a map cannot be performed. if it is found while looking for a reduction, it is a sign that
--    this expression represents a reduction variable.
analyseLoopIteratorUsage :: String -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarAccessAnalysis -> Expr Anno -> AnalysisInfo
analyseLoopIteratorUsage comment loopVars loopWrites nonTempVars accessAnalysis expr = (unusedIterMap, [],[],[])
                                where
                                    operands = case fnCall of
                                            True ->    extractContainedVars expr
                                            False -> extractOperands expr
                                    writtenOperands = filter (usesVarName_list loopWrites) operands
                                    fnCall = isFunctionCall f95IntrinsicFunctions accessAnalysis expr
                                    nonTempWrittenOperands = filter(usesVarName_list nonTempVars) writtenOperands

                                    unusedIterMap = foldl (analyseLoopIteratorUsage_foldl nonTempWrittenOperands comment) DMap.empty loopVars

analyseLoopIteratorUsage_foldl :: [Expr Anno] -> String -> Anno -> VarName Anno -> Anno
analyseLoopIteratorUsage_foldl nonTempWrittenOperands comment accumAnno loopVar = resultantMap
        where
            offendingExprs = filter (\item -> not (elem loopVar (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedOperands item) ))) nonTempWrittenOperands
            offendingExprsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) offendingExprs

            loopVarStr = varNameStr loopVar
            resultantMap = if (offendingExprs == [])
                        then accumAnno
                        else
                            DMap.insert (outputTab ++ comment ++ "Non temporary, write variables accessed without use of loop iterator \"" ++ loopVarStr ++ "\":\n") offendingExprsStrs accumAnno
            nonTempWrittenOperandsStrs = map (\item -> errorLocationFormatting (srcSpan item) ++ outputTab ++ outputExprFormatting item) nonTempWrittenOperands

--    Function checks whether the primary in a reduction assignmnet is an associative operation. Checks both associative ops and functions.
isAssociativeExpr :: Expr Anno -> Expr Anno -> Bool
isAssociativeExpr assignee assignment = case assignment of
                            (Bin _ _ op expr1 expr2) -> associativeOp
                            _                        -> associativeFunc
                        where
                            primaryOp = extractPrimaryReductionOp assignee assignment
                            primaryFunc = extractPrimaryReductionFunction assignee assignment
                            associativeOp = case primaryOp of
                                                Just oper -> isAssociativeOp oper
                                                Nothing -> False
                            associativeFunc = isAssociativeFunction primaryFunc

isAssociativeOp :: BinOp Anno -> Bool
isAssociativeOp (Plus p) = True
isAssociativeOp (Mul p)  = True
isAssociativeOp (Or p)   = True
isAssociativeOp _        = False

--    Not yet used. In future the program may be able to detect whether or not a variable is given an appropriate start value for a reduction
opIdentityValue :: BinOp Anno -> Expr Anno
opIdentityValue (Plus p) = Con nullAnno nullSrcSpan "0"
opIdentityValue (Mul p)  = Con nullAnno nullSrcSpan "1"
opIdentityValue (Or p)   = Con nullAnno nullSrcSpan ".FALSE."
opIdentityValue _        = Null nullAnno nullSrcSpan

isAssociativeFunction :: String -> Bool
isAssociativeFunction fnName = case (map (toLower) fnName) of
                                "min"   -> True
                                "max"   -> True
                                "amax1" -> True
                                "amin1" -> True
                                _       -> False
