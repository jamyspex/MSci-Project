module VarAccessAnalysis (
        VarAccessAnalysis,
        analyseAllVarAccess,
        collectVarNames,
        getDeclaredVarNames,
        analyseAllVarAccess_progUnit,
        getPrexistingVars,
        getValueAtSrcSpan,
        getNonTempVars,
        getArguments,
        getAccessLocationsInsideSrcSpan,
        isFunctionCall,
        f95IntrinsicFunctions,
        getAccessesBetweenSrcSpans,
        getAccessLocationsBeforeSrcSpan,
        getAccessLocationsAfterSrcSpan,
        getAccessesBetweenSrcSpansIgnore
        )

where

--    The code in this file is used to analyse which variables are read and written and where in a certain
--    program. This information is then used to determine whether or not a variable in a loop can be deemed
--    temporary and therefore governs how variables are treated. Analysis also determines values/expressions
--    for variables at different points in the program. This is used when generating reduction kernels to be
--    able to assign an initial value to reduction variables. Finally, data from this analysis phase is used
--    to differentiate between function calls and array accesses in the input source.
--
-- WV: Here we should identify stencils.
-- WV: A stencil means that inside the loop, an array has been accessed with different iterators
-- WV: So I will extend VarAccessRecord to include the read accesses; for completeness also the write accesses:
-- WV: we use getAccessedExprs() to get the list of index expressions.


import           Data.Char
import           Data.Generics           (Data, Typeable, everything,
                                          everywhere, gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.Map                as DMap
import           F95IntrinsicFunctions   (f95IntrinsicFunctions)
import           Language.Fortran
import           Language.Fortran.Parser
import           LanguageFortranTools
import           Utils
import           Warning


--    Type used to colate data on variable accesses throughout a program.
--                        All reads     All writes
type VarAccessRecord = ([SrcSpan],     [SrcSpan])

type VarAccessRecordWV = ( [(SrcSpan,Expr Anno)], [(SrcSpan, Expr Anno)] )

-- WV: this map proveds all read and write locations for a local variable
type LocalVarAccessAnalysis = DMap.Map (VarName Anno) VarAccessRecord
-- WV: this map provides the expression(s) defining the local variable, I guess, so this would be assignments and should be args of subcalls with intent Out or InOut
type LocalVarValueAnalysis = DMap.Map (VarName Anno) [(SrcSpan, Expr Anno)]
-- WV: I wonder if instead of [VarName Anno] a declaration would not have been better?
-- WV: The list of LocalVarAccessAnalysis is an extenstion to deal with IO routines: [Kernel, IORoutines]
--                                                                            Subroutine arguments     Declared var names
type VarAccessAnalysis = ([LocalVarAccessAnalysis],    LocalVarValueAnalysis, [VarName Anno],     [VarName Anno])

analyseAllVarAccess_progUnit :: [String] -> ProgUnit Anno -> VarAccessAnalysis
analyseAllVarAccess_progUnit ioWriteSubroutineNames progUnit = analyseAllVarAccess ioWriteSubroutineNames [progUnit]

analyseAllVarAccess :: [String] -> Program Anno -> VarAccessAnalysis
analyseAllVarAccess ioWriteSubroutineNames prog = ([localVarAccesses,ioRoutineAnalysis], localVarValues, arguments, declarations)
                        where
                            --    LocalVarAccesses is made up of information on all of the reads and writes throughout
                            --    the program being analysed. It is a list of triplets where each triplet contains a
                            --    VarName, a list of read locations of that VarName and a list of write locations of that
                            --    VarName
                            (localVarAccesses,ioRoutineAnalysis) = analyseLocalVarAccess ioWriteSubroutineNames declarations prog
                            --    Arguments to this program block are treated as de facto non temporary variables,
                            --    due to the fact that arguments are passed by reference by default in fortran.
                            arguments = getArguments prog

                            --    The main motivation for the tracking the declarations at the top of a program is for
                            --    differentiating function calls from array accesses as Language-Fortran does not do this
                            --    automatically it seems.
                            declarations = everything (++) (mkQ [] getDeclaredVarNames) prog

                            localVarValues = everything combineMaps (mkQ DMap.empty analyseAllVarValues_fortran) prog

analyseLocalVarAccess :: [String] -> [VarName Anno] -> Program Anno -> (LocalVarAccessAnalysis,LocalVarAccessAnalysis)
analyseLocalVarAccess ioWriteSubroutineNames declarations prog = (analysis,ioRoutineAnalysis')
                where
                    blockAnalysis_ioRoutineAnalysis :: [(LocalVarAccessAnalysis,LocalVarAccessAnalysis)]
                    blockAnalysis_ioRoutineAnalysis =
                        foldl (\accum item -> accum ++ (gmapQ (mkQ (DMap.empty,DMap.empty) (analyseAllVarAccess_block ioWriteSubroutineNames declarations) ) item) ) [] prog
                    (blockAnalysis,ioRoutineAnalysis) = unzip blockAnalysis_ioRoutineAnalysis
                    progUnitAnalysis_ :: [(LocalVarAccessAnalysis,LocalVarAccessAnalysis)]
                    progUnitAnalysis_ =
                        foldl (\accum item -> accum ++ (gmapQ (mkQ (DMap.empty,DMap.empty) (analyseLocalVarAccess ioWriteSubroutineNames declarations)) item)) [] prog
                    (progUnitAnalysis,_) = unzip progUnitAnalysis_
                    analysis = foldl combineLocalVarAccessAnalysis DMap.empty (blockAnalysis ++ progUnitAnalysis)
                    ioRoutineAnalysis' = foldl combineLocalVarAccessAnalysis DMap.empty ioRoutineAnalysis

--    Since Language-Fortran does not seem to differentiate between function calls and array access, it was necessary
--    to find a way to identify a function call. This function achieves that. When an expression is passed in, a top level
--    VarNames is extracted (The possibility for multipe varnames is also dealt with here as the Language-Fortran
--    specification allows for this). A check to see whether this VarName was NOT declared at the top of the program is done
--    and a check to see whether the expr in question contains other expressions is also performed. If both of these checks
--    pass then the expr is a function call. (The second check here comes from the fact that the arguments of a function call
--    are stored in a list inside the original expr. If there are no arguments to the function, a NullExpr
--    object can be found. For a normal scalar value, there would be absolutely nothing in this internal list, not even a
--    NullExpr object)
--    WV: we also need to check for intrinsics using f95IntrinsicFunctions

isFunctionCall :: [String] -> VarAccessAnalysis -> Expr Anno -> Bool
isFunctionCall intrinsics accessAnalysis expr =  (exprVarNameStr `elem` intrinsics  ) || ( (all (\x -> not (elem x declaredVarNames)) exprVarNames) && subVars /= [])
                        where
                            subVars = extractContainedVars expr -- not so sure if this also works on e.g. sqrt(5)
                            exprVarNames = extractVarNames expr -- in principle just 1 variable name?
                            exprVarNameStr = if length exprVarNames > 0
                                then
                                    let
                                        VarName _ exprVarNameStr = head exprVarNames
                                    in
                                        exprVarNameStr
                                else ""
                            declaredVarNames = (\(_,_,_,x) -> x) accessAnalysis -- all var names declared at toplevel

isFunctionCall_varNames :: [String] -> [VarName Anno] -> Expr Anno -> Bool
isFunctionCall_varNames intrinsics declaredVarNames expr =  (exprVarNameStr `elem` intrinsics  ) || ( (all (\x -> not (elem x declaredVarNames)) exprVarNames) && subVars /= [] )
                        where
                            exprVarNameStr = if length exprVarNames > 0
                                then
                                    let
                                        VarName _ exprVarNameStr = head exprVarNames
                                    in
                                        exprVarNameStr
                                else ""
                            subVars = extractContainedVars expr
                            exprVarNames = extractVarNames expr

isNullExpr :: Expr Anno -> Bool
isNullExpr (NullExpr _ _) = True
isNullExpr _              = False

getAccessLocationsInsideSrcSpan :: VarAccessAnalysis -> VarName Anno -> SrcSpan -> ([SrcSpan], [SrcSpan])
getAccessLocationsInsideSrcSpan accessAnalysis accessVar src = (readsInside, writesInside)
        where
            localVarAccesses = (getAccessesInsideSrcSpan ((\(x:xs,_,_,_) -> x) accessAnalysis) src)
            -- (reads, writes) = DMap.findWithDefault (error "getAccessLocationsBeforeSrcSpan: doesn't exist") accessVar localVarAccesses

            (readsInside, writesInside) = DMap.findWithDefault ([], []) accessVar localVarAccesses
            -- readsInside = filter (\x -> checkSrcSpanContainsSrcSpan src x) reads
            -- writesInside = filter (\x -> checkSrcSpanContainsSrcSpan src x) writes

getAccessesBetweenSrcSpansIgnore :: VarAccessAnalysis -> SrcSpan -> SrcSpan -> [SrcSpan] -> ([VarName Anno], [VarName Anno])
getAccessesBetweenSrcSpansIgnore accessAnalysis (_,startLoc) (endLoc,_) skipSrcs = getAccessesBetweenManySrcSpans accessAnalysis allowedSrcSpans -- ([], [])
        where
            sortedSkipSrcs = sortBy srcSpanCompare skipSrcs
            allowedSrcSpans = getAccessesBetweenSrcSpansIgnoreBuildSrcSpans startLoc endLoc skipSrcs

getAccessesBetweenSrcSpansIgnoreBuildSrcSpans :: SrcLoc -> SrcLoc -> [SrcSpan] -> [(SrcLoc, SrcLoc)]
getAccessesBetweenSrcSpansIgnoreBuildSrcSpans prevEndLoc finalEndLoc [] = [(prevEndLoc, finalEndLoc)]
getAccessesBetweenSrcSpansIgnoreBuildSrcSpans prevEndLoc finalEndLoc ((startLoc, endLoc):skipSrcs) = [(prevEndLoc, startLoc)] ++ getAccessesBetweenSrcSpansIgnoreBuildSrcSpans endLoc finalEndLoc skipSrcs

srcSpanCompare :: SrcSpan -> SrcSpan -> Ordering
srcSpanCompare ((SrcLoc f1 l1 c1), _) ((SrcLoc f2 l2 c2), _)     |    l1 < l2 || (l1 == l2 && c1 < c2)     = LT
                                                                |    l1 > l2 || (l1 == l2 && c1 > c2)     = GT
                                                                |    l1 == l2 && c1 == c2                 = EQ

getAccessesBetweenManySrcSpans ::  VarAccessAnalysis -> [(SrcLoc, SrcLoc)] -> ([VarName Anno], [VarName Anno])
getAccessesBetweenManySrcSpans accessAnalysis [] = ([],[])
getAccessesBetweenManySrcSpans accessAnalysis ((startLoc, endLoc):srcs) = ((listConcatUnique currentReads followingReads), (listConcatUnique currentWrites followingWrites))
        where
            (currentReads, currentWrites) = getAccessesBetweenSrcSpans accessAnalysis startLoc endLoc
            (followingReads, followingWrites) = getAccessesBetweenManySrcSpans accessAnalysis srcs

getAccessLocationsBeforeSrcSpan :: VarAccessAnalysis -> VarName Anno -> SrcSpan -> ([SrcSpan], [SrcSpan])
getAccessLocationsBeforeSrcSpan accessAnalysis accessVar src = (readsBefore, writesBefore)
        where
            localVarAccesses = (\(x:xs,_,_,_) -> x) accessAnalysis
            -- (reads, writes) = DMap.findWithDefault (error "getAccessLocationsBeforeSrcSpan: doesn't exist") accessVar localVarAccesses
            (reads, writes) = DMap.findWithDefault ([], []) accessVar localVarAccesses
            readsBefore = filter (\x -> checkSrcSpanBefore x src) reads
            writesBefore = filter (\x -> checkSrcSpanBefore x src) writes

getAccessLocationsAfterSrcSpan :: VarAccessAnalysis -> VarName Anno -> SrcSpan -> ([SrcSpan], [SrcSpan])
getAccessLocationsAfterSrcSpan
 accessAnalysis accessVar src = (readsAfter, writesAfter)
        where
            localVarAccesses = (\(x:xs,_,_,_) -> x) accessAnalysis
            -- (reads, writes) = DMap.findWithDefault (error "getAccessLocationsAfterSrcSpan: doesn't exist") accessVar localVarAccesses
            (reads, writes) = DMap.findWithDefault ([], []) accessVar localVarAccesses
            readsAfter = filter (\x -> checkSrcSpanAfter x src) reads
            writesAfter = filter (\x -> checkSrcSpanAfter x src) writes

getAccessesAfterSrcSpan :: VarAccessAnalysis -> SrcLoc -> ([VarName Anno], [VarName Anno])
getAccessesAfterSrcSpan accessAnalysis startLoc = (reads, writes)
        where
            localVarAccesses = (\(x:xs,_,_,_) -> x) accessAnalysis
            allVars = DMap.keys localVarAccesses
            reads = filter (varReadAfterSrcLoc localVarAccesses startLoc) allVars
            writes = filter (varWrittenAfterSrcLoc localVarAccesses startLoc) allVars

varWrittenAfterSrcLoc :: LocalVarAccessAnalysis -> SrcLoc -> VarName Anno -> Bool
varWrittenAfterSrcLoc localVarAccesses loc var = appearance
        where
            writes = map (snd) (snd (DMap.findWithDefault ([],[]) var localVarAccesses) )
            appearance = foldl (\accum item -> accum || ((checkSrcLocBefore loc item) )) False writes

varReadAfterSrcLoc :: LocalVarAccessAnalysis -> SrcLoc -> VarName Anno -> Bool
varReadAfterSrcLoc localVarAccesses loc var = appearance
        where
            reads = map (fst) (fst (DMap.findWithDefault ([],[]) var localVarAccesses))
            appearance = foldl (\accum item -> accum || ((checkSrcLocBefore loc item) )) False reads

getAccessesBeforeSrcSpan :: VarAccessAnalysis -> SrcLoc -> ([VarName Anno], [VarName Anno])
getAccessesBeforeSrcSpan accessAnalysis endLoc = getAccessesBetweenSrcSpans accessAnalysis (SrcLoc "" 0 0) endLoc

getAccessesBetweenSrcSpans :: VarAccessAnalysis -> SrcLoc -> SrcLoc -> ([VarName Anno], [VarName Anno])
getAccessesBetweenSrcSpans accessAnalysis startLoc endLoc = (reads, writes)
        where
            localVarAccesses = (\(x:xs,_,_,_) -> x) accessAnalysis
            allVars = DMap.keys localVarAccesses
            reads = filter (varReadInRange localVarAccesses startLoc endLoc) allVars
            writes = filter (varWrittenInRange localVarAccesses startLoc endLoc) allVars

varReadInRange :: LocalVarAccessAnalysis -> SrcLoc -> SrcLoc -> VarName Anno -> Bool
varReadInRange localVarAccesses startLoc endLoc var = appearance
        where
            reads = map (fst) (fst (DMap.findWithDefault ([],[]) var localVarAccesses))
            appearance = foldl (\accum item -> accum || ((checkSrcLocBefore startLoc item) && (checkSrcLocBefore item endLoc))) False reads

varWrittenInRange :: LocalVarAccessAnalysis -> SrcLoc -> SrcLoc -> VarName Anno -> Bool
varWrittenInRange localVarAccesses startLoc endLoc var = appearance
        where
            writes = map (fst) (snd (DMap.findWithDefault ([],[]) var localVarAccesses) )
            appearance = foldl (\accum item -> accum || ((checkSrcLocBefore startLoc item) && (checkSrcLocBefore item endLoc))) False writes

-- checkSrcLocBefore

getArguments :: Program Anno -> [VarName Anno]
getArguments prog = argNames
        where
            argNames = everything (++) (mkQ [] getArgNamesAsVarNames) prog

getArguments_list :: Arg Anno -> [VarName Anno]
getArguments_list arg = everything (++) (mkQ [] getArgNamesAsVarNames) arg

getArgNamesAsVarNames :: ArgName Anno -> [VarName Anno]
getArgNamesAsVarNames (ArgName _ str) = [VarName nullAnno str]
getArgNamesAsVarNames _               = []

getDeclaredVarNames :: Decl Anno -> [VarName Anno]
getDeclaredVarNames (Decl _ _ lst _) = foldl (\accum (expr1, _, _) -> accum ++ extractVarNames expr1) [] lst
getDeclaredVarNames decl = []

analyseAllVarValues_fortran :: Fortran Anno -> LocalVarValueAnalysis
analyseAllVarValues_fortran (Assg _ src expr1 expr2) = foldl (\accum item -> appendToMap item (src, expr2) accum) DMap.empty varnames
                                where
                                    varnames = extractVarNames expr1
analyseAllVarValues_fortran _ = DMap.empty

analyseAllVarAccess_block :: [String] -> [VarName Anno] -> Block Anno -> (LocalVarAccessAnalysis,LocalVarAccessAnalysis)
analyseAllVarAccess_block ioWriteSubroutineNames declarations (Block _ _ _ _ _ fortran) = analyseAllVarAccess_fortran ioWriteSubroutineNames declarations (DMap.empty,DMap.empty) fortran


analyseAllVarAccess_fortran :: [String] -> [VarName Anno] -> (LocalVarAccessAnalysis,LocalVarAccessAnalysis) -> Fortran Anno ->  (LocalVarAccessAnalysis,LocalVarAccessAnalysis)
analyseAllVarAccess_fortran ioWriteSubroutineNames declarations (prevAnalysis,prevAnalysis_io) codeSeg  = case codeSeg of
                                    Assg _ _ writeExpr readExpr -> (analysis',DMap.empty)
                                                where
                                                    readExprs = extractOperands readExpr
                                                    readVarNames = foldl (collectVarNames_foldl declarations) [] readExprs
                                                    -- on the LHS the expr can never be a function call so it is easy
                                                    writtenVarNames = extractVarNames writeExpr

                                                    analysis = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis readVarNames
                                                    analysis' = foldl (addVarWriteAccess (srcSpan writeExpr)) analysis writtenVarNames
                                    If _ _ readExpr mainFortran elseList maybeFortran -> (analysisIncChildren,analysisIncChildren_io)
                                                where
                                                    readExprs = extractOperands readExpr
                                                    readVarNames = foldl (collectVarNames_foldl declarations) [] readExprs

                                                    elseListFortran = map (snd) elseList
                                                    allFortran = case maybeFortran of
                                                            Nothing -> mainFortran:elseListFortran
                                                            Just finalElse -> mainFortran:finalElse:elseListFortran

                                                    analysis = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis readVarNames
                                                    analysis_io = foldl (addVarReadAccess (srcSpan readExpr)) prevAnalysis_io readVarNames

                                                    (analysisIncChildren_list,analysisIncChildren_io_list) = unzip $ map (analyseAllVarAccess_fortran ioWriteSubroutineNames declarations (analysis,analysis_io)) allFortran
                                                    analysisIncChildren = foldl combineLocalVarAccessAnalysis analysis analysisIncChildren_list
                                                    analysisIncChildren_io = foldl combineLocalVarAccessAnalysis analysis analysisIncChildren_io_list

                                 -- Call p SrcSpan (Expr p) (ArgList p);
                                    Call _ src callExpr argList -> analysis_tup
                                                where
                                                    subroutineName = if extractVarNames callExpr == []
                                                        then (error "flattenSubroutineCall: callExpr\n" ++ (show callExpr))
                                                        else varNameStr (head (extractVarNames callExpr))
                                                    -- IOWRITE
                                                    -- WV the plan is that any calls to subs that are not in the srcs in list on command will be
                                                    -- treated as I/O and eventually lead to generation of oclRead calls
                                                    -- But only if they are not in the main loop.
                                                    -- TODO: for now I just add their names on command line with the -iowrite flag
                                                    -- We need to collect the arguments, identify which ones are written to the OpenCL device
                                                    -- and read these back before this call
                                                    -- What we need to do is replace the AST node for the sub call with a group (FSeq) with all OpenCLBufferRead statements
                                                    -- Best place to do this is where the other OpenCLBufferRead statements are added
                                                    analysis_tup =
                                                        let
                                                                extractedExprs = everything (++) (mkQ [] extractExpr_list) argList
                                                                extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
                                                                --varNames :: [VarName Anno String]
                                                                varNames = foldl (collectVarNames_foldl declarations) [] extractedOperands
                                                                -- So these names need to be saved and used later to compare them to the
                                                                -- arguments of the kernel
                                                        in
                                                            if (subroutineName `elem` ioWriteSubroutineNames)
                                                            then
                                                                let
                                                                    analysis = foldl (addVarReadAccess src) prevAnalysis_io varNames
                                                                    analysis' = foldl (addVarWriteAccess src) analysis varNames
                                                                in
                                                                    --warning DMap.empty ("Subroutine "++subroutineName++" is an I/O Write subroutine, skipping analysis for "++(show analysis')++"\n")
                                                                    -- (prevAnalysis,warning analysis' ("Subroutine "++subroutineName++" is an I/O Write subroutine, skipping analysis for "++(show analysis')++"\n"))
                                                                    (prevAnalysis,analysis')
                                                            else
                                                                let

                                                        --    Both read and write since we don't know the intent inside the function/subroutine call.
                                                        --    When the subtoutine is supplied to the compiler, this information is disregarded for
                                                        --    more in depth analysis
                                                                    analysis = foldl (addVarReadAccess src) prevAnalysis varNames
                                                                    analysis' = foldl (addVarWriteAccess src) analysis varNames
                                                                in
                                                                    (analysis',prevAnalysis_io)


                                    _ -> (analysisIncChildren ,analysisIncChildren_io)
                                                where
                                                    extractedExprs = gmapQ (mkQ (Null nullAnno nullSrcSpan) extractExpr) codeSeg
                                                    extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
                                                    readVarNames = foldl (collectVarNames_foldl declarations) [] extractedOperands

                                                    analysis = foldl (addVarReadAccess (srcSpan codeSeg)) prevAnalysis readVarNames
                                                    analysis_io = foldl (addVarReadAccess (srcSpan codeSeg)) prevAnalysis_io readVarNames
                                                    (analysisIncChildren_list,analysisIncChildren_io_list) = unzip $ gmapQ (mkQ (DMap.empty,DMap.empty) (analyseAllVarAccess_fortran ioWriteSubroutineNames declarations (analysis,analysis_io))) codeSeg
                                                    analysisIncChildren = foldl (combineLocalVarAccessAnalysis) DMap.empty analysisIncChildren_list
                                                    analysisIncChildren_io = foldl (combineLocalVarAccessAnalysis) DMap.empty analysisIncChildren_io_list

-- WV: I guess the structure of the If is:
-- If  _ _ condition first_block [else_if cond more_blocks] (Maybe else_block)
-- If  p SrcSpan (Expr p) (Fortran p) [((Expr p),(Fortran p))] (Maybe (Fortran p))
-- WV: This is to deal with function calls. For these, we extract the arguments.
collectVarNames :: [VarName Anno] -> Expr Anno -> [VarName Anno]
collectVarNames declarations item = varnames
                        where
                            fnCall = isFunctionCall_varNames f95IntrinsicFunctions declarations item
                            fnArgs = extractContainedVars item
                            varnames = case fnCall of
                                True -> foldl (\accum item -> accum ++ extractVarNames item) [] fnArgs
                                False -> extractVarNames item

collectVarNames_foldl :: [VarName Anno] -> [VarName Anno] -> Expr Anno -> [VarName Anno]
collectVarNames_foldl declarations accum item = accum ++ collectVarNames declarations item

getValueAtSrcSpan :: VarName Anno -> SrcSpan -> VarAccessAnalysis -> Expr Anno
getValueAtSrcSpan varname target_src (_, analysis, _, _) = valueAtSrc
                                where
                                    values = DMap.findWithDefault [] varname analysis
                                    valueAtSrc = foldl (\accum (item_src, expr) -> if checkSrcSpanBefore item_src target_src then expr else accum) (NullExpr nullAnno nullSrcSpan) values

getAccessedExprs :: [VarName Anno] -> [Expr Anno] -> Expr Anno -> [Expr Anno]
getAccessedExprs declarations accum item = case fnCall of
                                            True ->    accum ++ extractContainedVars item
                                            False -> accum ++ extractOperands item
                                        where
                                            fnCall = isFunctionCall_varNames f95IntrinsicFunctions declarations item

--     Recursive function to add a record of a read for a certain VarName
addVarReadAccess :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
addVarReadAccess srcspan analysis varname = DMap.insert varname (newAccessRecord) analysis
                                        where
                                            (oldReads, oldWrites) = (DMap.findWithDefault ([],[]) varname analysis)
                                            newAccessRecord = (oldReads ++ [srcspan], oldWrites)

--     Recursive function to add a record of a write for a certain VarName
addVarWriteAccess :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
addVarWriteAccess srcspan analysis varname = DMap.insert varname newAccessRecord analysis
                                        where
                                            (oldReads, oldWrites) = (DMap.findWithDefault ([],[]) varname analysis)
                                            newAccessRecord = (oldReads, oldWrites ++ [srcspan])

combineVarAccessAnalysis :: VarAccessAnalysis -> VarAccessAnalysis -> VarAccessAnalysis
combineVarAccessAnalysis analysis1 analysis2 = resultantAnalysis
                        where
                            ([varAccess1,varAccess1'], varValue1, subArgs1, declared1) = analysis1
                            ([varAccess2,varAccess2'], varValue2, subArgs2, declared2) = analysis2
                            varAccessComb = combineLocalVarAccessAnalysis varAccess1 varAccess2
                            varAccessComb' = combineLocalVarAccessAnalysis varAccess1' varAccess2'
                            varValueComb = combineMaps varValue1 varValue2
                            subArgsComb = subArgs1 ++ subArgs2
                            declaredComb = declared1 ++ declared2

                            resultantAnalysis = ([varAccessComb,varAccessComb'], varValueComb, subArgsComb, declaredComb)

{-
type VarAccessRecord = ([SrcSpan],     [SrcSpan])
type LocalVarAccessAnalysis = DMap.Map (VarName Anno) VarAccessRecord
-
-}
--    Helper function used to merge two sets of variable access analysis records.
combineLocalVarAccessAnalysis :: LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
combineLocalVarAccessAnalysis analysis1 analysis2 = resultantAnalysis
                        where
                            analysis2List = DMap.toList analysis2
                            resultantAnalysis = foldl (\accum (key, value) -> DMap.insert key (combineBinaryListTuples (DMap.findWithDefault ([],[]) key accum) value) accum) analysis1 analysis2List

combineBinaryListTuples :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
combineBinaryListTuples (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

getPrexistingVars :: SrcSpan -> VarAccessAnalysis -> [VarName Anno]
getPrexistingVars codeBlockSrcSpan accessAnalysis = listRemoveDuplications (prexistingVars ++ subroutineArguments)
        where
            localVarAccesses = (\(x:xs,_, _, _) -> x) accessAnalysis
            subroutineArguments = (\(_,_, x, _) -> x) accessAnalysis
            allVars = DMap.keys localVarAccesses
            accessAnalysisInsideSrc = getAccessesInsideSrcSpan localVarAccesses codeBlockSrcSpan
            prexistingVars = filter (isPrexistingVar codeBlockSrcSpan accessAnalysisInsideSrc) allVars
-- This tests if a variable has been written to or read from before the point it is encountered.
-- If it was not written to, returns True
-- If it was only written to, returns False
-- If it was written to after being read from, return True
isPrexistingVar :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> Bool
isPrexistingVar codeBlockSrcSpan accessAnalysisInsideSrc var = case earliestWrite of
                                                                    Nothing -> True
                                                                    Just firstWrite -> case earliestRead of
                                                                                            Nothing -> False
                                                                                            Just firstRead -> checkSrcSpanBefore firstRead firstWrite
        where
            (reads, writes) = DMap.findWithDefault ([], []) var accessAnalysisInsideSrc
            earliestRead = getEarliestSrcSpan reads
            earliestWrite = getEarliestSrcSpan writes



varHasSrcBefore :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> Bool
varHasSrcBefore codeBlockSrcSpan localVarAccess var = foldl (\accum item -> accum || (checkSrcSpanBefore item codeBlockSrcSpan)) False varWrites
        where
            (varReads, varWrites) = DMap.findWithDefault ([],[]) var localVarAccess

--    The function is directly called by Transformer.hs when it is attempting to parallelise a certain loop. This function is supplied with a start
--    and end point for a loop (SrcSpan) and the VarAccessAnalysis record for the program. The returned list is all of the VarNames that must be
--    considdered non temporary for that loop. For a variable to be considered non temporary, it must either be an argument to this code block or
--    it must be read after the end of the loop, before any data is written to it. In the second case, this means that a variable is non temporary
--    if the final value left in it by the loop is read and used elsewhere.
getNonTempVars :: SrcSpan -> VarAccessAnalysis -> [VarName Anno]
getNonTempVars codeBlockSpan accessAnalysis = hangingReads ++ subroutineArguments
        where
            localVarAccesses = (\(x:xs,_, _, _) -> x) accessAnalysis
            subroutineArguments = (\(_,_, x, _) -> x) accessAnalysis
            readsAfterBlock = varAccessAnalysis_readsAfter codeBlockSpan localVarAccesses
            writesReadsAfterBlock = varAccessAnalysis_writesAfter codeBlockSpan readsAfterBlock
            hangingReads = filter (checkHangingReads writesReadsAfterBlock) (DMap.keys writesReadsAfterBlock)

varAccessAnalysis_writesAfter :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
varAccessAnalysis_writesAfter codeBlockSpan accessAnalysis = foldl (varAccessAnalysis_writesAfter' codeBlockSpan accessAnalysis) DMap.empty (DMap.keys accessAnalysis)


varAccessAnalysis_writesAfter' :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
varAccessAnalysis_writesAfter' (_, SrcLoc _ line_end _) accessAnalysis accumAnalysis varname = combineLocalVarAccessAnalysis accumAnalysis outputAnalysis
        where
            (readSpans, writeSpans) = DMap.findWithDefault ([], []) varname accessAnalysis
            newWriteSpans = filter (\((SrcLoc _ line_write column_write), _) -> line_write >= line_end) writeSpans
            outputAnalysis = DMap.insert varname (readSpans, newWriteSpans) DMap.empty

varAccessAnalysis_readsAfter :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis
varAccessAnalysis_readsAfter codeBlockSpan accessAnalysis = foldl (varAccessAnalysis_readsAfter' codeBlockSpan accessAnalysis) DMap.empty (DMap.keys accessAnalysis)


varAccessAnalysis_readsAfter' :: SrcSpan -> LocalVarAccessAnalysis -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
varAccessAnalysis_readsAfter' (_, SrcLoc _ line_end _) accessAnalysis accumAnalysis varname = combineLocalVarAccessAnalysis accumAnalysis outputAnalysis
        where
            (readSpans, writeSpans) = DMap.findWithDefault ([], []) varname accessAnalysis
            newReadSpans = filter (\((SrcLoc _ line_read column_read), _) -> line_read >= line_end) readSpans
            outputAnalysis = DMap.insert varname (newReadSpans, writeSpans) DMap.empty

checkHangingReads :: LocalVarAccessAnalysis -> VarName Anno -> Bool
checkHangingReads analysis varname = case earliestRead of
                                                        Just r ->    case earliestWrite of
                                                                        Just w -> not (checkSrcSpanBefore_line w r)
                                                                        Nothing -> True
                                                        Nothing ->    False
        where
            (readSpans, writeSpans) = DMap.findWithDefault ([], []) varname analysis
            earliestRead = getEarliestSrcSpan readSpans
            earliestWrite = getEarliestSrcSpan writeSpans

getAccessesInsideSrcSpan :: LocalVarAccessAnalysis -> SrcSpan -> LocalVarAccessAnalysis
getAccessesInsideSrcSpan localVarAccesses src = foldl (getAccessesInsideSrcSpan_var src) localVarAccesses vars
        where
            vars = DMap.keys localVarAccesses

getAccessesInsideSrcSpan_var :: SrcSpan -> LocalVarAccessAnalysis -> VarName Anno -> LocalVarAccessAnalysis
getAccessesInsideSrcSpan_var src localVarAccesses var = newLocalVarAccesses
        where
            (reads, writes) = DMap.findWithDefault ([], []) var localVarAccesses
            newReads = filter (srcSpanInSrcSpan src) reads
            newWrites = filter (srcSpanInSrcSpan src) writes
            newLocalVarAccesses = DMap.insert var (newReads, newWrites) localVarAccesses

