module BufferTransferAnalysis         (optimiseBufferTransfers, replaceSubroutineAppearances)
where

--     This module deals with optimising the use of buffer reads and writes in an attempt to minimise memory transfers. The functions
--     produce versions of the AST nodes of OpenCLMap and OpenCLReduce kernels where written and read arguments are removed if they
--     are deemed surplus. The new versions of the AST nodes are then passed back to the main which in turn passes them to code emission.
--     Also, this module produces a set of Data.Maps that contain argument/varname translations. That is, in situations where the same
--     variable is used with different varnames, there is a way to determine whether two differntly named vars from different subroutines
--     are in fact the same variable. This is necessary for correct buffer accesses across different subroutines

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere, everywhere')
import Data.Maybe                     (fromMaybe)
import Data.List (nub, foldl', partition)
import Language.Fortran
import Warning (warning)
import MiniPP 

import VarAccessAnalysis             (VarAccessAnalysis, analyseAllVarAccess, getAccessLocationsBeforeSrcSpan, getAccessLocationsInsideSrcSpan, getAccessesBetweenSrcSpans, 
                                    getAccessesBetweenSrcSpansIgnore, getAccessLocationsAfterSrcSpan, getArguments, collectVarNames , getDeclaredVarNames)
import LanguageFortranTools 
import SubroutineTable                 (SubroutineTable, SubRec(..), SubroutineArgumentTranslationMap, ArgumentTranslation, replaceKernels_foldl, subroutineTable_ast, extractAllCalls, 
                                    extractCalls)
import qualified Data.Map as DMap

--    This is one of the entry points into this module from the main. Using the previously constructed subroutine table and argument translation table(s), 
--    the function tries to preen the buffer transfers that take place. 
--    BASIC STEPS:
--        -    Flatten the main AST by replacing any call to known* subroutines with body of the already parallelised subroutine. This step involves adjusting
--            source location information such as line numbers so that variable access analysis can be correclty performed and utilised. (*known here means that
--            the subroutine body was provided in one of the input files to the compiler),
--        -    On the new flattened main AST, perform variable access analysis to catalogue where and how variables are accessed across the entire codebase. This
--            information allows the compiler to determine whether or not a variable is used between successive kernel calls and therefore whether it needs to be
--            read back to the host or written to the device.
--        -    Using variable access analysis, remove as many arguments as possible from all of the kernels that have been created.
--        -    Take the new kernels and determine when the 'initialisation' subroutine call can be perfomed. This is done with 'findEarliestInitialisationSrcSpan'
--        -    Finally, replace the kernels in the original subroutine table with the new kernels that use far less read/write arguments. Return the new
--            subroutine table along with the SrcSpans that indicate where initialisation should happen and where the OpenCL portion of the main ends.
optimiseBufferTransfers :: ([String],[String]) -> SubroutineTable -> SubroutineArgumentTranslationMap -> (Program Anno,[String]) -> (SubroutineTable, Program Anno)
optimiseBufferTransfers (ioWriteSubroutines,ioReadSubroutines) subTable argTranslations (mainAst,mainOrigLines) = -- error ("kernels_optimisedBetween: " ++ (show kernels_optimisedBetween))
                                                            (newSubTable, newMainAst_withReadsWritesIO) -- warning newMainAst_withReadsWrites (miniPPP (head newMainAst_withReadsWrites)))
        where
            -- WV: this is an inliner but only works if all args are vars
            flattenedAst = flattenSubroutineAppearances subTable argTranslations mainAst
            flattenedVarAccessAnalysis = analyseAllVarAccess [] flattenedAst
            flattenedVarAccessAnalysis' = flattenedVarAccessAnalysis -- warning flattenedVarAccessAnalysis (show flattenedVarAccessAnalysis)
            allArguments = nub $ foldl (++) [] $ map (getArguments . (\(_,sub_rec) -> [subAst sub_rec])) (DMap.toList subTable)
            allWrittenArgs :: [VarName Anno]
            allWrittenArgs = nub $ foldl (++) [] $ map ( getWrittenArgs . (\(_,sub_rec) -> subAst sub_rec)) (DMap.toList subTable)
            allReadArgs :: [VarName Anno]
            allReadArgs = nub $ foldl (++) [] $ map ( getReadArgs . (\(_,sub_rec) -> subAst sub_rec)) (DMap.toList subTable)

            -- WV: What this does is removing read/write pairs between a kernel and subsequent kernels
            optimisedFlattenedAst = optimseBufferTransfers_program flattenedVarAccessAnalysis flattenedAst
            -- WV: The next operation is only analysis, no transformation
            varAccessAnalysis = analyseAllVarAccess ioWriteSubroutines mainAst

            -- WV: So this is the list of buffers that need to be read back
            args_in_io_sub :: [(VarName Anno,([SrcSpan],[SrcSpan]) )] -- whereas what we need is just SrcSpan
            args_in_io_sub = filter (\(k,v) -> k `elem` allWrittenArgs) (DMap.toList $ (\([_,v],_,_,_) -> v) varAccessAnalysis)
            ioBufferReads = map (\(v,(s1s,s2s)) -> (v, head s1s)) args_in_io_sub
            ioWriteVars = map fst args_in_io_sub
            varAccessAnalysis' = varAccessAnalysis -- warning varAccessAnalysis ( "IO ROUTINE ARGS:"++(showVarLst $ map fst args_in_io_sub))
            kernels_optimisedBetween = extractKernels optimisedFlattenedAst            
            (kernelStartSrc, kernelEndSrc) = generateKernelCallSrcRange subTable mainAst
            kernelRangeSrc = (fst kernelStartSrc, snd kernelEndSrc)
            -- WV: This function attempts to remove redundant writes from Init and reads at the end
            (kernels_withoutInitOrTearDown, initWrites, varsOnDeviceAfterOpenCL) = stripInitAndTearDown flattenedVarAccessAnalysis' kernelRangeSrc kernels_optimisedBetween
            -- Any Sub arg that is a kernel write arg : S `intersection` KW
            initWrites' = initWrites `listUnion` (allArguments `listIntersection` allWrittenArgs)

            (bufferWritesBefore, bufferWritesAfter) = generateBufferInitPositions initWrites' mainAst kernelStartSrc kernelEndSrc varAccessAnalysis'
            (bufferReadsBefore, bufferReadsAfter) = generateBufferReadPositions varsOnDeviceAfterOpenCL mainAst kernelStartSrc kernelEndSrc varAccessAnalysis
            -- so a possible way to handle ioWrites is to add the variables to read to the bufferReadsAfter list
            -- I should now have access to the variables from IO routines
            --

            oldKernels = extractKernels flattenedAst
            kernelPairs = zip oldKernels kernels_withoutInitOrTearDown
            -- matches = map (\(a, b) -> a == b) kernelPairs
            -- keys = (DMap.keys subTable)

            newSubTable = foldl (replaceKernels_foldl kernelPairs) subTable (DMap.keys subTable)
            newMainAst_withReadsWrites = updateFStmtLstInMainAst mainAst updated_fstmtlst
--            updated_fstmtlst' = map (\stmt -> if isCall stmt then (warning stmt ("CALL "++(miniPPF (insertIOBufferWV ioWriteVars stmt)) )) else (warning stmt ("NOT CALL "++(miniPPF stmt) )) ) updated_fstmtlst
            -- What we need to do is find all calls to subs in the ioWrite list and replace them by an FSeq of buffer reads and the call
            updated_fstmtlst = foldl' 
                (\fstmtlst_ (varSrcPairs,writes,after) -> insertBufferWV writes after varSrcPairs fstmtlst_)
                (getFStmtLstFromMainAst mainAst) 
                [(bufferReadsAfter,False,True),(bufferReadsBefore,False,False),(bufferWritesAfter,True,True),(bufferWritesBefore,True,False)]
            newMainAst_withReadsWritesIO' =  insertIOBuffersBeforeIOWriteCalls allWrittenArgs ioWriteSubroutines newMainAst_withReadsWrites 
            newMainAst_withReadsWritesIO =  insertIOBuffersAfterIOReadCalls allReadArgs ioReadSubroutines newMainAst_withReadsWritesIO' 
-- !            TODO:
-- !            generate the code for the buffer reads as an FStmt list and modify updated_fstmtlst
-- ! Basically 
                            
isCall :: Fortran p -> Bool
isCall (Call _ _ _ _) = True
isCall _ = False

-- WV20170820 A hack to insert buffer reads before IO Write subroutines. 
-- The more-or-less proper way to do this I think is take the AST, and figure out where the kernel routine calls end and IO routines start.
-- So we split the main loop into zones: Kernel | IOWrite | Kernel | IOWrite  and maybe also IORead and even IORW. 
-- Basically, the way I would do this is by looking for a sequence of subroutine calls consisting of the union of the
-- kernel subroutines (subroutineNames) and the ioRead/Write/RWSubroutines from command line. So we find the first call to a sub in this set,
-- then keep going until we find something that is not a sub call.
-- then we prune at the back any calls  not in the set
-- If there are any calls in between that are not in the set they are simply ignored for this purpose
-- Then we add oclRead calls for every buffer in the first IOWrite routine before the call, we keep track, and add remaining calls as we proceed
-- For IORead calls we add oclWrite calls after the call. For IORW it would be a combination as we'd need to know which buffer is R and which W or
-- we simply assume that they are all RW
-- So the key question is of course, which args are buffers; and also, what kind of sub is it?
-- So given a sub, find if it is R/W/RW. Otherwise we do nothing.
-- Then get its args from the AST and check which ones are buffers using the table (e.g. kernelArgs)
-- 
-- WV20170405 New approach to OpenCL buffer read/write insertion
-- And of course this will now work for bufferWritesBefore etc as well
insertBufferWritesAfterWV' = insertBufferWV True True
insertBufferWritesBeforeWV' = insertBufferWV True False
insertBufferReadsAfterWV' = insertBufferWV False True
insertBufferReadsBeforeWV' = insertBufferWV False False

insertBufferWV' writes after varSrcPairs ast = let
    openCLBufferReadOrWrite 
        | writes = OpenCLBufferWrite
        | otherwise = OpenCLBufferRead
    fortranSrcPairs = map (\(var, src) -> (openCLBufferReadOrWrite nullAnno src var, src)) varSrcPairs
    astSrcSpans = nub $ map snd varSrcPairs
    groupedFortranSrcPairs :: [([Fortran Anno],SrcSpan)]
    groupedFortranSrcPairs = map (\astSrcSpan -> (map fst $ filter (\(bufferWrite,srcSpan) -> srcSpan ==  astSrcSpan) fortranSrcPairs,astSrcSpan)) astSrcSpans
    -- now each of these should be turned into an FSeq
    fortranFSeqSrcPairs :: [(Fortran Anno, SrcSpan)]
    fortranFSeqSrcPairs = map (\(flst,srcspan) -> (transformFortranListIntoFSeq flst, srcspan)) groupedFortranSrcPairs
    -- now we have (FSeq, SrcSpan pairs) . We now want to find the corresponding srcspan in the ast. 
    -- The ast is the main program and what I need is the first FSeq. Easiest way is to traverse the AST top-down and modify the FSeq that matches the SrcSpan
    -- ast' = foldl' (\ast_ (fseq,srcspan) -> everywhere' (mkT (insertFortranAfterSrcSpanWV srcspan fseq)) ast_) ast fortranFSeqSrcPairs
    -- ast' = (\fseq_srcspan -> everywhere (mkT (insertFortranAfterSrcSpanWV fseq_srcspan)) ast) (head  fortranFSeqSrcPairs)
    main_progunit = head ast
    Main _ _ _ _ main_block _ = main_progunit
    Block _ _ _ _ _ start_fseq = main_block  
    fstmtlst = traverseFSeq start_fseq []
    -- These are the matching lines so what I need to do is splice in the groupedFortranSrcPairs
    matching_lines = foldl' (++) [] $ map (\(fseq,srcspan) -> filter (\fstmt -> (sameLine srcspan (getSrcSpan fstmt))) fstmtlst) fortranFSeqSrcPairs 
    updated_fstmtlst = foldl' (updateFStmtLst after) fstmtlst groupedFortranSrcPairs
    -- Finally we must turn updated_fstmtlst into an FSeq and wrap it into a Program
  in
--        error $ show fortranFSeqSrcPairs
        error $ unlines $ map (\fstmt -> (miniPPF fstmt)++"\t"++(showSrcSpan fstmt)) updated_fstmtlst 


insertBufferWV writes after varSrcPairs fstmtlst = let
    openCLBufferReadOrWrite 
        | writes = OpenCLBufferWrite
        | otherwise = OpenCLBufferRead
    fortranSrcPairs = map (\(var, src) -> (openCLBufferReadOrWrite nullAnno src var, src)) varSrcPairs
    astSrcSpans = nub $ map snd varSrcPairs
    groupedFortranSrcPairs :: [([Fortran Anno],SrcSpan)]
    groupedFortranSrcPairs = map (\astSrcSpan -> (map fst $ filter (\(bufferWrite,srcSpan) -> srcSpan ==  astSrcSpan) fortranSrcPairs,astSrcSpan)) astSrcSpans
    -- now each of these should be turned into an FSeq
    fortranFSeqSrcPairs :: [(Fortran Anno, SrcSpan)]
    fortranFSeqSrcPairs = map (\(flst,srcspan) -> (transformFortranListIntoFSeq flst, srcspan)) groupedFortranSrcPairs
    -- These are the matching lines so what I need to do is splice in the groupedFortranSrcPairs
    -- matching_lines = foldl' (++) [] $ map (\(fseq,srcspan) -> filter (\fstmt -> (sameLine srcspan (getSrcSpan fstmt))) fstmtlst) fortranFSeqSrcPairs 
    updated_fstmtlst = foldl' (updateFStmtLst after) fstmtlst groupedFortranSrcPairs
    -- Finally we must turn updated_fstmtlst into an FSeq and wrap it into a Program
  in
        updated_fstmtlst 


insertIOBufferReadWV :: [VarName Anno] -> Fortran Anno -> Fortran Anno
insertIOBufferReadWV vars fstmt = let
    ocl_buf_fstmts = map (OpenCLBufferRead nullAnno nullSrcSpan ) vars
    flst = ocl_buf_fstmts ++ [fstmt]
    fortranFSeq = transformFortranListIntoFSeq flst
  in
        fortranFSeq 

insertIOBufferWriteWV :: [VarName Anno] -> Fortran Anno -> Fortran Anno
insertIOBufferWriteWV vars fstmt = let
    ocl_buf_fstmts = map (OpenCLBufferWrite nullAnno nullSrcSpan ) vars
    flst = fstmt:ocl_buf_fstmts 
    fortranFSeq = transformFortranListIntoFSeq flst
  in
        fortranFSeq 

insertIOBuffersAfterIOReadCalls allReadArgs ioReadSubroutines ast = 
    let
        declarations = everything (++) (mkQ [] getDeclaredVarNames) ast
        main_progunit = head ast
        Main m1 m2 m3 m4 main_block m5 = main_progunit
        Block b1 b2 b3 b4 b5 start_fseq = main_block  
        new_start_fseq = everywhere (mkT (insertIOBufferAfterIOCall allReadArgs ioReadSubroutines declarations)) start_fseq
    in        
        [Main m1 m2 m3 m4 (Block b1 b2 b3 b4 b5 new_start_fseq) m5]    

insertIOBufferAfterIOCall allReadArgs ioReadSubroutines declarations fstmt@(Call p srcSpan callExpr argList) = 
    let
         subroutineName = if extractVarNames callExpr == [] 
            then (error "insertIOBufferAfterIOCall: callExpr\n" ++ (show callExpr))
            else varNameStr (head (extractVarNames callExpr))
         extractedExprs = everything (++) (mkQ [] extractExpr_list) argList
         extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
         --varNames :: [VarName Anno String]
         varNames = foldl (collectVarNames_foldl declarations) [] extractedOperands
         writtenVarNames = filter (\var -> var `elem` allReadArgs) varNames
    in
        if subroutineName `elem` ioReadSubroutines 
            then insertIOBufferWriteWV writtenVarNames fstmt 
            else fstmt

insertIOBufferAfterIOCall allReadArgs ioReadSubroutines declarations fstmt = fstmt



insertIOBuffersBeforeIOWriteCalls allWrittenArgs ioWriteSubroutines ast = 
    let
        declarations = everything (++) (mkQ [] getDeclaredVarNames) ast
        main_progunit = head ast
        Main m1 m2 m3 m4 main_block m5 = main_progunit
        Block b1 b2 b3 b4 b5 start_fseq = main_block  
        new_start_fseq = everywhere (mkT (insertIOBufferBeforeIOCall allWrittenArgs ioWriteSubroutines declarations)) start_fseq
    in        
        [Main m1 m2 m3 m4 (Block b1 b2 b3 b4 b5 new_start_fseq) m5]    

insertIOBufferBeforeIOCall allWrittenArgs ioWriteSubroutines declarations fstmt@(Call p srcSpan callExpr argList) = 
    let
         subroutineName = if extractVarNames callExpr == [] 
            then (error "insertIOBufferBeforeIOCall: callExpr\n" ++ (show callExpr))
            else varNameStr (head (extractVarNames callExpr))
         extractedExprs = everything (++) (mkQ [] extractExpr_list) argList
         extractedOperands = foldl (\accum item -> accum ++ extractOperands item) [] extractedExprs
         --varNames :: [VarName Anno String]
         varNames = foldl (collectVarNames_foldl declarations) [] extractedOperands
         writtenVarNames = filter (\var -> var `elem` allWrittenArgs) varNames
    in
        if subroutineName `elem` ioWriteSubroutines 
            then insertIOBufferReadWV writtenVarNames fstmt 
            else fstmt

insertIOBufferBeforeIOCall allWrittenArgs ioWriteSubroutines declarations fstmt = fstmt

collectVarNames_foldl :: [VarName Anno] -> [VarName Anno] -> Expr Anno -> [VarName Anno]
collectVarNames_foldl declarations accum item = accum ++ collectVarNames declarations item            


--getDeclaredVarNames :: Decl Anno -> [VarName Anno]
--getDeclaredVarNames (Decl _ _ lst _) = foldl (\accum (expr1, _, _) -> accum ++ extractVarNames expr1) [] lst
--getDeclaredVarNames decl = []


getFStmtLstFromMainAst ast = let
        main_progunit = head ast
        Main _ _ _ _ main_block _ = main_progunit
        Block _ _ _ _ _ start_fseq = main_block  
        fstmtlst = traverseFSeq start_fseq []
    in
        fstmtlst

updateFStmtLstInMainAst ast fstmtlst = let
        main_progunit = head ast
        Main m1 m2 m3 m4 main_block m5 = main_progunit
        Block b1 b2 b3 b4 b5 start_fseq = main_block  
        new_start_fseq = transformFortranListIntoFSeq fstmtlst
    in
        [Main m1 m2 m3 m4 (Block b1 b2 b3 b4 b5 new_start_fseq) m5]


updateFStmtLst after fstmtlst (bufstmtlst,srcspan) =
    foldl' (++) [] $ map (\fstmt -> if (sameLine srcspan (getSrcSpan fstmt)) 
            then 
                if after
                    then [fstmt]++bufstmtlst
                    else bufstmtlst++[fstmt]
            else [fstmt]) fstmtlst

sameLine ((SrcLoc _ startLine1 _), (SrcLoc _ endLine1 _)) ((SrcLoc _ startLine2 _), (SrcLoc _ endLine2 _)) = startLine1 == startLine2
        
showSrcSpan (Assg _ srcspan _ _) = show  srcspan
showSrcSpan (Call _ srcspan _ _) = show  srcspan
showSrcSpan fstmt = ""

getSrcSpan (Assg _ srcspan _ _) =   srcspan
getSrcSpan (Call _ srcspan _ _) =   srcspan
getSrcSpan fstmt = nullSrcSpan

insertFortranAfterSrcSpanWV (fseq_to_add,fseq_to_add_srcspan) fseq_ast@(FSeq anno ast_srcspan f1 f2) = let
        fseq_ast' = FSeq anno ast_srcspan (FSeq anno nullSrcSpan f1 fseq_to_add) f2
    in
        if ast_srcspan == fseq_to_add_srcspan then (warning fseq_ast' ("FOUND FSEQ: " ++(show ast_srcspan)++"\n") ) else (warning fseq_ast ("OTHER FSEQ: "++(show (fseq_to_add_srcspan,ast_srcspan))++"\n"))
insertFortranAfterSrcSpanWV fseq_srcspan f_other = warning f_other "NOT FSEQ\n"

-- So clearly ths 'everywhere' is not working the way I'd like it to. How about a manual traversal through FSeq? 
-- 
traverseFSeq fseq acc = case fseq of
    NullStmt nullAnno nullSrcSpan -> acc
    (FSeq anno ast_srcspan f1 f2) -> acc++(traverseFSeq f1 [])++(traverseFSeq f2 [])
    f -> acc ++ [f]


transformFortranListIntoFSeq :: [Fortran Anno] -> Fortran Anno
transformFortranListIntoFSeq flst
    | length flst == 0 = NullStmt nullAnno nullSrcSpan
    | length flst == 1 = head flst
    | otherwise = foldl' groupPairIntoFSeq (FSeq nullAnno nullSrcSpan fst_f (NullStmt nullAnno nullSrcSpan) ) rest_fs
  where
    fst_f:rest_fs = flst  
    
-- We can safely assume that the first arg is FSeq
groupPairIntoFSeq fseq f_to_add = let
        FSeq anno srcspan f_acc null_f = fseq
    in    
        FSeq anno srcspan (FSeq anno srcspan f_acc f_to_add) null_f
    
--    AIM:
--        Produce a list of varnames and associated locations where the location is the position at which that var's buffer must be read to allow the host
--        program to function correctly. This must consider loops.
--    LOGIC:
--        Construct an SrcSpan that represents the part of the program that would affect the values written to OpenCL buffers PRIOR to OpenCL doing work.
--        Inside this range, (which includes whole loops if the openCl work starts in a loop), locations where the vars in question are written to are
--        picked out. Those locations that are null (there are no writes in range) are removed. Next, only latests locations for each var are saved.
--        Finally, if those locations are inside the consideration range, but AFTER the end of the kernel calls (as in, we are in a loop, and the write
--        happens after the kennel calls in the loop) then the write location must be just before the start of the kernel calls, to ensure correctness.
generateBufferInitPositions :: [VarName Anno] -> Program Anno -> SrcSpan -> SrcSpan -> VarAccessAnalysis -> ([(VarName Anno, SrcSpan)], [(VarName Anno, SrcSpan)])
generateBufferInitPositions initWrites mainAst kernelStartSrc kernelEndSrc varAccessAnalysis = (bufferWritesBeforeSrc, bufferWritesAfterSrc)
        where 
            fortranNode = extractFirstFortran mainAst
            writeConsiderationEnd = findWriteConsiderationEnd (fst kernelStartSrc) fortranNode
            writeConsiderationSrc = (fst (srcSpan fortranNode), writeConsiderationEnd)

            varWritesInSrc = map (\var -> snd (getAccessLocationsInsideSrcSpan varAccessAnalysis var writeConsiderationSrc)) initWrites
            varWritesInSrcExcludingKernelRange = map (filter (\x -> not (srcSpanInSrcSpanRange kernelStartSrc kernelEndSrc x))) varWritesInSrc
            bufferWritePositions = zip initWrites (map (\srcs -> fromMaybe (nullSrcSpan) (getLatestSrcSpan srcs)) varWritesInSrcExcludingKernelRange)
            bufferWritePositionsExcludingNull = filter (\(var, src) -> src /= nullSrcSpan) bufferWritePositions
            bufferWritePositionsExcludingNull' = bufferWritePositionsExcludingNull -- warning bufferWritePositionsExcludingNull (show (bufferWritePositions, bufferWritePositionsExcludingNull))
            -- here, p2 is still present
            -- bufferWritePositionsKernelStart = map (\(var, src) -> if checkSrcLocBefore (fst kernelStartSrc) (fst src) then (var, kernelStartSrc) else (var, src)) bufferWritePositionsExcludingNull

            bufferWritesBeforeSrc = filter (\(var, src) -> src == kernelStartSrc) bufferWritePositionsExcludingNull' 
            bufferWritesAfterSrc = filter (\(var, src) -> src /= kernelStartSrc) bufferWritePositionsExcludingNull'

--    A similar approach to the function above, except we are looking for locations where a variable is read rather than written to. The consideration range
--    is after the kernel calls (including any loops that the kernel calls appear in) and any read that happens before the kernel calls in a loop mean that a
--    buffer read MUST happen directly after the kernel calls.
--    (Following functions are used to consider for-loops when placing buffer reads and writes in the main)
generateBufferReadPositions :: [VarName Anno] -> Program Anno -> SrcSpan -> SrcSpan -> VarAccessAnalysis -> ([(VarName Anno, SrcSpan)], [(VarName Anno, SrcSpan)])
generateBufferReadPositions finalReads mainAst kernelStartSrc kernelEndSrc varAccessAnalysis = (bufferReadsBeforeSrc, bufferReadsAfterSrc)
        where
            fortranNode = (extractFirstFortran mainAst)
            readConsiderationStart = findReadConsiderationStart (snd kernelEndSrc) fortranNode
            readConsiderationSrc = (readConsiderationStart, snd (srcSpan fortranNode))

            varReadsInSrc = map (\var -> snd (getAccessLocationsInsideSrcSpan varAccessAnalysis var readConsiderationSrc)) finalReads
            varReadsInSrcExcludingKernelRange = map (filter (\x -> not (srcSpanInSrcSpanRange kernelStartSrc kernelEndSrc x))) varReadsInSrc

            bufferReadPositions = zip finalReads (map (\srcs -> fromMaybe (nullSrcSpan) (getEarliestSrcSpan srcs)) varReadsInSrcExcludingKernelRange)
            bufferReadPositionsExcludingNull = filter (\(var, src) -> src /= nullSrcSpan) bufferReadPositions
            bufferReadPositionsKernelStart = map (\(var, src) -> if checkSrcLocBefore (fst src) (fst kernelEndSrc) then (var, kernelEndSrc) else (var, src)) bufferReadPositionsExcludingNull

            (bufferReadsBeforeSrc,bufferReadsAfterSrc) = partition (\(var, src) -> src == kernelEndSrc) bufferReadPositionsExcludingNull

--    Traverse the ast looking for a target location. If the target location is found to be within a loop, return the location of the end of the loop,
--    otherwise return the first location that appears at the target location.
findWriteConsiderationEnd :: SrcLoc -> Fortran Anno ->  SrcLoc
findWriteConsiderationEnd targetSrcLoc (For  _ src _ _ _ _ fortran) = findWriteConsiderationEnd (snd src) fortran
findWriteConsiderationEnd targetSrcLoc codeSeg     |    reachedTarget = (snd src)
                                                |    otherwise = fromMaybe (nullSrcLoc) (getEarliestSrcLoc recursiveResult)
        where
            src = srcSpan codeSeg
            reachedTarget = checkSrcLocEqualLines targetSrcLoc (snd src)

            recursiveResult = filter (/= nullSrcLoc) (gmapQ (mkQ nullSrcLoc (findWriteConsiderationEnd targetSrcLoc)) codeSeg)

--    Traverse the ast looking for a target location. If the target location is found to be within a loop, return the location of the start of the loop,
--    otherwise return the first location that appears at the target location.
findReadConsiderationStart :: SrcLoc -> Fortran Anno -> SrcLoc
findReadConsiderationStart targetSrcLoc (For _ src _ _ _ _ fortran)     |    childResult /= nullSrcLoc = fst src
                                                                        |    otherwise = nullSrcLoc
        where
            childResult = findReadConsiderationStart targetSrcLoc fortran
findReadConsiderationStart targetSrcLoc codeSeg     |    reachedTarget = fst src
                                                    |    otherwise = fromMaybe nullSrcLoc (getEarliestSrcLoc recursiveResult)
        where
            src = srcSpan codeSeg
            reachedTarget = checkSrcLocEqualLines targetSrcLoc (snd src)

            recursiveResult = filter (/= nullSrcLoc) (gmapQ (mkQ nullSrcLoc (findReadConsiderationStart targetSrcLoc)) codeSeg)        


replaceSubroutineAppearances :: SubroutineTable -> [(Program Anno,[String])] -> [(Program Anno,[String])]
replaceSubroutineAppearances subTable [] = []
replaceSubroutineAppearances subTable (firstProgram:programs) = (updated,orig_lines):replaceSubroutineAppearances subTable programs
        where 
            (firstProgram_code, orig_lines) = firstProgram
            updated = everywhere (mkT (replaceSubroutine subTable)) firstProgram_code

replaceSubroutine :: SubroutineTable -> ProgUnit Anno -> ProgUnit Anno
replaceSubroutine subTable codeSeg = case codeSeg of
                                (Sub _ _ _ (SubName _ str) _ _) -> subroutineTable_ast (DMap.findWithDefault (MkSubRec codeSeg "" []) str subTable)
                                _ -> codeSeg

--    This function has no type signiture to allow it to be appled to any AST node, making using of SYB generics.
flattenSubroutineAppearances subTable argTransTable mainAst = updated
        where
            subroutines = DMap.keys subTable
            updated = everywhere (mkT (flattenSubroutineCall_container subTable argTransTable)) mainAst

--    This function only makes changes to AST nodes that contain 'Call' nodes. It must consider the level above the call because the source
--    line information of surrounding nodes must be updated if a new subroutine body is folded into the original AST. 
--    STEPS:
--        -    Determine whether a node contains a call node, if so, continue, otherwise return the original node.
--        -    Extract the name of the called subroutine and retrieve the appropriate source line info of the body from the subroutine table.
--        -    Retrieve the appropriate argument translation entry for this subroutine, so that any arguments/variables that must be changed
--            in the body of the subroutine can be updated.
--        -    Call 'flattenSubroutineCall'
--            +    Retrieve the subroutine body
--            +    Adjust line/location information in the retrieved body code so that it fits into the new (usually the main) AST.
--            +    Return the subroutine body
--        -    Return the subroutine body
--       
flattenSubroutineCall_container :: SubroutineTable -> SubroutineArgumentTranslationMap -> Fortran Anno -> Fortran Anno
flattenSubroutineCall_container subTable argTransTable containerSeg     
    |     containedCall /= [] = containerWithFlattenedSub                                                                        
    |    otherwise = containerSeg                                                        
        where
            -- Get the call statement
            containedCall = foldl (++) [] (gmapQ (mkQ [] extractCalls) containerSeg)
            (Call _ _ callExpr _) = if (length containedCall > 1) then error "flattenSubroutineCall_container: multiple contained calls unsupported" else head containedCall
            -- get the subroutine name
            subroutineName = if extractVarNames callExpr == [] 
                then (error "flattenSubroutineCall: callExpr\n" ++ (show callExpr))  
                else varNameStr (head (extractVarNames callExpr))
            -- get the source span of the subroutine iff it is in the table, so only "known" subs
            subroutineSrc = case DMap.lookup subroutineName subTable of
                                        Nothing -> nullSrcSpan
                                        Just subroutine -> srcSpan (subroutineTable_ast subroutine)
            subroutineLineSize = srcSpanLineCount subroutineSrc
            -- adapt the src spans to account for the inlined code
            containerWithScrShifts = gmapT (mkT (ignoreCall_T (shiftSrcSpanLineGlobal subroutineLineSize))) containerSeg
            --  for inlining we need to replace the signature arguments with the call arguments
            --  first get the argument translation table
            subroutineArgTrans = DMap.findWithDefault (error "flattenSubroutineCall_container: subroutineArgTrans") subroutineName argTransTable
            -- now use this to acutally "flatten" the subroutine
            containerWithFlattenedSub = gmapT (mkT (flattenSubroutineCall subTable subroutineArgTrans)) containerWithScrShifts

flattenSubroutineCall :: SubroutineTable -> ArgumentTranslation -> Fortran Anno -> Fortran Anno
flattenSubroutineCall subTable argTransTable (Call anno cSrc callExpr args) = fromMaybe callFortran shiftedSubroutineReplacement
        where
            callFortran = (Call anno cSrc callExpr args)
            subroutineName = if extractVarNames callExpr == [] 
                then (error "flattenSubroutineCall:callExpr\n" ++ (show callExpr))  
                else varNameStr (head (extractVarNames callExpr))
            subroutineReplacement = case DMap.lookup subroutineName subTable of
                                        Nothing -> Nothing
                                        Just subroutine -> Just (substituteArguments argTransTable callFortran (subroutineTable_ast subroutine))

            subroutineSrc = srcSpan (fromMaybe (error "flattenSubroutineCall: subroutineSrc") subroutineReplacement)
            ((SrcLoc _ callLineStart _), _) =  cSrc
            ((SrcLoc _ bodyLineStart _), _) =  subroutineSrc
            bodyOffset = (callLineStart -  bodyLineStart) + 1

            shiftedSubroutineReplacement = case subroutineReplacement of
                                                Nothing -> Nothing
                                                Just rep -> Just (shiftSrcSpanLineGlobal bodyOffset rep) 
flattenSubroutineCall subTable argTransTable codeSeg = codeSeg

--    The following four functions deal with placing buffer reads into the AST of the main. These reads happen when a value has been calculated
--    on the device but is then used by the host. Given a point in the main AST where the OpenCL work is done, the AST and a list of variables
--    that exist on the device at the end of its use, the following happens.
--    STEPS
--        -    Find Assignments after the provided SrcSpan that read one of the vars on the device.
--        -    Insert an OpenCLBufferRead before the assignment
insertBufferReads mainAst varsOnDevice openCLendSrc = everywhere (mkT (insertBufferReads_block varsOnDevice openCLendSrc)) mainAst

insertBufferReads_block :: [VarName Anno] -> SrcSpan -> Block Anno -> Block Anno
insertBufferReads_block varsOnDevice afterSrc inputBlock = gmapT (mkT (insertBufferReads_fortran varsOnDevice afterSrc)) inputBlock

insertBufferReads_fortran :: [VarName Anno] -> SrcSpan -> Fortran Anno -> Fortran Anno
insertBufferReads_fortran varsOnDevice afterSrc (FSeq fseqAnno fseqSrc (Assg assgAnno assgSrc expr1 expr2) fortran2)     |     rightLocation = gmapT (mkT (insertBufferReads_fortran newVarsOnDevice afterSrc)) bufferReadFortran
                                                                                                                        |    otherwise =     gmapT (mkT (insertBufferReads_fortran varsOnDevice afterSrc)) codeSeg
        where
            codeSeg = (FSeq fseqAnno fseqSrc (Assg assgAnno assgSrc expr1 expr2) fortran2)
            rightLocation = checkSrcSpanBefore afterSrc assgSrc 
            readVarNames = listRemoveDuplications ((extractAllVarNames expr2) ++ (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars expr1)))
            readVarNamesOnDevice = listIntersection varsOnDevice readVarNames

            bufferReadFortran = buildBufferReadFortran readVarNamesOnDevice codeSeg

            newVarsOnDevice = listSubtract varsOnDevice readVarNamesOnDevice
insertBufferReads_fortran varsOnDevice afterSrc codeSeg     |     rightLocation = gmapT (mkT (insertBufferReads_fortran newVarsOnDevice afterSrc)) bufferReadFortran
                                                            |    otherwise =     gmapT (mkT (insertBufferReads_fortran varsOnDevice afterSrc)) codeSeg
        where
            exprs = foldl (++) [] (gmapQ (mkQ [] extractExpr_list) codeSeg)
            rightLocation = checkSrcSpanBefore afterSrc (srcSpan codeSeg)
            readVarNames = listRemoveDuplications (extractAllVarNames codeSeg)
            readVarNamesOnDevice = listIntersection varsOnDevice readVarNames

            bufferReadFortran = buildBufferReadFortran readVarNamesOnDevice codeSeg
            newVarsOnDevice = listSubtract varsOnDevice readVarNamesOnDevice

--    This following 4 functions allow for buffer reads and writes to be inserted at certain line numbers within the file.
--    All 4 are given an AST and a list of (varName, srcSpan (read: line number)) pairs. The differences between the '..before'
--    and '..after' functions are that the 'before' functions insert the buffer access BEFORE the supplied srcSpan and the
--    'after' functions place the buffer access AFTER the srcSpan. 
insertBufferReadsAfter varSrcPairs ast = everywhere (mkT (insertFortranAfter_block fortranSrcPairs)) ast
        where 
            fortranSrcPairs = map (\(var, src) -> (OpenCLBufferRead nullAnno src var, src)) varSrcPairs

insertBufferReadsBefore varSrcPairs ast = everywhere (mkT (insertFortranBefore_block fortranSrcPairs)) ast
        where 
            fortranSrcPairs = map (\(var, src) -> (OpenCLBufferRead nullAnno src var, src)) varSrcPairs

insertBufferWritesAfter :: [(VarName Anno, SrcSpan)] -> Fortran Anno -> Fortran Anno     
insertBufferWritesAfter varSrcPairs ast = everywhere (mkT (insertFortranAfter_block fortranSrcPairs )) ast -- this does show p2
        where 
            fortranSrcPairs = map (\(var, src) -> (OpenCLBufferWrite nullAnno src var, src)) varSrcPairs

insertBufferWritesBefore varSrcPairs ast = everywhere (mkT (insertFortranBefore_block fortranSrcPairs)) ast
        where 
            fortranSrcPairs = map (\(var, src) -> (OpenCLBufferWrite nullAnno src var, src)) varSrcPairs

insertFortranAfter_block :: [(Fortran Anno, SrcSpan)] -> Block Anno -> Block Anno
insertFortranAfter_block fortranSrcPairs (Block anno useBlock imp src decl fortran) = Block anno useBlock imp src decl (insertFortranAfter_fortran fortranSrcPairs fortran) -- (warning fortranSrcPairs ("BLOCK: "++(show fortranSrcPairs))) fortran) -- this does show p2

insertFortranBefore_block :: [(Fortran Anno, SrcSpan)] -> Block Anno -> Block Anno
insertFortranBefore_block fortranSrcPairs (Block anno useBlock imp src decl fortran) = Block anno useBlock imp src decl (insertFortranBefore_fortran fortranSrcPairs fortran)

insertFortranAfter_fortran :: [(Fortran Anno, SrcSpan)] -> Fortran Anno -> Fortran Anno
insertFortranAfter_fortran fortranSrcPairs ast = foldl' (\astAccum (fort, src) -> insertAfterSrc fort src astAccum) ast fortranSrcPairs -- (warning fortranSrcPairs (show fortranSrcPairs))-- HERE I've lost p2 !!! also when I reverse the list, so it is the value that causes this.

insertFortranBefore_fortran :: [(Fortran Anno, SrcSpan)] -> Fortran Anno -> Fortran Anno
insertFortranBefore_fortran fortranSrcPairs ast = foldl' (\astAccum (fort, src) -> insertBeforeSrc fort src astAccum) ast fortranSrcPairs

insertAfterSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertAfterSrc newFortran@(OpenCLBufferWrite _ _ (VarName _ var)) src ast = 
    if var == "p2" 
        then insertFortranAfterSrcWV (warning newFortran  ("WARNING:" ++(show (newFortran, src)))) src ast -- So this warning is never printed. 
        else insertFortranAfterSrc newFortran src ast

insertFortranAfterSrcWV :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertFortranAfterSrcWV newFortran src targetFortran =  let
    src' = case targetFortran of 
        Assg _ srcspan _ _ -> srcspan
        _ -> nullSrcSpan
    in
        if src == src' then
            error $ show (newFortran,targetFortran)    
        else
            warning targetFortran ("LINE: "++(show targetFortran)++"\n\n")

-- original code
-- insertAfterSrc newFortran src ast = insertFortranAfterSrc newFortran src ast

insertBeforeSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertBeforeSrc newFortran src ast = insertFortranBeforeSrc newFortran src ast

-- so I should create a dummy here and abort on p2

insertFortranAfterSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertFortranAfterSrc newFortran src (FSeq fseqAnno fseqSrc fortran1 fortran2)     
            |    correctPosition = FSeq fseqAnno fseqSrc fortran1 (FSeq nullAnno nullSrcSpan newFortran fortran2) -- (warning newFortran ("insertFortranAfterSrc FSeq correctPosition "++(show newFortran))) fortran2)
            |    otherwise       = FSeq fseqAnno fseqSrc updateFortran1 updateFortran2
        where
--            ((SrcLoc _ startLine _), (SrcLoc _ endLine _)) = src
--            ((SrcLoc _ f1LineStart _), (SrcLoc _ f1LineEnd _)) = srcSpan fortran1
            correctPosition = src == srcSpan fortran1
            updateFortran1 = insertFortranAfterSrc newFortran src fortran1 -- (warning newFortran ("insertFortranAfterSrc FSeq NOT correctPosition 1 "++(show newFortran))) src fortran1
            updateFortran2 = if (updateFortran1 == fortran1) then insertFortranAfterSrc newFortran src fortran2 else fortran2
--            updateFortran2 = if (updateFortran1 == fortran1) then insertFortranAfterSrc (warning newFortran ("insertFortranAfterSrc FSeq NOT correctPosition 2 "++(show newFortran))) src fortran2 else fortran2
insertFortranAfterSrc newFortran src codeSeg     
            |    correctPosition = FSeq nullAnno nullSrcSpan codeSeg newFortran -- (warning newFortran ("insertFortranAfterSrc codeSeg correctPosition "++(show newFortran))) 
            |    otherwise = gmapT (mkT (insertFortranAfterSrc newFortran src)) codeSeg
--            |    otherwise = gmapT (mkT (insertFortranAfterSrc (warning newFortran ("insertFortranAfterSrc codeSeg NOT correctPosition"++(show newFortran))) src)) codeSeg
        where
--            ((SrcLoc _ startLine _), _) = src
--            ((SrcLoc _ codeSegLineStart _), _) = srcSpan codeSeg
            correctPosition = src == srcSpan codeSeg

insertFortranBeforeSrc :: Fortran Anno -> SrcSpan -> Fortran Anno -> Fortran Anno
insertFortranBeforeSrc newFortran src (FSeq fseqAnno fseqSrc fortran1 fortran2)     |    correctPosition = FSeq nullAnno nullSrcSpan newFortran ((FSeq fseqAnno fseqSrc fortran1 fortran2)) -- (FSeq fseqAnno fseqSrc fortran1 (FSeq nullAnno nullSrcSpan newFortran fortran2))
                                                                                    |    otherwise = FSeq fseqAnno fseqSrc updateFortran1 updateFortran2
        where
--            ((SrcLoc _ startLine _), (SrcLoc _ endLine _)) = src
--            ((SrcLoc _ f1LineStart _), (SrcLoc _ f1LineEnd _)) = srcSpan fortran1
            correctPosition = src == srcSpan fortran1
            updateFortran1 = insertFortranBeforeSrc newFortran src fortran1
            updateFortran2 = if (updateFortran1 == fortran1) then insertFortranBeforeSrc newFortran src fortran2 else fortran2
insertFortranBeforeSrc newFortran src codeSeg     |    correctPosition = FSeq nullAnno nullSrcSpan newFortran codeSeg
                                                |    otherwise = gmapT (mkT (insertFortranBeforeSrc newFortran src)) codeSeg
        where
--            ((SrcLoc _ startLine _), _) = src
--            ((SrcLoc _ codeSegLineStart _), _) = srcSpan codeSeg
            correctPosition = src == srcSpan codeSeg

buildBufferReadFortran :: [VarName Anno] -> Fortran Anno -> Fortran Anno
buildBufferReadFortran [] followingFortran = followingFortran
buildBufferReadFortran (varToRead:[]) followingFortran = FSeq nullAnno nullSrcSpan oclRead followingFortran
        where
            oclRead = (OpenCLBufferRead nullAnno nullSrcSpan varToRead)
buildBufferReadFortran (varToRead:vars) followingFortran = FSeq nullAnno nullSrcSpan oclRead (buildBufferReadFortran vars followingFortran)
        where
            oclRead = (OpenCLBufferRead nullAnno nullSrcSpan varToRead)

buildBufferWriteFortran :: [VarName Anno] -> Fortran Anno -> Fortran Anno
buildBufferWriteFortran [] followingFortran = followingFortran
buildBufferWriteFortran (varToWrite:[]) followingFortran = FSeq nullAnno nullSrcSpan oclWrite followingFortran
        where
            oclWrite = (OpenCLBufferWrite nullAnno nullSrcSpan varToWrite)
buildBufferWriteFortran (varToWrite:vars) followingFortran = FSeq nullAnno nullSrcSpan oclWrite (buildBufferWriteFortran vars followingFortran)
        where
            oclWrite = (OpenCLBufferWrite nullAnno nullSrcSpan varToWrite)

--    The following function determines which variables should be 'initialised' before any OpenCL kernel call. This is done by considering the kernels in order to find kernel arguments that
--    are read by the kernel before any writen occurs on them. As in, the kernels expect that the variable/argument already exists on the device.
extractSubroutineInitBufferWrites :: ProgUnit Anno -> [VarName Anno]
extractSubroutineInitBufferWrites ast = fst (foldl (\(accum_r, accum_w) (item_r, item_w) -> (accum_r ++ (listSubtract item_r accum_w), accum_w ++ item_w)) ([], []) (zip reads writes))
        where
            kernels = extractKernels ast
            reads = map extractKernelReads kernels
            writes = map extractKernelWrites kernels

--     The following function determines which variables will exist on the device when all of the OpenCL work is done. Using a similar strategy to the function above, finds the arguments that
--    are written by the kernel without any following kernel reading them afterward. 
extractSubroutineFinalBufferReads :: ProgUnit Anno -> [VarName Anno]
extractSubroutineFinalBufferReads ast = snd (foldl (\(accum_r, accum_w) (item_r, item_w) -> (accum_r ++ item_r, accum_w ++ (listSubtract item_w accum_r))) ([], []) (zip reads writes))
        where
            kernels = extractKernels ast
            reads = reverse $ map extractKernelReads kernels
            writes = reverse $ map extractKernelWrites kernels

generateKernelCallSrcRange :: SubroutineTable -> Program Anno -> (SrcSpan, SrcSpan)
generateKernelCallSrcRange subTable ast = (kernelsStart, kernelsEnd)
        where
            callsAndStrings = everything (++) (mkQ [] extractCallsWithStrings) ast
            callSrcs = map (\(callCode, callString) -> srcSpan callCode) (filter (\(callCode, callString) -> elem callString (DMap.keys subTable)) callsAndStrings)
            kernelsStart = (fromMaybe (error "generateKernelCallSrcRange") (getEarliestSrcSpan callSrcs))
            kernelsEnd = (fromMaybe (error "generateKernelCallSrcRange") (getLatestSrcSpan callSrcs))
            -- kernelsStart = fst (fromMaybe (error "generateKernelCallSrcRange") (getEarliestSrcSpan callSrcs))
            -- kernelsEnd = snd (fromMaybe (error "generateKernelCallSrcRange") (getLatestSrcSpan callSrcs))

optimseBufferTransfers_program :: VarAccessAnalysis -> Program Anno -> Program Anno
optimseBufferTransfers_program varAccessAnalysis ast = ast_optimisedBetweenKernels
        where
            kernels = extractKernels ast
            ast_optimisedBetweenKernels = compareKernelsInOrder varAccessAnalysis kernels ast
--            kernels_optimisedBetween = extractKernels ast_optimisedBetweenKernels

findEarliestInitialisationSrcSpan :: VarAccessAnalysis -> SrcSpan -> [VarName Anno] -> SrcSpan
findEarliestInitialisationSrcSpan varAccessAnalysis kernelsRange initWrites = lastWrite
        where
            writesBefore = foldl (\accum item -> accum ++ snd (getAccessLocationsBeforeSrcSpan varAccessAnalysis item kernelsRange)) [] initWrites
            lastWrite = fromMaybe nullSrcSpan (getLatestSrcSpan writesBefore)

findLatestTearDownSrcSpan :: VarAccessAnalysis -> SrcSpan -> [VarName Anno] -> SrcSpan
findLatestTearDownSrcSpan varAccessAnalysis kernelsRange tearDownReads = firstRead
        where
            readsAfter = foldl (\accum item -> accum ++ fst (getAccessLocationsAfterSrcSpan varAccessAnalysis item kernelsRange)) [] tearDownReads
            kernelsEndLine = (snd kernelsRange, snd kernelsRange)
            firstRead = fromMaybe kernelsEndLine (getEarliestSrcSpan readsAfter)

--    Using the variable access analysis from the flattened main AST, strip away those arguments that the kernel writes back to the host that are never read
--    by the host after the end of the kernel and before the end of ALL of the kernels (As in, there is no need for the variable to be on the host while OpenCL stuff is
--    still happening. Similarily, strip away any arguments that the kernel reads from the host that are not written to by the host between kernels.
stripInitAndTearDown :: VarAccessAnalysis -> SrcSpan -> [Fortran Anno] -> ([Fortran Anno], [VarName Anno], [VarName Anno])
stripInitAndTearDown _ _ [] = ([], [], [])
stripInitAndTearDown varAccessAnalysis (kernelsStart, kernelsEnd) kernels = (newCurrentKernel:recursiveKernels, initialisingWrites ++ recursiveInitialisingWrites, tearDownReads ++ recursiveTearDownReads)
        where
            currentKernel = head kernels
            (currentSrcStart, currentSrcEnd) = srcSpan currentKernel
            bufferWrites = extractKernelReads currentKernel -- buffer writes are for variables that are read by the kernel
            bufferReads = extractKernelWrites currentKernel -- buffer reads are for variables that are written to by the kernel
            (_, writesBeforeCurrentKernel) = getAccessesBetweenSrcSpans varAccessAnalysis kernelsStart currentSrcStart
            (readsAfterCurrentKernel, _) = getAccessesBetweenSrcSpans varAccessAnalysis currentSrcEnd kernelsEnd
            -- WV: all elts of bufferWrites that are not in  writesBeforeCurrentKernel
            --
            initialisingWrites = listSubtract bufferWrites writesBeforeCurrentKernel
            tearDownReads = listSubtract bufferReads readsAfterCurrentKernel
            -- WV: a safe way to deal with iterative loops is to remove these from the bufferWrites, then add them
            iterLoopVars = getIterLoopVars currentKernel
            iterLoopVarsInKernel = filter (\elt -> elt `elem` bufferWrites) iterLoopVars
            bufferWritesNoIterLoopVars = filter (\elt -> not (elt `elem` bufferWrites)) iterLoopVars
            newBufferWrites = (listSubtract bufferWrites initialisingWrites)++iterLoopVarsInKernel
            newBufferWrites' = newBufferWrites
--            warning newBufferWrites (
--                "KERNEL: "++(miniPPF currentKernel)++
--                "\nBUFFER WRITES: "++
--                "\nBEFORE: "++ (showVarLst writesBeforeCurrentKernel) ++
--                "\nINIT: "++ (showVarLst initialisingWrites) ++
--                "\nNEW:"++(showVarLst newBufferWrites)++
--                "\nOLD:"++(showVarLst bufferWrites))
            newBufferReads = listSubtract bufferReads tearDownReads
            
            newCurrentKernel = replaceKernelWrites (replaceKernelReads currentKernel newBufferWrites') newBufferReads

            (recursiveKernels, recursiveInitialisingWrites, recursiveTearDownReads) = stripInitAndTearDown varAccessAnalysis (kernelsStart, kernelsEnd) (tail kernels)


--    Taking the kernels in the order that they are called by the subroutines, and in the order that the subroutines are called by the host, eliminate pairs of
--    arguments that cancel each other out. For example, kernel A writes back var x and kernel B reads it again soon after, with no host interaction - in this case,
--    both the read and the write of x can be removed from the respective kernels.
compareKernelsInOrder :: VarAccessAnalysis -> [Fortran Anno] -> Program Anno -> Program Anno
compareKernelsInOrder varAccessAnalysis [] ast = ast
compareKernelsInOrder varAccessAnalysis kernels ast = compareKernelsInOrder varAccessAnalysis (newKernels) newAst
        where
            currentKernel = head kernels
            (newFirstKernel, newKernels) = eliminateBufferPairsKernel_recurse varAccessAnalysis currentKernel (tail kernels) []
            newAst = foldl (\accumAst (oldFortran, newFortran) -> replaceFortran accumAst oldFortran newFortran) ast (zip kernels (newFirstKernel:newKernels))

eliminateBufferPairsKernel_recurse :: VarAccessAnalysis -> Fortran Anno -> [Fortran Anno] -> [SrcSpan] -> (Fortran Anno, [Fortran Anno])
eliminateBufferPairsKernel_recurse varAccessAnalysis firstKernel [] ignoredSpans = (firstKernel, [])
eliminateBufferPairsKernel_recurse varAccessAnalysis firstKernel kernels ignoredSpans = (resursiveCall_firstKernel, newSecondKernel:resursiveCall_kernels)
        where
            secondKernel = head kernels
            (newFirstKernel, newSecondKernel) = eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel
            (resursiveCall_firstKernel, resursiveCall_kernels) = eliminateBufferPairsKernel_recurse varAccessAnalysis newFirstKernel (tail kernels) ((srcSpan secondKernel):ignoredSpans)

eliminateBufferPairsKernel :: VarAccessAnalysis -> [SrcSpan] -> Fortran Anno -> Fortran Anno -> (Fortran Anno, Fortran Anno)
eliminateBufferPairsKernel varAccessAnalysis ignoredSpans firstKernel secondKernel = (newFirstKernel, newSecondKernel) -- warning (newFirstKernel, newSecondKernel) debugMessage
        where
--            debugMessage = "\n\nNewFirstKernel:\n" ++ (miniPPF newFirstKernel) ++ "\n\nNewSecondKernel:\n" ++ (miniPPF newSecondKernel)
--                            ++ "\n\n readsBetween:\n" ++ (show $ map (\(VarName _ v) -> v) readsBetween) ++ "\n\n writesBetween:\n" ++ (show $ map (\(VarName _ v) -> v) writesBetween)
--                            ++ "\n\n firstBufferReads:\n" ++ (show $ map (\(VarName _ v) -> v) firstBufferReads) ++ "\n\n newFirstBufferReads:\n" ++ (show $ map (\(VarName _ v) -> v) newFirstBufferReads)
--                            ++ "\n\n secondBufferWrites:\n" ++ (show $ map (\(VarName _ v) -> v) secondBufferWrites) ++ "\n\n newSecondBufferWrites:\n" ++ (show $ map (\(VarName _ v) -> v) newSecondBufferWrites)    

            firstKernel_src = srcSpan firstKernel
            secondKernel_src = srcSpan secondKernel

            (readsBetween, writesBetween) = getAccessesBetweenSrcSpansIgnore varAccessAnalysis firstKernel_src secondKernel_src ignoredSpans

            --     Rather confusingly, the reads I am refering to here are the buffer reads that must occur 
            --    AFTER the kernel call and NOT the arguments that are 'read' by the kernel. As it turns out,
            --    the variables that I am interested in are the variables that the kernel views as 'writes'.
            --    The equivalent is true for the secondBufferWrites variable. Not the written arguments but 
            --     the buffers that must be written to before the start of the kernel.
            firstBufferReads = extractKernelWrites firstKernel
            firstBufferWrites = extractKernelReads firstKernel
            secondBufferReads = extractKernelWrites secondKernel
            secondBufferWrites = extractKernelReads secondKernel
            --    SIMPLE CASE wihtout any analysis between kernels
            --        newFirstBufferReads = listSubtract firstBufferReads secondBufferWrites
            --        newsecondBufferWrites = listSubtract secondBufferWrites firstBufferReads

            --    More complex analysis would differentiate between reads and writes between kernels.
            -- newFirstBufferReads = listSubtractWithExemption (readsBetween ++ writesBetween) firstBufferReads secondBufferWrites
            -- newSecondBufferWrites = listSubtractWithExemption (readsBetween ++ writesBetween) secondBufferWrites firstBufferReads 

            newSecondBufferWrites_preCrossOver = listSubtractWithExemption (writesBetween) secondBufferWrites firstBufferWrites 
            newSecondBufferWrites = listSubtractWithExemption (writesBetween) newSecondBufferWrites_preCrossOver firstBufferReads
            newFirstBufferReads_crossOver = listSubtractWithExemption (readsBetween) firstBufferReads newSecondBufferWrites
            newFirstBufferReads = listSubtractWithExemption (readsBetween) newFirstBufferReads_crossOver secondBufferReads

            k1src = errorLocationFormatting (srcSpan firstKernel)
            k2src = errorLocationFormatting (srcSpan secondKernel)

            newFirstKernel = replaceKernelWrites firstKernel newFirstBufferReads
            newSecondKernel = replaceKernelReads secondKernel newSecondBufferWrites

--
--    UTILITIES
--

extractKernelReads :: Fortran Anno -> [VarName Anno]
extractKernelReads codeSeg = case codeSeg of
                OpenCLMap _ _ reads _ _ _ _ -> reads -- WV20170426
                OpenCLReduce _ _ reads _ _ _ _ _ -> reads -- WV20170426
                _ -> error "extractKernelReads: not a kernel"

extractKernelWrites :: Fortran Anno -> [VarName Anno]
extractKernelWrites codeSeg = case codeSeg of
                OpenCLMap _ _ _ writes _ _ _ -> writes -- WV20170426
                OpenCLReduce _ _ _ writes _ _ _ _ -> writes -- WV20170426
                _ -> error "extractKernelWrites: not a kernel"

replaceKernelReads :: Fortran Anno -> [VarName Anno] -> Fortran Anno
replaceKernelReads codeSeg newReads = case codeSeg of
                OpenCLMap anno src reads writes loopV iterLoopV fortran -> OpenCLMap anno src newReads writes loopV iterLoopV fortran -- WV20170426
                OpenCLReduce anno src reads writes loopV iterLoopV redV fortran -> OpenCLReduce anno src newReads writes loopV iterLoopV redV fortran -- WV20170426
                _ -> error "replaceKernelReads: not a kernel"

replaceKernelWrites :: Fortran Anno -> [VarName Anno] -> Fortran Anno
replaceKernelWrites codeSeg newWrites = case codeSeg of
                OpenCLMap anno src reads writes loopV iterLoopV fortran -> OpenCLMap anno src reads newWrites loopV iterLoopV fortran -- WV20170426
                OpenCLReduce anno src reads writes loopV iterLoopV redV fortran -> OpenCLReduce anno src reads newWrites loopV iterLoopV redV fortran -- WV20170426
                _ -> error "replaceKernelWrites: not a kernel"


ignoreCall_T :: (Fortran Anno -> Fortran Anno) -> Fortran Anno -> Fortran Anno
ignoreCall_T func codeSeg = case codeSeg of
                                Call _ _ _ _ -> codeSeg
                                _ -> func codeSeg

extractCallsWithStrings :: Fortran Anno -> [(Fortran Anno, String)]
extractCallsWithStrings codeSeg = case codeSeg of
                            Call _ _ callExpr _ -> [(codeSeg, varNameStr $ head (extractVarNames callExpr))]
                            _ -> []

substituteArguments :: ArgumentTranslation -> Fortran Anno -> ProgUnit Anno -> (Fortran Anno)
substituteArguments argTransTable (Call _ _ _ arglist) (Sub _ _ _ _ arg (Block _ _ _ _ _ for)) = everywhere (mkT (replaceArgs argTransTable)) for
-- WV: TODO: this allows only to substitute a variable name with another variable name 
-- A more complete approach is to allow substitution of expressions
-- However, to make this work for e.g. p2(0,:,:,:) would mean replacing p(i,j,k) with p2(0,i,j,k), this is far from obvious
replaceArgs :: DMap.Map (VarName Anno) (VarName Anno) -> VarName Anno -> VarName Anno
replaceArgs varNameReplacements varname = DMap.findWithDefault varname varname varNameReplacements

extractCalledSubroutines :: Program Anno -> [(String, SrcSpan)]
extractCalledSubroutines ast = everything (++) (mkQ [] extractCalledSubroutines_fortran) ast

extractCalledSubroutines_fortran :: Fortran Anno -> [(String, SrcSpan)]
extractCalledSubroutines_fortran (Call _ src expr _)     |    isVar expr = map (\x -> (varNameStr x, src)) (extractVarNames expr)
                                                        |    otherwise = error "extractCalledSubroutines_fortran: not var"
extractCalledSubroutines_fortran _ = []
