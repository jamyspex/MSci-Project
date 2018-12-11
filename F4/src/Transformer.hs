module Transformer                 (paralleliseProgUnit_foldl, combineKernelProgUnit_foldl)

where

--    This module handles the parallelism detection of the compiler, along with loop/kernel fusion. The main method first calls 'paralleliseProgUnit_foldl' in a left fold
--    with a table of subroutines. The result there is a new subroutine table with parallelised subroutines along with a set of parallelism errors (annotations) that are
--    to be output to the user. The parallelism analysis code is housed in 'LoopAnalysis.hs' but this module performs the transformations to abstract syntax trees in
--    order to include OpenCLMap and OpenCLReduce nodes.

--    After the parallelism is detected, the main makes calls to 'combineKernelProgUnit_foldl', again in a left fold and with a table of subroutines. This function returns
--    a new table of subroutines, with fewer kernels hopefully, along with some strings to be output to the user. The strings in this case describe which (if any) loop fusions
--    have been performed. These messages, as well as the parallelism errors mentioned previously, are only reported in verbose mode ('-v' command line argument)

import           Warning

import           Control.Monad
import           Data.Char
import           Data.Generics           (Data, Typeable, everything,
                                          everywhere, gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.Map                as DMap
import           Language.Fortran
import           Language.Fortran.Parser
import           System.Directory
import           System.Environment
import           System.Process

import           CombineKernels          (combineKernelsProgUnit)
import           ConstantFolding         (foldConstants)
import           LanguageFortranTools
import           LoopAnalysis            (analyseLoop_map, analyseLoop_reduce,
                                          getErrorAnnotations, getReads,
                                          getReductionVarNames, getWrites)
import           MiniPP                  (miniPPF)
import           Parser                  (SubRec (..), SubroutineTable)
import           VarAccessAnalysis       (VarAccessAnalysis,
                                          analyseAllVarAccess_progUnit,
                                          getNonTempVars, getPrexistingVars,
                                          getValueAtSrcSpan)
import           VarDependencyAnalysis   (VarDependencyAnalysis,
                                          analyseDependencies,
                                          loopCarriedDependencyCheck,
                                          loopCarriedDependencyCheck_reductionWithIteration)
-- import FortranSynthesiser ( produceCode_progUnit )
{-
WV:
paralleliseProgUnit_foldl
    ConstantFolding::foldConstants
    analyseAllVarAccess_progUnit
    paralleliseBlock
        isolateAndParalleliseForLoops
            paralleliseLoop
                paralleliseLoop_map
                    VarAccessAnalysis::getPrexistingVars
                    LoopAnalysis::paralleliseLoop_map
                paralleliseLoop_reduce
                    LoopAnalysis::analyseLoop_reduce
                paralleliseLoop_reduceWithOuterIteration

This works on a full subroutine. So in principle after this is done, the var decls should be fine.
data SubroutineTable = DMap.Map String (ProgUnit Anno, String)
annoListing :: [(String, String)] : append (filename, parAnno) where parAnno contains all of the parallelising errors for this particular run of the compiler.

combineKernelProgUnit_foldl
    CombineKernels::combineKernelsProgUnit
        LanguageFortranTools::removeAllAnnotations
    compileAnnotationListing

-}
paralleliseProgUnit_foldl :: [String] -> SubroutineTable -> (SubroutineTable, [(String, String)]) -> String -> (SubroutineTable, [(String, String)])
paralleliseProgUnit_foldl ioWriteSubroutines originalTable (accumSubTable, annoListing) subName = (newSubTable, annoListing ++ [(filename, parAnno)])
        where
            subrec = DMap.findWithDefault (error "paralleliseProgUnit_foldl") subName originalTable
            progUnit = subAst subrec
            filename = subSrcFile subrec
            progUnitfoldedConstants = foldConstants progUnit
            accessAnalysis = analyseAllVarAccess_progUnit ioWriteSubroutines progUnit
            parallelisedProgUnit = everywhere (mkT (paralleliseBlock filename originalTable accessAnalysis)) progUnitfoldedConstants
            parAnno = compileAnnotationListing parallelisedProgUnit -- (warning  (show parallelisedProgUnit)) -- (produceCode_progUnit DMap.empty DMap.empty ([],"") "kernel_module" "superkernel" [] parallelisedProgUnit)))
            newSubTable = DMap.insert subName
                (MkSubRec parallelisedProgUnit filename [] subName (argTranslations subrec) (parallelise subrec)) accumSubTable

-- This one merges loops so again it should not touch the vardecls in the sub
combineKernelProgUnit_foldl :: Maybe Float -> (SubroutineTable, [(String, String)]) -> String -> (SubroutineTable, [(String, String)])
combineKernelProgUnit_foldl loopFusionBound (subTable, annoListing) subName = (newSubTable, annoListing ++ [(filename, combAnno)])
        where
        -- get the record from the subroutine table
            subrec = DMap.findWithDefault (error "paralleliseProgUnit_foldl") subName subTable
            -- get the program unit, i.e. in practice the subroutine AST, from the record
            progUnit = subAst subrec
            -- get the filename from the record
            filename = subSrcFile subrec
            -- the actual loop fusion call. removeAllAnnotations is called because after the fusion the annotations are meaningless
            combinedProgUnit = combineKernelsProgUnit loopFusionBound (removeAllAnnotations progUnit)
            combAnno = compileAnnotationListing combinedProgUnit
            newSubTable = DMap.insert subName
                (MkSubRec combinedProgUnit filename [] subName (argTranslations subrec) (parallelise subrec)) subTable

--    This function is called using generics so that every 'Block' is traversed. This step is necessary to be able to reach the first 'Fortran'
--    node. From here, the first call to 'isolateAndParalleliseForLoops' is made (again with generics) which recursively traverses the 'Fortran' nodes to
--    find for loop that should be analysed
paralleliseBlock :: String -> SubroutineTable -> VarAccessAnalysis -> Block Anno -> Block Anno
paralleliseBlock filename subTable accessAnalysis block = gmapT (mkT (isolateAndParalleliseForLoops filename subTable accessAnalysis)) block

--    Function traverses the 'Fortran' nodes to find For loops. It calls 'paralleliseLoop' on identified for loops in a recusive way such that the most
--    nested loops in a cluster are analysed first.
isolateAndParalleliseForLoops :: String -> SubroutineTable -> VarAccessAnalysis -> Fortran Anno -> Fortran Anno
isolateAndParalleliseForLoops filename subTable accessAnalysis inp = case inp of
        For _ _ _ _ _ _ _ -> paralleliseLoop filename [] accessAnalysis subTable recusivelyAnalysedNode
            where
                recusivelyAnalysedNode = gmapT (mkT (isolateAndParalleliseForLoops filename subTable accessAnalysis )) inp
        _ -> gmapT (mkT (isolateAndParalleliseForLoops filename subTable accessAnalysis)) inp

--    Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--    nodes or the original sub-tree annotated with parallelisation errors. Attempts to determine whether the loop is a 'map', then checks
--    for whether it's a 'reduce'. Finally, it checks whether the loop is a reduction with an outer iteration. If any of these structures
--     are detected, the AST node is tranformed appropriately and placed back into the AST.
--     WV: This is restrictive because the inner loop nest could be a map as well, or even a combination
--
paralleliseLoop :: String -> [VarName Anno] -> VarAccessAnalysis -> SubroutineTable -> Fortran Anno -> Fortran Anno
paralleliseLoop filename loopVars accessAnalysis subTable loop = transformedAst
                                where
                                    newLoopVars = case getLoopVar loop of
                                        Just a  -> loopVars ++ [a]
                                        Nothing -> loopVars

                                    nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis
                                    prexistingVars = getPrexistingVars (srcSpan loop) accessAnalysis
                                    dependencies = analyseDependencies loop

                                    --    If the 'bool' variable for any of the attempts to parallelise is true, then parallism has been found
                                    --    and the new AST node is returned from this function, to be placed in the AST by the calling function.
                                    --
                                    mapAttempt = paralleliseLoop_map filename loop newLoopVars nonTempVars prexistingVars dependencies accessAnalysis subTable
                                    mapAttempt_bool = fst mapAttempt
                                    mapAttempt_ast = snd mapAttempt

                                    reduceAttempt = paralleliseLoop_reduce filename mapAttempt_ast newLoopVars nonTempVars prexistingVars  dependencies accessAnalysis
                                    reduceAttempt_bool = fst reduceAttempt
                                    reduceAttempt_ast = snd reduceAttempt

                                    reduceWithOuterIterationAttempt = paralleliseLoop_reduceWithOuterIteration filename reduceAttempt_ast Nothing newLoopVars newLoopVars nonTempVars prexistingVars dependencies accessAnalysis
                                    reduceWithOuterIterationAttempt_bool = fst reduceWithOuterIterationAttempt
                                    reduceWithOuterIterationAttempt_ast = snd reduceWithOuterIterationAttempt
                                    -- WV: TODO: if all these fail we should move the loop to the OpenCL device anyway, using a new OpenCLSeq node

                                    transformedAst = case mapAttempt_bool of
                                        True    ->  mapAttempt_ast
                                        False     -> case reduceAttempt_bool of
                                                    True     -> reduceAttempt_ast
                                                    False    -> reduceWithOuterIterationAttempt_ast

--    These functions are used to extract a list of varnames that are written to in a particular chunk of code.
--    WV: TODO: what about subroutine calls in the loop?
extractWrites_query :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites_query = everything (++) (mkQ [] extractWrites)

extractWrites :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites (Assg _ _ (Var _ _ list) _) = map (\(varname, exprs) -> varname) list
extractWrites _ = []

--    Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--    original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: String -> Fortran Anno -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> SubroutineTable -> (Bool, Fortran Anno)
paralleliseLoop_map filename loop loopVarNames nonTempVars prexistingVars dependencies accessAnalysis subTable
                                    |    errors_map' == nullAnno     =    (True, appendAnnotation mapCode (compilerName ++ ": Map at " ++ errorLocationFormatting (srcSpan loop)) "")
                                    |    otherwise                    =    (False, appendAnnotationMap loop errors_map')
                                    where
                                        loopWrites = extractWrites_query loop
-- WV: def:                                            analyseLoop_map  comment loopVars loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable codeSeg
                                        loopAnalysis = analyseLoop_map "Cannot map: " [] loopWrites nonTempVars prexistingVars accessAnalysis dependencies subTable loop

                                        errors_map = getErrorAnnotations loopAnalysis
                                        reads_map = getReads loopAnalysis
                                        writes_map = getWrites loopAnalysis

                                        (loopCarriedDeps_bool, evaluated_bool, loopCarriedDeps) = loopCarriedDependencyCheck loop

                                        errors_map' = case loopCarriedDeps_bool of
                                                            True -> case evaluated_bool of
                                                                    True -> DMap.insert (outputTab ++ "Cannot map: Loop carried dependency detected:\n")
                                                                        (formatLoopCarriedDependencies loopCarriedDeps) errors_map
                                                                    False -> DMap.insert (outputTab ++ "Cannot map: Loop carried dependency possible (not evaluated):\n")
                                                                        (formatLoopCarriedDependencies loopCarriedDeps) errors_map
                                                            False -> errors_map

                                        loopVariables = loopCondtions_query loop
                                        startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
                                        endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
                                        stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

                                        varNames_loopConditions = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames
                                        containedLoopIteratorVarNames = (map (\(a, _, _, _) -> a) (loopCondtions_query loop))

                                        reads_map_varnames = foldl (++) [] (map extractVarNames reads_map)
                                        readArgs = listRemoveDuplications $ listSubtract reads_map_varnames containedLoopIteratorVarNames    -- List of arguments to kernel that are READ
                                        -- readArgs = (listRemoveDuplications $ listSubtract reads_map_varnames (containedLoopIteratorVarNames ++ varNames_loopConditions)    )    -- List of arguments to kernel that are READ

                                        writes_map_varnames = foldl (++) [] (map extractVarNames writes_map)
                                        writtenArgs = listRemoveDuplications $ listSubtract writes_map_varnames containedLoopIteratorVarNames     -- List of arguments to kernel that are WRITTEN
                                        -- WV20170426
                                        iterLoopVariables=[]

                                        mapCode = OpenCLMap nullAnno (generateSrcSpan filename (srcSpan loop))     -- Node to represent the data needed for an OpenCL map kernel -- WV20170426
                                            readArgs        -- List of arguments to kernel that are READ
                                            writtenArgs     -- List of arguments to kernel that are WRITTEN
                                            loopVariables    -- Loop variables of nested maps
                                            iterLoopVariables -- WV20170426
                                            (removeLoopConstructs_recursive loop) -- Body of kernel code

--    Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--    original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce :: String -> Fortran Anno -> [VarName Anno] -> [VarName Anno]-> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_reduce filename loop loopVarNames nonTempVars prexistingVars dependencies accessAnalysis
                                    |     errors_reduce' == nullAnno     =    (True, appendAnnotation reductionCode' (compilerName ++ ": Reduction at " ++ errorLocationFormatting (srcSpan loop)) "")
                                    |    otherwise                    =    (False, appendAnnotationMap loop errors_reduce')
                                    where
                                        loopWrites = extractWrites_query loop
                                        loopAnalysis = analyseLoop_reduce "Cannot reduce: " [] [] loopWrites nonTempVars prexistingVars dependencies accessAnalysis loop
                                        errors_reduce = getErrorAnnotations loopAnalysis
                                        reductionVarNames = getReductionVarNames loopAnalysis
                                        reads_reduce = getReads loopAnalysis
                                        writes_reduce = getWrites loopAnalysis

                                        (loopCarriedDeps_bool, evaluated_bool, loopCarriedDeps) = loopCarriedDependencyCheck loop

                                        errors_reduce' = case loopCarriedDeps_bool of
                                                            True -> case evaluated_bool of
                                                                    True -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency detected:\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
                                                                    False -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency possible (not evaluated):\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
                                                            False -> errors_reduce


                                        loopVariables = loopCondtions_query loop
                                        startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
                                        endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
                                        stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

                                        varNames_loopConditions = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames
                                        containedLoopIteratorVarNames = (map (\(a, _, _, _) -> a) (loopCondtions_query loop))

                                        reads_map_varnames = foldl (++) [] (map extractVarNames reads_reduce)
                                        readArgs = (listRemoveDuplications $ listSubtract reads_map_varnames (varNames_loopConditions)    )    -- List of arguments to kernel that are READ
                                        -- readArgs = (listRemoveDuplications $ listSubtract reads_map_varnames (containedLoopIteratorVarNames ++ varNames_loopConditions)    )    -- List of arguments to kernel that are READ

                                        writes_map_varnames = foldl (++) [] (map extractVarNames writes_reduce)
                                        writtenArgs = (listRemoveDuplications $ listSubtract writes_map_varnames containedLoopIteratorVarNames)     -- List of arguments to kernel that are WRITTEN

                                        allReductionVarNames = (foldl (\accum item -> accum ++ extractVarNames item) [] reductionVarNames)
                                        reductioNVarInfo = (listRemoveDuplications (foldl (\accum item -> accum ++ [(item, getValueAtSrcSpan item (srcSpan loop) accessAnalysis)] ) [] allReductionVarNames))
                                        -- WV20170426
                                        iterLoopVariables=[]
                                        reductionCode' =  reductionCode -- (miniPPF reductionCode)
                                        reductionCode = OpenCLReduce nullAnno (generateSrcSpan filename (srcSpan loop))   -- WV20170426
                                            readArgs            -- List of arguments to kernel that are READ
                                            writtenArgs         -- List of arguments to kernel that are WRITTEN
                                            loopVariables        -- Loop variables of nested maps
                                            iterLoopVariables -- WV20170426
                                            reductioNVarInfo     -- List of variables that are considered 'reduction variables' along with their initial values
                                            (removeLoopConstructs_recursive loop) -- Body of kernel code

--    A reduction with outer iteration occurs when a parallel reduction occurs in some nested loops but requires values from some outer, iterative loop. More advanced loop carried dependency
--    analysis caused this to be necessary.
--    WV: I need to add in the loop vars for the iterative loop.
--    So given a For-loop, I need to find all For-loops inside it and extract the vars. This is not hard: simply do an everything on the loop and match the For ,then get the vars.

-- WV20170426 I added a field for the loop vars for enclosing iterative loops, this function updates that field
-- It relies on the code being traversed bottom-up so that the last update will include all loop vars.
updateIterLoopVars :: [VarName Anno] -> Fortran Anno -> Fortran Anno
updateIterLoopVars loopVarNames (OpenCLMap f1 f2 f3 f4 f5 ilvars stmt) = OpenCLMap f1 f2 f3 f4 f5 (ilvars++loopVarNames) stmt
updateIterLoopVars loopVarNames (OpenCLReduce f1 f2 f3 f4 f5 ilvars f6 stmt) = OpenCLReduce f1 f2 f3 f4 f5 (ilvars++loopVarNames) f6 stmt
updateIterLoopVars loopVarNames n = n

-- WV20170426 The addition of these loop variables is a bit ad-hoc because
paralleliseLoop_reduceWithOuterIteration :: String -> Fortran Anno -> Maybe (Fortran Anno) -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> [VarName Anno] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran Anno)
paralleliseLoop_reduceWithOuterIteration filename loop Nothing loopVarNames loopVarNames' nonTempVars prexistingVars dependencies accessAnalysis
    |    nextFor_maybe == Nothing = (False, loop')
    |    otherwise = (reduceWithOuterIterationAttempt_bool, newAst')
        where
            loop'' = everywhere (mkT (updateIterLoopVars loopVarNames)) loop
            loop' = loop'' -- warning loop'' ("! ITERATIVE LOOP (1a): \n"++(show loopVarNames')++"\n"++(miniPPF loop'')++"\n! END ITERATIVE LOOP (1a)\n" )
            newAst' = newAst -- warning newAst ("! ITERATIVE LOOP (1b): \n"++(show loopVarNames')++"\n"++(miniPPF newAst)++"\n! END ITERATIVE LOOP (1b)\n" )
            nextFor_maybe = extractFirstChildFor loop
            (priorFortran, nextFor, followingFortran) = case nextFor_maybe of
                            Nothing -> error "paralleliseLoop_reduceWithOuterIteration: nextFor_maybe is Nothing"25
                            Just a -> a
            newLoopVarNames = case getLoopVar nextFor of
                Just a  -> loopVarNames' ++ [a]
                Nothing -> loopVarNames'
            (reduceWithOuterIterationAttempt_bool, reduceWithOuterIterationAttempt_ast) = paralleliseLoop_reduceWithOuterIteration filename loop (Just nextFor) loopVarNames newLoopVarNames nonTempVars prexistingVars dependencies accessAnalysis
            newAst = case reduceWithOuterIterationAttempt_ast of
                        For a1 a2 a3 a4 a5 a6 fortran -> For a1 a2 a3 a4 a5 a6 (appendFortran_recursive (appendFortran_recursive followingFortran fortran) priorFortran)

paralleliseLoop_reduceWithOuterIteration filename iteratingLoop (Just parallelLoop) loopVarNames loopVarNames' nonTempVars prexistingVars dependencies accessAnalysis
                | errors_reduce' == nullAnno     =
                    (True, appendAnnotation reduceWithOuterIterationCode (compilerName ++ ": Reduction with outer iteration at " ++ errorLocationFormatting (srcSpan iteratingLoop) ++ " with parallel loop at "  ++ errorLocationFormatting (srcSpan parallelLoop')) "")
                | nextFor_maybe /= Nothing     =
                    paralleliseLoop_reduceWithOuterIteration filename (appendAnnotationMap iteratingLoop errors_reduce') (Just nextFor) loopVarNames newLoopVarNames nonTempVars prexistingVars dependencies accessAnalysis
                | otherwise                    =    (False, appendAnnotationMap iteratingLoop errors_reduce')

        where
            parallelLoop' = parallelLoop -- warning parallelLoop ("! ITERATIVE LOOP (1a): \n"++(show loopVarNames')++"\n"++(miniPPF parallelLoop)++"\n! END ITERATIVE LOOP (1a)\n" )
            loopWrites = extractWrites_query parallelLoop'
            loopAnalysis = analyseLoop_reduce reduceWithOuterIterationComment [] [] loopWrites nonTempVars prexistingVars dependencies accessAnalysis parallelLoop'
            errors_reduce = getErrorAnnotations loopAnalysis
            reductionVarNames = getReductionVarNames loopAnalysis
            reads_reduce = getReads loopAnalysis
            writes_reduce = getWrites loopAnalysis
            -- WV: CHECK THIS!
            iteratingLoop' =   everywhere (mkT (updateIterLoopVars loopVarNames)) iteratingLoop -- warning iteratingLoop ("! ITERATIVE LOOP (2): \n"++(miniPPF iteratingLoop)++"\n! END ITERATIVE LOOP (2)\n" )
            iteratingLoopVars = listSubtract (extractLoopVars iteratingLoop') (extractLoopVars parallelLoop')

            (loopCarriedDeps_bool, evaluated_bool, loopCarriedDeps) = loopCarriedDependencyCheck_reductionWithIteration iteratingLoop parallelLoop'
            errors_reduce' = case loopCarriedDeps_bool of
                                True -> case evaluated_bool of
                                        True -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency detected:\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
                                        False -> DMap.insert (outputTab ++ "Cannot reduce: Loop carried dependency possible (not evaluated):\n") (formatLoopCarriedDependencies loopCarriedDeps) errors_reduce
                                False -> errors_reduce

            loopVariables = loopCondtions_query parallelLoop'

            startVarNames = foldl (\accum (_,x,_,_) -> accum ++ extractVarNames x) [] loopVariables
            endVarNames = foldl (\accum (_,_,x,_) -> accum ++ extractVarNames x) [] loopVariables
            stepVarNames = foldl (\accum (_,_,_,x) -> accum ++ extractVarNames x) [] loopVariables

            nextFor_maybe = extractFirstChildFor parallelLoop'
            (priorFortran, nextFor, followingFortran) = case nextFor_maybe of
                            Nothing -> error "paralleliseLoop_reduceWithOuterIteration: nextFor is Nothing"
                            Just a -> a
            newLoopVarNames = case getLoopVar nextFor of
                Just a  -> loopVarNames' ++ [a]
                Nothing -> loopVarNames'
            newAst = appendFortran_recursive followingFortran (appendFortran_recursive reductionCode priorFortran)

            reduceWithOuterIterationCode = case iteratingLoop of
                                        For a1 a2 a3 a4 a5 a6 fortran -> For a1 a2 a3 a4 a5 a6 newAst -- reductionCode
                                        _ -> error "paralleliseLoop_reduceWithOuterIteration: iterating loop is not FOR"

            varNames_loopConditions = listSubtract (listRemoveDuplications (startVarNames ++ endVarNames ++ stepVarNames)) loopVarNames
            containedLoopIteratorVarNames = (map (\(a, _, _, _) -> a) (loopCondtions_query parallelLoop'))

            reads_map_varnames = foldl (++) [] (map extractVarNames reads_reduce)
            readArgs = (listRemoveDuplications $ listSubtract reads_map_varnames (containedLoopIteratorVarNames ++ iteratingLoopVars)    )    -- List of arguments to kernel that are READ
            -- readArgs = (listRemoveDuplications $ listSubtract reads_map_varnames (containedLoopIteratorVarNames ++ varNames_loopConditions ++ iteratingLoopVars)    )    -- List of arguments to kernel that are READ

            writes_map_varnames = foldl (++) [] (map extractVarNames writes_reduce)
            writtenArgs = (listRemoveDuplications $ listSubtract writes_map_varnames containedLoopIteratorVarNames)     -- List of arguments to kernel that are WRITTEN

            allReductionVarNames = (foldl (\accum item -> accum ++ extractVarNames item) [] reductionVarNames)
            reductioNVarInfo = (listRemoveDuplications (foldl (\accum item -> accum ++ [(item, getValueAtSrcSpan item (srcSpan parallelLoop') accessAnalysis)] ) [] allReductionVarNames))

            iterLoopVariables = extractLoopVars iteratingLoop -- WV20170426
            reductionCode' = reductionCode -- warning reductionCode (miniPPF reductionCode)
            reductionCode = OpenCLReduce nullAnno (generateSrcSpan filename (srcSpan parallelLoop'))  -- WV20170426
                         readArgs            -- List of arguments to kernel that are READ
                        writtenArgs         -- List of arguments to kernel that are WRITTEN
                        loopVariables        -- Loop variables of nested maps
                        iterLoopVariables -- WV20170426
                        reductioNVarInfo     -- List of variables that are considered 'reduction variables' along with their initial values
                        (removeLoopConstructs_recursive parallelLoop') -- Body of kernel code
            reduceWithOuterIterationComment = "Cannot iterative reduce (iter:" ++ (errorLocationFormatting $ srcSpan iteratingLoop) ++ ", par:" ++ (errorLocationFormatting $ srcSpan parallelLoop') ++ "): "

--    Function uses a SYB query to get all of the loop condtions contained within a particular AST. loopCondtions_query traverses the AST
--    and calls getLoopConditions when a Fortran node is encountered.
loopCondtions_query :: (Typeable p, Data p) =>  Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
loopCondtions_query = everything (++) (mkQ [] getLoopConditions)

getLoopConditions :: (Typeable p, Data p) => Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
getLoopConditions codeSeg = case codeSeg of
        For _ _ var start end step _ -> [(var, start, end, step)]
        OpenCLMap _ _ _ _ loopVars iterLoopVars _ -> loopVars -- WV20170426
        OpenCLReduce _ _ _ _ loopVars iterLoopVars _ _ -> loopVars -- WV20170426
        _ -> []

--    Traverses the AST and produces a single string that contains all of the parallelising errors for this particular run of the compiler.
--    compileAnnotationListing traverses the AST and applies getAnnotations to AST nodes. The resulting string is then output to the user.
compileAnnotationListing codeSeg = everything (++) (mkQ [] getAnnotations) codeSeg

getAnnotations :: Fortran Anno -> String
getAnnotations codeSeg = case (tag codeSeg) == nullAnno of
    True -> ""
    False -> leadMessageCheck
        where
            leadMessageCheck = case DMap.findWithDefault [] (head keys) errorMap == [""] of
                True  -> errorListing
                False -> leadMessage ++ errorListing ++ "\n"
            leadMessage = compilerName ++ ": Cannot parallelise loop at " ++ errorLocationFormatting (srcSpan codeSeg) ++ "\n"
            errorListing = foldl (\errorDescription key -> errorDescription ++ key ++
                                                (foldl (\errorInstance item -> errorInstance ++ "\t" ++ item) "" (applyAnnotationFormatting 2 (listRemoveDuplications (DMap.findWithDefault [] key errorMap)))) ++ "\n"
                    ) "" keys
            keys = DMap.keys errorMap
            errorMap = tag codeSeg

applyAnnotationFormatting :: Int -> [String] -> [String]
applyAnnotationFormatting itemsPerLine items = formattedList
            where
                indexorList = [1..(length items)]
                indexoredItems = zip indexorList items
                formattedList = map (\(index, item) -> if (mod index itemsPerLine) == 0 && index /= (length items)
                                                            then item ++ "\n" else item) indexoredItems

formatLoopCarriedDependencies :: [(Expr Anno, Expr Anno)] -> [String]
formatLoopCarriedDependencies ((readExpr, writtenExpr):exprs) = [outputTab ++ (outputExprFormatting writtenExpr) ++ " -> " ++ (outputExprFormatting readExpr)] ++ formatLoopCarriedDependencies exprs
formatLoopCarriedDependencies [] = []

--    Returns a list of all of the names of variables that are used in a particular AST. getVarNames_query performs the traversal and applies
--    getVarNames at appropriate moments.
getVarNames_query :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_query fortran = everything (++) (mkQ [] getVarNames) fortran

getVarNames :: (Typeable p, Data p) =>  VarName p -> [VarName p]
getVarNames expr = [expr]

--    Function checks whether every Expr in a list is a VarName from another list.
exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]
