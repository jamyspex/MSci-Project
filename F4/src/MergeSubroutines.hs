{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MergeSubroutines

where


import           Data.Function
import           Data.Generics        (Data, Typeable, everything, everywhere,
                                       gmapQ, gmapT, mkQ, mkT)
import           Data.List
import           Data.List.Unique
import qualified Data.Map             as DMap
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils

mergeSubsToBeParallelised :: SubroutineTable -> IO (SubroutineTable)
mergeSubsToBeParallelised srt = do
    mergeSubs forOffloadPairs
    return (srt)
    where
        paraReplacementPairs = getArgTransSubroutinePairs srt
        -- (mergedArgTrans, mergedSub) = mergeSubs forOffloadPairs
        -- updateSubRecInSrt subRec srt = DMap.insert (subName subRec) subRec srt
        -- srtWithMergedAdded = DMap.insert (subName mergedSub) mergedSub srt
        forOffloadPairs = filter (\(_, sub) -> parallelise sub) $ paraReplacementPairs

-- argTransToSubRecs is ordered by the call sequence in question this means the
-- bodies will be merged in the order they are in the argTransToSubRecs list
mergeSubs :: [([ArgumentTranslation], SubRec)] -> IO () --([ArgumentTranslation], SubRec)
mergeSubs argTransToSubRecs = do
    putStrLn ("unique args = " ++ show (map getArgName uniqueArgs))
    putStrLn ("unique decls = \n" ++ (concatMap (\decl -> miniPPD decl ++ "\n") uniqueDecls))
    return ()
    where
        conflictFreeParaReplacementPairs = resolveConflictsWithLocalDecls argTransToSubRecs
        subsWithParamsReplaced = map (\(argTrans, calledSub) -> replaceParametersWithArgumentNames argTrans calledSub) conflictFreeParaReplacementPairs
        uniqueDecls = getUniqueDecls $ getAllDecls subsWithParamsReplaced
        uniqueArgs = getUniqueArgs $ getAllArgs subsWithParamsReplaced

fst3 (a, _, _) = a

getArgTransSubroutinePairs :: SubroutineTable -> [([ArgumentTranslation], SubRec)]
getArgTransSubroutinePairs srt = map (\(_, argTrans, subrec) -> (argTrans, subrec)) sorted
    where
        sorted = sortBy (getCallOrdering `on` fst3) allPairs
        allPairs = concatMap (\caller -> concatMap (getPair caller) $ getCalled caller) callers
        getCalled callerSubRec = DMap.keys $ argTranslations callerSubRec
        getPair :: SubRec -> String -> [(Fortran Anno, [ArgumentTranslation], SubRec)]
        getPair caller calleeName =
            case (DMap.lookup calleeName (argTranslations caller)) of
                Just argTrans -> [(fst argTrans, snd argTrans, srt DMap.! calleeName)]
                _             -> []
        callers = getSubRoutinesThatMakeCalls srt

getCallOrdering (Call _ (start1, _) _ _) (Call _ (start2, _) _ _) =
        if lineCompareResult == EQ then
            srcColumn start1 `compare` srcColumn start2
        else
            lineCompareResult
        where
            lineCompareResult = srcLine start1 `compare` srcLine start2

-- -- get subrecs with entires in their argumentTranslation table
getSubRoutinesThatMakeCalls :: SubroutineTable -> [SubRec]
getSubRoutinesThatMakeCalls srt = filter (\subrec -> numberOfCallsMade subrec > 0) $ DMap.elems srt
    where
        numberOfCallsMade = length . DMap.keys . argTranslations

getSubCalls :: SubRec -> [String]
getSubCalls subrec = DMap.keys (argTranslations subrec)

-- getB

-- getArgTransForCallToSub :: SubRec -> String -> [ArgumentTranslation]
-- getArgTransForCallToSub subrec name =
--     let
--         (_, argTrans) = (name DMap.! (argumentTranslations subrec))
--     in
--         argTrans

resolveConflictsWithLocalDecls :: [([ArgumentTranslation], SubRec)] -> [([ArgumentTranslation], SubRec)]
resolveConflictsWithLocalDecls pairs =
    if hasBeenUpdated then
        resolveConflictsWithLocalDecls result
    else
        result
    where
        allLocalDecls = concatMap (getNonParameterDeclarations . snd) pairs
        allArgTrans = concatMap fst pairs
        conflicts = findConflicts allLocalDecls allArgTrans
        foldResults (updated, curResult) (updatedAcc, resultAcc) = (updated || updatedAcc, (curResult:resultAcc))
        (hasBeenUpdated, result) = foldr foldResults (False, []) withConflictsResolved
        withConflictsResolved = map (updateArgTransToRespectLocalVariables conflicts) pairs
        -- resolveConflicts (argTrans, subrec) = (map (updateArgTran conflicts) argTrans, subrec)

findConflicts :: [String] -> [ArgumentTranslation] -> [String]
findConflicts localDecls argTrans = filter (\decl -> decl `elem` argNames) localDecls
    where
        argumentVarNames = map (\(argTran) -> argument argTran) argTrans
        argNames = map (\(VarName _ name) -> name) argumentVarNames

updateArgTransToRespectLocalVariables :: [String] -> ([ArgumentTranslation], SubRec) -> (Bool, ([ArgumentTranslation], SubRec))
updateArgTransToRespectLocalVariables conflicts (argTrans, subrec) = (runAgain, (conflictFreeArgTrans, subrec))
    where
        (runAgain, conflictFreeArgTrans) = foldr foldResults (False, []) updatedArgTrans
        updatedArgTrans =  map (updateArgTran conflicts) argTrans
        foldResults (updated, argTran) (updatedAcc, argTransAcc) = (updated || updatedAcc, argTran:argTransAcc)

updateArgTran :: [String] -> ArgumentTranslation -> (Bool, ArgumentTranslation)
updateArgTran localDecls a@(ArgTrans param arg@(VarName anno name)) =
    if (name `elem` localDecls) then
        (True, ArgTrans param (VarName anno (name ++ "_F4_cas")))
    else
        (False, a)


getNonParameterDeclarations :: SubRec -> [String]
getNonParameterDeclarations subrec = (decls \\ arguments)
    where
        arguments = getArgsAsString $ subAst subrec
        decls = getDeclNames $ subAst subrec

getUniqueArgs :: [ArgName Anno] -> [ArgName Anno]
getUniqueArgs args = removeDuplicates getArgName args

getAllArgs :: [SubRec] -> [ArgName Anno]
getAllArgs subrecs = concatMap (\subrec -> getArgs $ subAst subrec) subrecs

getUniqueDecls :: [Decl Anno] -> [Decl Anno]
getUniqueDecls decls = removeDuplicates (getNameFromVarName . getVarName) decls

-- DMap.elems uniqueMap
--     where
--         pairsForMap = map (\decl -> ( decl, decl)) decls
--         uniqueMap = foldr addToMap DMap.empty pairsForMap
--         addToMap :: (String, Decl Anno) -> DMap.Map String (Decl Anno) -> DMap.Map String (Decl Anno)
--         addToMap (key, val) map = DMap.insert key val map

getAllDecls :: [SubRec] -> [Decl Anno]
getAllDecls subrecs = concatMap (\subrec -> getDeclForMerge $ subAst subrec) subrecs

getDeclForMerge :: ProgUnit Anno -> [Decl Anno]
getDeclForMerge (Sub _ _ _ _ _ (Block _ _ _ _ decls _))  = everything (++) (mkQ [] getDeclsQuery) decls
    where
        getDeclsQuery :: Decl Anno -> [Decl Anno]
        getDeclsQuery decl = case decl of
                                (Decl _ _ _ _) -> [decl]
                                _              -> []

getDeclNames :: ProgUnit Anno -> [String]
getDeclNames  (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) = map (getNameFromVarName . getVarName) declStatements
    where
        declStatements = everything (++) (mkQ [] declNameQuery) $ decls

declNameQuery :: Decl Anno -> [Expr Anno]
declNameQuery decl = case decl of
                                (Decl _ _ ((expr, _, _):_) _) -> [expr] --everything (++) (mkQ [] extractVarNamesFromExpr) expr
                                _                             -> []

getVarName decl = head $ everything (++) (mkQ [] extractVarNamesFromExpr) decl
getNameFromVarName (VarName _ name) = name

getArgs :: ProgUnit Anno -> [ArgName Anno]
getArgs (Sub _ _ _ _ args _) = everything (++) (mkQ [] argNameQuery) args
    where
        argNameQuery :: ArgName Anno -> [ArgName Anno]
        argNameQuery input = case input of
                        argname@(ArgName _ name) -> [argname]
                        _                        -> []
getArgs _ = error "Passed something other than a Sub to getArgs"

getArgsAsString :: ProgUnit Anno -> [String]
getArgsAsString sub = map getArgName $ getArgs sub
gerArgsAsString = error "Passed something other than a Sub to getArgsAsString"


replaceParametersWithArgumentNames :: [ArgumentTranslation] -> SubRec -> SubRec
replaceParametersWithArgumentNames argTransForCall callee = updatedAst
   where
        updatedAst = foldr preformUpdates callee argTransForCall
        preformUpdates argTran = ((renameParameter argTran) . (replaceOneParamUsageWithArg argTran))

-- renameParameter :: ArgumentTranslation -> SubRec -> SubRec
-- renameParameter argTrans subRec = subRec { subAst = everywhere (mkT renameQuery) $ subAst subRec }
--     where
--         renameQuery curVal@(ArgName _ name)
--             | name == (getArgName $ parameter argTrans) = ArgName nullAnno $ getVarName (argument argTrans)
--             | otherwise = curVal
--         renameQuery curVal = curVal

-- replaceOneParamUsageWithArg :: ArgumentTranslation -> SubRec -> SubRec
-- replaceOneParamUsageWithArg argTrans subRec = subRec { subAst = everywhere (mkT replaceQuery) $ subAst subRec }
--     where
--         replaceQuery curVal@(VarName _ name)
--             | name == (getArgName $ parameter argTrans) = argument argTrans
--             | otherwise = curVal

renameParameter :: ArgumentTranslation -> SubRec -> SubRec
renameParameter argTrans subRec = subRec { subAst = everywhere (mkT renameQuery) $ subAst subRec }
    where
        renameQuery curVal@(ArgName anno name)
            | ((name == (getArgName $ parameter argTrans)) && (not . hasBeenUpdated) anno)
                = ArgName updatedAnno $ getNameFromVarName (argument argTrans)
            | otherwise = curVal
        renameQuery curVal = curVal

replaceOneParamUsageWithArg :: ArgumentTranslation -> SubRec -> SubRec
replaceOneParamUsageWithArg argTrans subRec = subRec { subAst = everywhere (mkT replaceQuery) $ subAst subRec }
    where
        replaceQuery curVal@(VarName anno name)
            | ((name == (getArgName $ parameter argTrans)) && (not . hasBeenUpdated) anno)
                 = VarName updatedAnno $ getNameFromVarName (argument argTrans)
            | otherwise = curVal

getArgName (ArgName _ name) = name

updatedAnno :: Anno
updatedAnno = DMap.singleton (mergeSubsAnnoKey) []

mergeSubsAnnoKey = "msak"
hasBeenUpdated anno = DMap.member mergeSubsAnnoKey anno


