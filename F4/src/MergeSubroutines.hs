{-# LANGUAGE ScopedTypeVariables #-}

module MergeSubroutines

where


import           Data.Generics        (Data, Typeable, everything, everywhere,
                                       gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.Map             as DMap
import           Language.Fortran
import           LanguageFortranTools
import           Parser

mergeSubs :: SubroutineTable -> SubroutineTable
mergeSubs srt = updatedSrt
    -- do
    -- mapM_ (\(key, val) -> putStrLn (key ++ " = " ++ (show $ getNonParameterDeclarations val))) subsToOffload
    -- putStrLn ("offloadlength = " ++ (show $ length subsToOffload))
    -- return (updatedSrt)
    where
        paraReplacementPairs = getArgTransSubroutinePairs srt
        conflictFreeParaReplacementPairs = resolveConflictsWithLocalDecls paraReplacementPairs
        subsWithParamsReplaced = map (\(argTrans, calledSub) -> replaceParametersWithArgumentNames argTrans calledSub) conflictFreeParaReplacementPairs
        updateSubRecInSrt subRec srt = DMap.insert (subName subRec) subRec srt
        updatedSrt = foldr updateSubRecInSrt srt subsWithParamsReplaced
        -- map (\(argTrans, calledSub) -> replaceParametersWithArgumentNames argTrans calledSub)
        subsToOffload = filter (\(_, sub) -> parallelise sub) $ DMap.toList srt


getArgTransSubroutinePairs :: SubroutineTable -> [([ArgumentTranslation], SubRec)]
getArgTransSubroutinePairs srt = concatMap (\caller -> concatMap (getPair caller) $ getCalled caller) callers
        where
            getCalled callerSubRec = DMap.keys $ argTranslations callerSubRec
            getPair :: SubRec -> String -> [([ArgumentTranslation], SubRec)]
            getPair caller calleeName =
                case (DMap.lookup calleeName (argTranslations caller)) of
                    Just argTrans -> [(snd argTrans, srt DMap.! calleeName)]
                    _             -> []
            callers = getSubRoutinesThatMakeCalls srt

-- -- get subrecs with entires in their argumentTranslation table
getSubRoutinesThatMakeCalls :: SubroutineTable -> [SubRec]
getSubRoutinesThatMakeCalls srt = filter (\subrec -> numberOfCallsMade subrec > 0) $ DMap.elems srt
    where
        numberOfCallsMade = length . DMap.keys . argTranslations

getSubCalls :: SubRec -> [String]
getSubCalls subrec = DMap.keys (argTranslations subrec)

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
        arguments = getArguments $ subAst subrec
        decls = getDecls $ subAst subrec


getDecls :: ProgUnit Anno -> [String]
getDecls  (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) = getNames
    where
        getDecls = everything (++) (mkQ [] declQuery) $ decls
        declQuery :: Decl Anno -> [Expr Anno]
        declQuery decl = case decl of
                                (Decl _ _ ((expr, _, _):_) _) -> [expr] --everything (++) (mkQ [] extractVarNamesFromExpr) expr
                                _                             -> []
        getVarNames = concatMap (everything (++) (mkQ [] extractVarNamesFromExpr)) getDecls
        getNames = map (\(VarName _ name) -> name) getVarNames


getArguments :: ProgUnit Anno -> [String]
getArguments (Sub _ _ _ _ arg _) =  everything (++) (mkQ [] argNameQuery) $ arg
    where
        argNameQuery :: ArgName Anno -> [String]
        argNameQuery input = case input of
                                (ArgName _ name) -> [name]
                                _                -> []
getArguments _ = []

replaceParametersWithArgumentNames :: [ArgumentTranslation] -> SubRec -> SubRec
replaceParametersWithArgumentNames argTransForCall callee = updatedAst
   where
        updatedAst = foldr preformUpdates callee argTransForCall
        preformUpdates argTran = ((renameParameter argTran) . (replaceOneParamUsageWithArg argTran))

renameParameter :: ArgumentTranslation -> SubRec -> SubRec
renameParameter argTrans subRec = subRec { subAst = everywhere (mkT renameQuery) $ subAst subRec }
    where
        renameQuery curVal@(ArgName _ name)
            | name == (getArgName $ parameter argTrans) = ArgName nullAnno $ getVarName (argument argTrans)
            | otherwise = curVal
        renameQuery curVal = curVal

replaceOneParamUsageWithArg :: ArgumentTranslation -> SubRec -> SubRec
replaceOneParamUsageWithArg argTrans subRec = subRec { subAst = everywhere (mkT replaceQuery) $ subAst subRec }
    where
        replaceQuery curVal@(VarName _ name)
            | name == (getArgName $ parameter argTrans) = argument argTrans
            | otherwise = curVal

getArgName (ArgName _ name) = name
getVarName (VarName _ name) = name



