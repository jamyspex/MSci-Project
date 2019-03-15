{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MergeSubroutines where


import           ConstantFolding                ( foldConstants )
import           Data.Function
import           Data.Generics                  ( Data
                                                , Typeable
                                                , everything
                                                , everywhere
                                                , gmapQ
                                                , gmapT
                                                , mkQ
                                                , mkT
                                                )
import           Data.List
import qualified Data.Map                      as DMap
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils

mergeSubsToBeParallelised :: SubroutineTable -> SubroutineTable
mergeSubsToBeParallelised srt = DMap.insert (subName mergedSubRec)
                                            mergedSubRec
                                            originalsRemoved
 where
  paraReplacementPairs = getArgTransSubroutinePairs srt
  keysToRemove = map (\(_, sub) -> (subName sub)) forOffloadPairs
  (mergedArgTrans, mergedSubRec) = mergeSubs forOffloadPairs
  originalsRemoved = foldr (\cur acc -> DMap.delete cur acc) srt keysToRemove
  forOffloadPairs =
    filter (\(_, sub) -> parallelise sub) $ paraReplacementPairs

-- argTransToSubRecs is ordered by the call sequence in question this means the
-- bodies will be merged in the order they are in the argTransToSubRecs list
mergeSubs
  :: [([ArgumentTranslation], SubRec)] -> ([ArgumentTranslation], SubRec)
mergeSubs argTransToSubRecs = (uniqueArgTrans, mergedSubRec)
 where
  conflictFreeParaReplacementPairs =
    resolveConflictsWithLocalDecls argTransToSubRecs
  subsWithParamsReplaced = map
    (\(argTrans, calledSub) ->
      replaceParametersWithArgumentNames argTrans calledSub
    )
    conflictFreeParaReplacementPairs
  mergedName  = getMergedName subsWithParamsReplaced
  uniqueDecls = sortBy getDeclOrdering $ getUniqueDecls $ getAllDecls
    subsWithParamsReplaced
  uniqueArgs = getUniqueArgs $ getAllArgs subsWithParamsReplaced
  uniqueArgTrans =
    getUniqueArgTrans $ concatMap (\(argTrans, _) -> argTrans) argTransToSubRecs
  combinedBody     = combineBodies bodies
  bodies           = map getSubBodyWithOriginalNameNode subsWithParamsReplaced
  combinedDeclNode = (combineDecls . seperateDecls) uniqueDecls
  combinedArgs     = combineArgs uniqueArgs
  block            = Block nullAnno
                           nullUseBlock
                           (ImplicitNull nullAnno)
                           nullSrcSpan
                           combinedDeclNode
                           combinedBody
  sub = Sub nullAnno
            nullSrcSpan
            Nothing
            (SubName nullAnno mergedName)
            combinedArgs
            block
  subWithConstantsFolded = foldConstants sub
  returnsRemoved         = everywhere (mkT removeReturns) subWithConstantsFolded
  mergedSubRec           = MkSubRec
    { subAst          = returnsRemoved
    , subSrcFile      = ""
    , subSrcLines     = []
    , subName         = mergedName
    , argTranslations = DMap.empty
    , parallelise     = True
    }


-- Seperate decl statements, so that they all appear on their own line
-- otherwise causes issues when seperating into kernels
seperateDecls :: [Decl Anno] -> [Decl Anno]
seperateDecls inputDecls = concatMap seperateOne inputDecls
 where
  seperateOne :: Decl Anno -> [Decl Anno]
  seperateOne (Decl anno srcSpan declList declType) =
    map (\item -> Decl anno srcSpan [item] declType) declList

-- Gets the subroutine body from the subRec and enclose it in a meta
-- node which simply holds its the subroutines original name so this
-- can be used to build up a more logical kernel name in the output
getSubBodyWithOriginalNameNode :: SubRec -> Fortran Anno
getSubBodyWithOriginalNameNode subrec = OriginalSubContainer nullAnno name body
 where
  body = getSubroutineBody subrec
  name = subName subrec

removeReturns :: Fortran Anno -> Fortran Anno
removeReturns (Return anno srcSpan _) = NullStmt anno srcSpan
removeReturns val                     = val

fst3 (a, _, _) = a

getArgTransSubroutinePairs
  :: SubroutineTable -> [([ArgumentTranslation], SubRec)]
getArgTransSubroutinePairs srt = map
  (\(_, argTrans, subrec) -> (argTrans, subrec))
  sorted
 where
  sorted = sortBy (getCallOrdering `on` fst3) allPairs
  allPairs =
    concatMap (\caller -> concatMap (getPair caller) $ getCalled caller) callers
  getCalled callerSubRec = DMap.keys $ argTranslations callerSubRec
  getPair :: SubRec -> String -> [(Fortran Anno, [ArgumentTranslation], SubRec)]
  getPair caller calleeName =
    case (DMap.lookup calleeName (argTranslations caller)) of
      Just argTrans -> [(fst argTrans, snd argTrans, srt DMap.! calleeName)]
      _             -> []
  callers = getSubRoutinesThatMakeCalls srt

getDeclOrdering (Decl _ _ _ typeDecl1) (Decl _ _ _ typeDecl2) = case results of
  (True , False) -> LT
  (False, True ) -> GT
  (_    , _    ) -> EQ
 where
  results      = (hasParamAttr typeDecl1, hasParamAttr typeDecl2)
  hasParamAttr = (not . null . (concatMap (getParameterAttrs)) . getAttrs)
  getParameterAttrs attr = case attr of
    p@(Parameter _) -> [p]
    _               -> []
getDeclOrdering _ _ =
  error "Can't get ordering for declarations other than Decl"

getCallOrdering (Call _ (start1, _) _ _) (Call _ (start2, _) _ _) =
  if lineCompareResult == EQ
    then srcColumn start1 `compare` srcColumn start2
    else lineCompareResult
  where lineCompareResult = srcLine start1 `compare` srcLine start2
getCallOrdering _ _ =
  error "Can't get ordering for statements other than calls"

combineBodies :: [Fortran Anno] -> Fortran Anno
combineBodies =
  buildAstSeq (FSeq nullAnno nullSrcSpan) (NullStmt nullAnno nullSrcSpan)

combineDecls :: [Decl Anno] -> Decl Anno
combineDecls = buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan)

combineArgs :: [ArgName Anno] -> Arg Anno
combineArgs args =
  Arg nullAnno (buildAstSeq (ASeq nullAnno) (NullArg nullAnno) args) nullSrcSpan

getMergedName :: [SubRec] -> String
getMergedName items = (foldl buildString "" items) ++ "_merged"
 where
  buildString :: String -> SubRec -> String
  buildString acc cur =
    if acc /= "" then acc ++ "_" ++ (subName cur) else subName cur

-- -- get subrecs with entires in their argumentTranslation table
getSubRoutinesThatMakeCalls :: SubroutineTable -> [SubRec]
getSubRoutinesThatMakeCalls srt =
  filter (\subrec -> numberOfCallsMade subrec > 0) $ DMap.elems srt
  where numberOfCallsMade = length . DMap.keys . argTranslations

getSubCalls :: SubRec -> [String]
getSubCalls subrec = DMap.keys (argTranslations subrec)

resolveConflictsWithLocalDecls
  :: [([ArgumentTranslation], SubRec)] -> [([ArgumentTranslation], SubRec)]
resolveConflictsWithLocalDecls pairs = if hasBeenUpdated
  then resolveConflictsWithLocalDecls result
  else result
 where
  allLocalDecls = concatMap (getNonParameterDeclarations . snd) pairs
  allArgTrans   = concatMap fst pairs
  conflicts     = findConflicts allLocalDecls allArgTrans
  foldResults (updated, curResult) (updatedAcc, resultAcc) =
    (updated || updatedAcc, (curResult : resultAcc))
  (hasBeenUpdated, result) =
    foldr foldResults (False, []) withConflictsResolved
  withConflictsResolved =
    map (updateArgTransToRespectLocalVariables conflicts) pairs

findConflicts :: [String] -> [ArgumentTranslation] -> [String]
findConflicts localDecls argTrans = filter (\decl -> decl `elem` argNames)
                                           localDecls
 where
  argumentVarNames = map (\(argTran) -> argument argTran) argTrans
  argNames         = map (\(VarName _ name) -> name) argumentVarNames

updateArgTransToRespectLocalVariables
  :: [String]
  -> ([ArgumentTranslation], SubRec)
  -> (Bool, ([ArgumentTranslation], SubRec))
updateArgTransToRespectLocalVariables conflicts (argTrans, subrec) =
  (runAgain, (conflictFreeArgTrans, subrec))
 where
  (runAgain, conflictFreeArgTrans) =
    foldr foldResults (False, []) updatedArgTrans
  updatedArgTrans = map (updateArgTran conflicts) argTrans
  foldResults (updated, argTran) (updatedAcc, argTransAcc) =
    (updated || updatedAcc, argTran : argTransAcc)

updateArgTran :: [String] -> ArgumentTranslation -> (Bool, ArgumentTranslation)
updateArgTran localDecls a@(ArgTrans param arg@(VarName anno name)) =
  if (name `elem` localDecls)
    then (True, ArgTrans param (VarName anno (name ++ "_F4_cas")))
    else (False, a)

getNonParameterDeclarations :: SubRec -> [String]
getNonParameterDeclarations subrec = (decls \\ arguments)
 where
  arguments = getArgsAsString $ subAst subrec
  decls     = getDeclNames $ subAst subrec

getUniqueArgs :: [ArgName Anno] -> [ArgName Anno]
getUniqueArgs args = removeDuplicates getArgName args

getAllArgs :: [SubRec] -> [ArgName Anno]
getAllArgs subrecs = concatMap (\subrec -> getArgs $ subAst subrec) subrecs

orderDeclsByIntents :: Decl Anno -> Decl Anno -> Ordering
orderDeclsByIntents (Decl _ _ _ typeDecl1) (Decl _ _ _ typeDecl2) =
  case results of
    (Just (In    _), Just (Out _)  ) -> GT
    (Just (In    _), Just (InOut _)) -> GT
    (Just (Out   _), Just (In _)   ) -> GT
    (Just (Out   _), Just (InOut _)) -> GT
    (Just (InOut _), Just (In _)   ) -> LT
    (Just (InOut _), Just (Out _)  ) -> LT
    (Nothing       , Just _        ) -> GT
    (Just _        , Nothing       ) -> GT
    (_             , _             ) -> EQ
 where
  results = (getIntent typeDecl1, getIntent typeDecl2)
  getIntent attrs = case ((concatMap (getIntentAttrs)) . getAttrs) attrs of
    (Intent _ intentType) : is -> Just intentType
    []                         -> Nothing
  getIntentAttrs attr = case attr of
    i@(Intent _ _) -> [i]
    _              -> []
orderDeclsByIntents _ _ =
  error "Can't get ordering for declarations other than Decl"

getUniqueDecls :: [Decl Anno] -> [Decl Anno]
getUniqueDecls decls = map (\(key, val) -> head val)
  $ DMap.toList mapValsSorted
 where
  mapValsSorted = DMap.map (sortBy orderDeclsByIntents) declMap
  pairsForMap   = map (\item -> (declNameAsString item, item)) decls
  initialMap    = DMap.fromList $ map (\key -> (key, [])) uniqueKeys
  uniqueKeys    = map declNameAsString $ removeDuplicates declNameAsString decls
  declMap       = foldr updateMap initialMap pairsForMap
  updateMap
    :: (String, Decl Anno)
    -> DMap.Map String [Decl Anno]
    -> DMap.Map String [Decl Anno]
  updateMap (key, val) map = DMap.adjust ([val] ++) key map

getUniqueArgTrans :: [ArgumentTranslation] -> [ArgumentTranslation]
getUniqueArgTrans argTrans = removeDuplicates argument argTrans

getAllDecls :: [SubRec] -> [Decl Anno]
getAllDecls subrecs = concatMap (\subrec -> getDecls $ subAst subrec) subrecs

replaceParametersWithArgumentNames :: [ArgumentTranslation] -> SubRec -> SubRec
replaceParametersWithArgumentNames argTransForCall callee = updatedAst
 where
  updatedAst = foldr preformUpdates callee argTransForCall
  preformUpdates argTran =
    ((renameParameter argTran) . (replaceOneParamUsageWithArg argTran))

renameParameter :: ArgumentTranslation -> SubRec -> SubRec
renameParameter argTrans subRec = subRec
  { subAst = everywhere (mkT renameQuery) $ subAst subRec
  }
 where
  renameQuery curVal@(ArgName anno name)
    | (  (name == (getArgName $ parameter argTrans))
      && (not . hasBeenUpdated) anno
      )
    = ArgName updatedAnno $ getNameFromVarName (argument argTrans)
    | otherwise
    = curVal
  renameQuery curVal = curVal

replaceOneParamUsageWithArg :: ArgumentTranslation -> SubRec -> SubRec
replaceOneParamUsageWithArg argTrans subRec = subRec
  { subAst = everywhere (mkT replaceQuery) $ subAst subRec
  }
 where
  replaceQuery curVal@(VarName anno name)
    | (  (name == (getArgName $ parameter argTrans))
      && (not . hasBeenUpdated) anno
      )
    = VarName updatedAnno $ getNameFromVarName (argument argTrans)
    | otherwise
    = curVal

updatedAnno :: Anno
updatedAnno = DMap.singleton (mergeSubsAnnoKey) []

mergeSubsAnnoKey = "msak"
hasBeenUpdated anno = DMap.member mergeSubsAnnoKey anno


