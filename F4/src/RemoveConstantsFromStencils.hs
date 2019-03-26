{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module RemoveConstantsFromStencils where

import           Control.Monad
import           Data.Generics
import           Data.List
import           Data.List.Index
import           Data.List.Unique
import qualified Data.Map              as DMap
import           Data.Maybe
import qualified Data.Set              as Set
import           Debug.Trace
import           F95IntrinsicFunctions
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils                 hiding (arrayName, nulSrcSpan)

data ArrayAccess = AA
  { arrayName          :: String
  , indices            :: [Index]
  , declaredDimensions :: [(Int, Int)]
  } deriving (Eq, Ord)

instance Show ArrayAccess where
  show AA {..} =
    "ArrayAccess: " ++ arrayName ++ " indices = " ++ show indices ++ "\n"

data NestingDirection
  = Normal
  | Reverse
  | Undefined
  | Either
  deriving (Show, Eq)

data Index
  = LoopVar String
  | Const Int
  deriving (Show, Eq, Ord)

removeConstantsFromStencilsAndPrint :: SubRec -> IO SubRec
removeConstantsFromStencilsAndPrint subRec@MkSubRec {..} = do
  putStrLn $ miniPPProgUnit updatedSub
  return subRec {subAst = updatedSub}
  where
    updatedSub = removeConstantsFromStencils subAst

removeConstantsFromStencils :: ProgUnit Anno -> ProgUnit Anno
removeConstantsFromStencils subAst = newSub
  where
    allArrays = map (arrayFromDeclWithRanges True) $ getDecls subAst
    updatedMergedBody =
      everywhere' (mkT (processMergedSubroutineTransform allArrays)) body
    updatedDecls = updateDecls subAst updatedMergedBody
    (Sub subAnno subSrcSpan returnType subName args block) = subAst
    (Block blockAnno uses implicit blockSrcSpan decls body) = block
    newBlock =
      Block blockAnno uses implicit blockSrcSpan updatedDecls updatedMergedBody
    newSub = Sub subAnno subSrcSpan returnType subName args newBlock

-- The wisdom here is that the only varnames that won't have declarations
-- are those added by this pass so add a decl for them. The only thing this
-- pass adds are loop counters so just make intDecls.
-- DEFINITELY THE MOST ROBUST APPROACH
-- Also this pass doesn't check for existing
-- variables named synthIdxN etc. soooo....
updateDecls :: ProgUnit Anno -> Fortran Anno -> Decl Anno
updateDecls originalSub newBody = declNode $ originalDecls ++ newDecls
  where
    originalDecls = getDecls originalSub
    originalDeclNames = getDeclNames originalSub
    allUsedVars = map getNameFromVarName $ getAllVarNames newBody
    originalDeclsSet = Set.fromList originalDeclNames
    newDecls =
      map intDecl $
      sortUniq $
      filter
        (\dn ->
           dn `Set.notMember` originalDeclsSet &&
           dn `notElem` f95IntrinsicFunctions)
        allUsedVars

isTopLevelLoop :: Fortran Anno -> Bool
isTopLevelLoop fortran
  | loopBodyOnlyContainsLoop fortran = isTopLevelLoop (stripLoopNest fortran)
  | loopBodyStatementsOnly fortran = True
  | otherwise = False

processMergedSubroutineTransform :: [Array] -> Fortran Anno -> Fortran Anno
processMergedSubroutineTransform allArrays fortran
  | isTopLevelLoop fortran = withGuardsAdded
  | otherwise = fortran
  where
    loopVarPosTuples =
      getLoopVarConstantPosTuples
        accessesValidated
        nestingDirection
        withSyntheticLoopsInserted
    replacementMap = buildIndexReplacementData loopVarPosTuples
    withGuardsAdded = addLoopGuards loopVarPosTuples withIndicesReplaced
    withIndicesReplaced =
      everywhere
        (mkT (doIndexReplacement replacementMap))
        withSyntheticLoopsInserted
    withSyntheticLoopsInserted =
      insertSyntheticLoops
        highestDimArrayAccessedWithConstant
        (fromMaybe [] constantPositions)
        nestingDirection
        fortran
    constantPositions =
      fmap getConstantPositions highestDimArrayAccessedWithConstant
    arrayAccesses = parseArrayAccesses allArrays fortran
    highestDimArrayAccessedWithConstant =
      getHighestDimensionArrayAccess accessesValidated
    accessesValidated = allConstantsUsedInSamePosition arrayAccesses
    loopVars = getLoopVariableByNestOrder fortran
    nestingDirection = detectNestingDirection loopVars arrayAccesses

filterLoopVarTups (_, _, _, index) =
  case index of
    Const _ -> True
    _       -> False

getMinimumIndexValue loopVarTups =
  let (_, _, _, Const min) = head $ sortByIndexValue loopVarTups
   in min

sortByIndexValue loopVarTups =
  sortBy (\(_, _, _, Const val1) (_, _, _, Const val2) -> val1 `compare` val2) $
  filter filterLoopVarTups loopVarTups

addLoopGuards :: [(Int, String, String, Index)] -> Fortran Anno -> Fortran Anno
addLoopGuards replacementTuples = insertGuards allConditions
  where
    grpdByLoopVar =
      groupBy (\(_, lv1, _, _) (_, lv2, _, _) -> lv1 == lv2) $
      sortBy
        (\(_, lv1, _, _) (_, lv2, _, _) -> lv1 `compare` lv2)
        replacementTuples
    requiredGuards =
      map (\(_, lv, _, Const val) -> (lv, val)) $
      concatMap (take 1 . sortByIndexValue) grpdByLoopVar
    buildCondition (loopVarName, blockEntryValue) =
      var loopVarName `eq` con blockEntryValue
    allConditions = map buildCondition requiredGuards

insertGuards :: [Expr Anno] -> Fortran Anno -> Fortran Anno
insertGuards conditions (OriginalSubContainer _ _ body) =
  insertGuards conditions body
insertGuards conditions (FSeq _ _ f1 NullStmt {}) = insertGuards conditions f1
insertGuards conditions (For anno src vn expr1 expr2 expr3 body) =
  For anno src vn expr1 expr2 expr3 (insertGuards conditions body)
insertGuards conditions body
  | loopBodyStatementsOnly body && null conditions = body
  | loopBodyStatementsOnly body =
    If nullAnno nullSrcSpan (combineWithAnd conditions) body [] Nothing
  | otherwise = error ("Can't add guards: \n" ++ miniPPF body)

buildIndexReplacementData ::
     [(Int, String, String, Index)] -- (pos, loopVar, array, index)
  -> DMap.Map (Int, Maybe Int) (Int, String, Int, Int) -- map (pos, constant val) -> tuples of form (position, loopVarName, offset, original constant value)
buildIndexReplacementData loopVarPosTuples =
  DMap.fromList $
  map
    ((\f@(pos, _, _, const) -> ((pos, Just const), f)) . buildOne)
    usesConstsOnly
  where
    minimumConstValue = getMinimumIndexValue loopVarPosTuples
    usesConstsOnly = filter filterLoopVarTups loopVarPosTuples
    buildOne :: (Int, String, String, Index) -> (Int, String, Int, Int)
    buildOne (position, loopVarName, arrayName, Const val) =
      (position, loopVarName, val - minimumConstValue, val)

doIndexReplacement ::
     DMap.Map (Int, Maybe Int) (Int, String, Int, Int) -> Expr Anno -> Expr Anno
doIndexReplacement replacementDataMap expr@(Var anno srcSpan [(VarName vnAnno name, indices)])
  | isArrayAcccesExprUsingConst expr =
    Var anno srcSpan [(VarName vnAnno name, updatedIndices)]
  | otherwise = expr
  where
    updatedIndices =
      imap
        (\pos expr ->
           case DMap.lookup (pos, getConstantValue expr) replacementDataMap of
             Nothing -> indices !! pos
             Just (_, loopVarName, offset, _) -> buildIndex loopVarName offset)
        indices
    getConstantValue idx =
      case idx of
        (Con _ _ val) -> Just (read val :: Int)
        _             -> Nothing
    buildIndex loopVarName offset
      | offset > 0 = var loopVarName `plus` con (abs offset)
      | offset < 0 = var loopVarName `minus` con (abs offset)
      | offset == 0 = var loopVarName
      | otherwise = error "Cannot build array index"
doIndexReplacement _ expr = expr

isArrayAcccesExprUsingConst :: Expr Anno -> Bool
isArrayAcccesExprUsingConst (Var _ _ [(VarName _ name, indices)]) =
  any
    (\case
       Con {} -> True
       _ -> False)
    indices
isArrayAcccesExprUsingConst _ = False

getLoopVarConstantPosTuples ::
     [ArrayAccess]
  -> NestingDirection
  -> Fortran Anno
  -> [(Int, String, String, Index)] -- tuples of form (position, loop var name, array name, index)
getLoopVarConstantPosTuples arrayAccesses nestDirection loopNest =
  concatMap (buildTuple loopVarsOrderedByNesting) $
  filter hasConst arrayAccesses
  where
    loopVarsOrderedByNesting =
      case nestDirection of
        Reverse -> reverse $ getLoopVariableByNestOrder loopNest
        Normal  -> getLoopVariableByNestOrder loopNest
        _       -> error "Unsupported nesting direction"
    mostIndicesCount =
      (length . indices . getHighestDimensionArrayAccessNoMaybe) arrayAccesses
    buildTuple :: [String] -> ArrayAccess -> [(Int, String, String, Index)]
    buildTuple loopVars AA {..} =
      imap
        (\pos (loopVar, index) ->
           let loopVarIndex = pos + (mostIndicesCount - length indices)
            in (pos, loopVars !! loopVarIndex, arrayName, index)) $
      zip loopVars indices

hasConst :: ArrayAccess -> Bool
hasConst AA {..} =
  any
    (\case
       Const _ -> True
       _ -> False)
    indices

insertSyntheticLoops ::
     Maybe ArrayAccess
  -> [Bool]
  -> NestingDirection
  -> Fortran Anno
  -> Fortran Anno
insertSyntheticLoops Nothing [] _ loopNest = loopNest
insertSyntheticLoops (Just AA {..}) constantPositions Normal loopNest =
  addLoops declaredDimensions constantPositions loopNest
insertSyntheticLoops (Just AA {..}) constantPositions Reverse loopNest =
  addLoops (reverse declaredDimensions) (reverse constantPositions) loopNest

addLoops dims insertLoop wholeLoopNest =
  addLoops' maxNestLevel 0 dims insertLoop wholeLoopNest
  where
    maxNestLevel = length dims - 1

addLoops' ::
     Int -> Int -> [(Int, Int)] -> [Bool] -> Fortran Anno -> Fortran Anno
addLoops' maxLevel level dims insertLoop loopNest
  | level == maxLevel && loopBodyStatementsOnly loopNest && insertLoop !! level =
    for loopVarName lwb (con upb) loopNest
  | level == maxLevel && loopBodyStatementsOnly loopNest = loopNest
  | insertLoop !! level && isJust originalFor =
    for
      loopVarName
      lwb
      (con upb)
      (origFor
         (addLoops'
            maxLevel
            (level + 1)
            dims
            insertLoop
            (stripLoopNest loopNest)))
  | insertLoop !! level =
    for
      loopVarName
      lwb
      (con upb)
      (addLoops' maxLevel (level + 1) dims insertLoop (stripLoopNest loopNest))
  | isJust originalFor =
    origFor $
    addLoops' maxLevel (level + 1) dims insertLoop (stripLoopNest loopNest)
  | otherwise =
    addLoops' maxLevel (level + 1) dims insertLoop (stripLoopNest loopNest)
  where
    (lwb, upb) = dims !! level
    loopVarName = "synthIdx" ++ show level
    origFor = fromJust originalFor
    originalFor =
      case getNearestFor loopNest of
        For anno srcSpan loopVarName expr1 expr2 expr3 fortran ->
          Just (For anno srcSpan loopVarName expr1 expr2 expr3)
        _ -> Nothing

getNearestFor :: Fortran Anno -> Fortran Anno
getNearestFor (OriginalSubContainer _ _ body) = getNearestFor body
getNearestFor (FSeq _ _ f1 NullStmt {})       = getNearestFor f1
getNearestFor for@For {}                      = for
getNearestFor body                            = body

stripLoopNest :: Fortran Anno -> Fortran Anno
stripLoopNest (OriginalSubContainer _ _ body) = stripLoopNest body
stripLoopNest (FSeq _ _ f1 NullStmt {}) = stripLoopNest f1
stripLoopNest (For _ _ _ _ _ _ body) = body
stripLoopNest body
  | loopBodyStatementsOnly body = body
  | otherwise = error ("Can't strip loop nest from: \n" ++ miniPPF body)

getHighestDimensionArrayAccessNoMaybe :: [ArrayAccess] -> ArrayAccess
getHighestDimensionArrayAccessNoMaybe =
  maximumBy
    (\a1 a2 ->
       length (declaredDimensions a1) `compare` length (declaredDimensions a2))

getHighestDimensionArrayAccess :: [ArrayAccess] -> Maybe ArrayAccess
getHighestDimensionArrayAccess allArrayAccesses =
  if null onlyWithConstants
    then Nothing
    else Just mostConstants
  where
    mostConstants =
      maximumBy
        (\a1 a2 ->
           length (declaredDimensions a1) `compare`
           length (declaredDimensions a2))
        onlyWithConstants
    onlyWithConstants =
      filter
        (\AA {..} ->
           any
             (\case
                LoopVar _ -> False
                Const _ -> True)
             indices)
        allArrayAccesses

allConstantsUsedInSamePosition :: [ArrayAccess] -> [ArrayAccess]
allConstantsUsedInSamePosition allArrayAccesses =
  if all (\grp -> all (== head grp) grp) constantPositions
    then allArrayAccesses
    else error
           "constants are not used in the same positions across stencils of same size"
  where
    constantPositions = map (map getConstantPositions) grpdByLength
    grpdByLength =
      groupBy
        (\a1 a2 ->
           (length . declaredDimensions) a1 == (length . declaredDimensions) a2)
        allArrayAccesses

getConstantPositions :: ArrayAccess -> [Bool]
getConstantPositions AA {..} =
  map
    (\case
       LoopVar _ -> False
       Const _ -> True)
    indices

getLoopVariableByNestOrder :: Fortran Anno -> [String]
getLoopVariableByNestOrder (OriginalSubContainer _ _ body) =
  getLoopVariableByNestOrder body
getLoopVariableByNestOrder (FSeq _ _ f1 NullStmt {}) =
  getLoopVariableByNestOrder f1
getLoopVariableByNestOrder (For _ _ (VarName _ loopVarName) _ _ _ body) =
  loopVarName : getLoopVariableByNestOrder body
getLoopVariableByNestOrder _ = []

detectNestingDirection :: [String] -> [ArrayAccess] -> NestingDirection
detectNestingDirection loopVarsInNestOrder arrayAccesses =
  if valid
    then firstNotEither
    else error "Index usage ordering not consistent"
  where
    allNestingDirections = map (checkOne loopVarsInNestOrder) arrayAccesses
    firstNotEither =
      head $
      filter
        (\case
           Either -> False
           _ -> True)
        allNestingDirections
    valid = all (\v -> v == firstNotEither || v == Either) allNestingDirections

checkOne :: [String] -> ArrayAccess -> NestingDirection
checkOne loopVars arrayAccess =
  case (forward, backward) of
    (True, True)   -> Either
    (True, _)      -> Normal
    (_, True)      -> Reverse
    (False, False) -> error "Can not detect loop nesting direction"
  where
    forward = go 0 loopVars accessLoopVarsOnly
    backward = go 0 (reverse loopVars) accessLoopVarsOnly
    accessLoopVarsOnly =
      (concatMap
         (\case
            LoopVar name -> [name]
            Const _ -> []) .
       indices)
        arrayAccess
    go :: Int -> [String] -> [String] -> Bool
    go misMatchCount (lv:lvs) (ai:ais)
      | misMatchCount == 2 = False
      | lv == ai = go misMatchCount lvs ais
      | lv /= ai = go (misMatchCount + 1) lvs ais
      | otherwise = go misMatchCount lvs ais
    go misMatchCount _ _
      | misMatchCount == 2 = False
      | otherwise = True

parseArrayAccesses :: [Array] -> Fortran Anno -> [ArrayAccess]
parseArrayAccesses allArrays loopNest = uniqueArrayAccesses
  where
    uniqueArrayAccesses = sortUniq allParsedArrayAccesses
    allParsedArrayAccesses = map buildArrayAccess allArrayAccessExprs
    allArrayAccessExprs =
      getAllArrayAccessesWithMatchingArray allArrays loopNest
    buildArrayAccess :: (Expr Anno, Array) -> ArrayAccess
    buildArrayAccess (accessExpr, Array {..}) =
      AA
        { arrayName = name
        , indices = map buildIndex indexExprs
        , declaredDimensions = dimensionRanges
        }
      where
        (Var _ _ [(VarName _ name, indexExprs)]) = accessExpr
        accessIndices = map
        buildIndex :: Expr Anno -> Index
        buildIndex expr =
          case expr of
            Con _ _ val -> Const (read val :: Int)
            Bin _ _ _ lhs _ -> LoopVar $ (getNameFromVarName . getVarName) lhs
            Var _ _ [(VarName _ name, _)] -> LoopVar name
            expr -> error ("missing pattern for \n" ++ miniPP expr)

loopBodyStatementsOnly :: Fortran Anno -> Bool
loopBodyStatementsOnly fortran =
  case fortran of
    For {} -> False
    FSeq _ _ f1 f2 -> loopBodyStatementsOnly f1 && loopBodyStatementsOnly f2
    OriginalSubContainer _ _ body -> loopBodyStatementsOnly body
    _ -> True

loopBodyOnlyContainsLoop :: Fortran Anno -> Bool
loopBodyOnlyContainsLoop fortran =
  case fortran of
    For {}                      -> True
    FSeq _ _ For {} NullStmt {} -> True
    _                           -> False
-- A beautiful function I worked out with a state machine when working on this
-- pass but is no longer used in final version :'(
--
-- Too happy with it to delete it!
--
-- getInnerLoopNests :: Fortran Anno -> [Fortran Anno]
-- getInnerLoopNests = go "" Nothing
--   where
--     go :: String -> Maybe (Fortran Anno) -> Fortran Anno -> [Fortran Anno]
--     go name topLevel (FSeq _ _ f1 f2) =
--       go name topLevel f1 ++ go name topLevel f2
--     go _ _ (OriginalSubContainer _ name body) = go name Nothing body
--     go name Nothing topLevel@(For _ _ _ _ _ _ body)
--       | loopBodyStatementsOnly body = [topLevel]
--       | loopBodyOnlyContainsLoop body = go name (Just topLevel) body
--       | otherwise = concatMap (go name Nothing) $ allFors body
--     go name (Just topLevel) (For _ _ _ _ _ _ body)
--       | loopBodyStatementsOnly body = [topLevel]
--       | loopBodyOnlyContainsLoop body = go name (Just topLevel) body
--       | otherwise = concatMap (go name Nothing) $ allFors body
--     go _ _ _ = []
