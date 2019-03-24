{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module RemoveConstantsFromStencils where

import           Control.Monad
import           Data.List
import           Data.List.Unique
import           Data.Maybe
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

data ArrayAccess = AA
  { arrayName          :: String
  , indices            :: [Index]
  , declaredDimensions :: [(Int, Int)]
  } deriving (Eq, Ord)

instance Show ArrayAccess where
  show AA {..} =
    "ArrayAccess: " ++ arrayName ++ " indices = " ++ show indices ++ "\n"

data Index
  = LoopVar String
  | Const Int
  deriving (Show, Eq, Ord)

artificalStencilSizeBound = 2

removeConstantsFromStencils :: SubRec -> IO SubRec
removeConstantsFromStencils subrec@MkSubRec {..} = do
  loopNestsAndArrayAccesses <- getLoopNestsAndStencilAccesses subAst
  mapM_ processLoopNest loopNestsAndArrayAccesses
  return subrec

processLoopNest :: (Fortran Anno, [ArrayAccess]) -> IO (Fortran Anno)
processLoopNest (loopNest, arrayAccesses) = do
  putStrLn $ miniPPF loopNest
  putStrLn $ show arrayAccesses
  when
    (isJust highestDimArrayAccessedWithConstant)
    (putStrLn $ (show . fromJust) highestDimArrayAccessedWithConstant)
  putStrLn $ "Nesting direction = " ++ show nestingDirection
  putStrLn "Updated = "
  putStrLn $ miniPPF withSyntheticLoopsInserted
  putStrLn "-------------------------------------------"
  return loopNest
  where
    withSyntheticLoopsInserted =
      insertSyntheticLoops
        highestDimArrayAccessedWithConstant
        (fromMaybe [] constantPositions)
        nestingDirection
        loopNest
    constantPositions =
      fmap getConstantPositions highestDimArrayAccessedWithConstant
    highestDimArrayAccessedWithConstant =
      getHighestDimensionArrayAccess accessesValidated
    accessesValidated = allConstantsUsedInSamePosition arrayAccesses
    loopVars = getLoopVariableByNestOrder loopNest
    nestingDirection = detectNestingDirection loopVars arrayAccesses

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
    for ("synthIdx" ++ show level) lwb (con upb) loopNest
  | level == maxLevel && loopBodyStatementsOnly loopNest = loopNest
  | insertLoop !! level && isJust originalFor =
    for
      ("synthIdx" ++ show level)
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
      ("synthIdx" ++ show level)
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
    origFor = fromJust originalFor
    originalFor =
      case getNearestFor loopNest of
        For anno srcSpan loopVarName expr1 expr2 expr3 fortran ->
          Just (For anno srcSpan loopVarName expr1 expr2 expr3)
        _ -> Nothing

getNestAtLevel :: Int -> Fortran Anno -> Fortran Anno
getNestAtLevel level loopNest = getNestAtLevel' level 0 loopNest
  where
    totalNumberOfNests = countLoopNests loopNest

countLoopNests :: Fortran Anno -> Int
countLoopNests = countLoopNests' 0

getNearestFor :: Fortran Anno -> Fortran Anno
getNearestFor (OriginalSubContainer _ _ body) = getNearestFor body
getNearestFor (FSeq _ _ f1 NullStmt {})       = getNearestFor f1
getNearestFor for@For {}                      = for
getNearestFor body                            = body

-- getNearestFor body
--   | loopBodyStatementsOnly body = body
--   | otherwise = error ("Can't strip loop nest from: \n" ++ miniPPF body)
stripLoopNest :: Fortran Anno -> Fortran Anno
stripLoopNest (OriginalSubContainer _ _ body) = stripLoopNest body
stripLoopNest (FSeq _ _ f1 NullStmt {}) = stripLoopNest f1
stripLoopNest (For _ _ _ _ _ _ body) = body
stripLoopNest body
  | loopBodyStatementsOnly body = body
  | otherwise = error ("Can't strip loop nest from: \n" ++ miniPPF body)

countLoopNests' :: Int -> Fortran Anno -> Int
countLoopNests' currentCount (OriginalSubContainer _ _ body) =
  countLoopNests' currentCount body
countLoopNests' currentCount (FSeq _ _ f1 NullStmt {}) =
  countLoopNests' currentCount f1
countLoopNests' currentCount (For _ _ _ _ _ _ body) =
  countLoopNests' (currentCount + 1) body
countLoopNests' currentCount body
  | loopBodyStatementsOnly body = currentCount
  | otherwise =
    error ("Can't count number of loop nest in segment:\n" ++ miniPPF body)

getNestAtLevel' :: Int -> Int -> Fortran Anno -> Fortran Anno
getNestAtLevel' level currentLevel (OriginalSubContainer _ _ body) =
  getNestAtLevel' level currentLevel body
getNestAtLevel' level currentLevel (FSeq _ _ f1 NullStmt {}) =
  getNestAtLevel' level currentLevel f1
getNestAtLevel' level currentLevel for@(For _ _ _ _ _ _ body)
  | level == currentLevel = for
  | otherwise = getNestAtLevel' level (currentLevel + 1) body
getNestAtLevel' _ _ f = f -- error ("Pattern missing for =\n" ++ miniPPF f)

-- getNestAtLevel' level currentLevel from =
--   error
--     ("missing pattern for = \n" ++
--      miniPPF from ++
--      "\nlevel = " ++ show level ++ " currentLevel = " ++ show currentLevel)
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

getLoopNestsAndStencilAccesses ::
     ProgUnit Anno -> IO [(Fortran Anno, [ArrayAccess])]
getLoopNestsAndStencilAccesses mergedBody = do
  return loopNestsToArrayAccesses
  where
    loopNests = getInnerLoopNests $ getSubBody mergedBody
    allArrays = map (arrayFromDeclWithRanges True) $ getDecls mergedBody
    loopNestsToArrayAccesses =
      map (\ln -> (ln, parseArrayAccesses allArrays ln)) loopNests

data NestingDirection
  = Normal
  | Reverse
  | Undefined
  | Either
  deriving (Show, Eq)

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

getInnerLoopNests :: Fortran Anno -> [Fortran Anno]
getInnerLoopNests = go "" Nothing
  where
    go :: String -> Maybe (Fortran Anno) -> Fortran Anno -> [Fortran Anno]
    go name topLevel (FSeq _ _ f1 f2) =
      go name topLevel f1 ++ go name topLevel f2
    go _ _ (OriginalSubContainer _ name body) = go name Nothing body
    go name Nothing topLevel@(For _ _ _ _ _ _ body)
      | loopBodyStatementsOnly body = [osc name topLevel]
      | loopBodyOnlyContainsLoop body = go name (Just topLevel) body
      | otherwise = concatMap (go name Nothing) $ allFors body
    go name (Just topLevel) (For _ _ _ _ _ _ body)
      | loopBodyStatementsOnly body = [osc name topLevel]
      | loopBodyOnlyContainsLoop body = go name (Just topLevel) body
      | otherwise = concatMap (go name Nothing) $ allFors body
    go _ _ _ = []

allFors body =
  case body of
    FSeq _ _ f1 f2 -> allFors f1 ++ allFors f2
    for@For {}     -> [for]
    _              -> []
