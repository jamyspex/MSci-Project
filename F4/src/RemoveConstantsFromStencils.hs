{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module RemoveConstantsFromStencils where

import           Data.List
import           Data.List.Unique
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

data ArrayAccess = AA
  { arrayName  :: String
  , indices    :: [Index]
  , dimensions :: [(Int, Int)]
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
  getLoopNestsAndStencilArrayDecls subAst
  return subrec

getLoopVariableByNestOrder :: Fortran Anno -> [String]
getLoopVariableByNestOrder (OriginalSubContainer _ _ body) =
  getLoopVariableByNestOrder body
getLoopVariableByNestOrder (FSeq _ _ f1 NullStmt {}) =
  getLoopVariableByNestOrder f1
getLoopVariableByNestOrder (For _ _ (VarName _ loopVarName) _ _ _ body) =
  loopVarName : getLoopVariableByNestOrder body
getLoopVariableByNestOrder _ = []

getLoopNestsAndStencilArrayDecls ::
     ProgUnit Anno -> IO [(Fortran Anno, [ArrayAccess])]
getLoopNestsAndStencilArrayDecls mergedBody = do
  mapM_
    (\(ln, arrayAccesses, loopVarNestOrder) ->
       putStrLn
         (miniPPF ln ++
          "\n" ++
          show arrayAccesses ++
          "\n" ++
          show loopVarNestOrder ++
          "\n-------------------------------------------\n"))
    loopNestsToArrayAccesses
  return []
  where
    loopNests = getInnerLoopNests $ getSubBody mergedBody
    allArrays = map (arrayFromDeclWithRanges True) $ getDecls mergedBody
    loopNestsToArrayAccesses =
      map
        (\ln ->
           (ln, parseArrayAccesses allArrays ln, getLoopVariableByNestOrder ln))
        loopNests

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
        , dimensions = dimensionRanges
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
    For {}         -> False
    FSeq _ _ f1 f2 -> loopBodyStatementsOnly f1 && loopBodyStatementsOnly f2
    _              -> True

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
