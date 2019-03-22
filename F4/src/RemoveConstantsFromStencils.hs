{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module RemoveConstantsFromStencils where

import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

data ArrayAccess = AA
  { arrayName :: String
  , indices   :: [Index]
  }

instance Show ArrayAccess where
  show AA {..} =
    "ArrayAccess: " ++ arrayName ++ " indices = " ++ show indices ++ "\n"

data Index
  = LoopVar String
  | Const Int
  deriving (Show)

removeConstantsFromStencils :: SubRec -> IO SubRec
removeConstantsFromStencils subrec@MkSubRec {..} = do
  getLoopNestsAndStencilArrayDecls subAst
  return subrec

getLoopNestsAndStencilArrayDecls ::
     ProgUnit Anno -> IO [(Fortran Anno, [ArrayAccess])]
getLoopNestsAndStencilArrayDecls mergedBody = do
  mapM_
    (\(ln, arrayAccesses) ->
       putStrLn
         (miniPPF ln ++
          "\n" ++
          show arrayAccesses ++
          "\n-------------------------------------------\n"))
    loopNestsToArrayAccesses
  return []
  where
    loopNests = getInnerLoopNests $ getSubBody mergedBody
    allArrays = map (arrayFromDeclWithRanges True) $ getDecls mergedBody
    loopNestsToArrayAccesses =
      map (\ln -> (ln, parseArrayAccesses allArrays ln)) loopNests

parseArrayAccesses :: [Array] -> Fortran Anno -> [ArrayAccess]
parseArrayAccesses allArrays loopNest = map buildArrayAccess allArrayAccess
  where
    allArrayAccess = getAllArrayAccesses allArrays loopNest
    buildArrayAccess :: Expr Anno -> ArrayAccess
    buildArrayAccess accessExpr =
      AA {arrayName = name, indices = map buildIndex indexExprs}
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
    go _ _ _ = [] --error ("Missing for = \n" ++ miniPPF for)

allFors body =
  case body of
    FSeq _ _ f1 f2 -> allFors f1 ++ allFors f2
    for@For {}     -> [for]
    _              -> []
-- go name preamble (FSeq _ _ f1 f2)
--   | isLoop f1 = osc name (block (preamble ++ [f1])) : go name [] f2
--   | isLoop f2 = [osc name (block $ preamble ++ [f1, f2])]
--   | isOSC f1 = go "" [] f1 ++ go "" [] f2
--   | isOSC f2 = go "" [] f2
--   | isFSeq f2 = go name (preamble ++ [f1]) f2
--   | otherwise = error "can't find main kernel body after preamble"
-- go _ _ (NullStmt _ _) = []
-- go name preamble fortran
--   | isLoop fortran = [osc name (block $ preamble ++ [fortran])]
--   | otherwise = error "encountered fortran that is not main body"
