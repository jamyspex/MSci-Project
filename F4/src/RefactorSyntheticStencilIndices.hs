module RefactorSyntheticStencilIndices where

import           Data.Generics
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

refactorStencilAcceses :: [Array] -> Fortran Anno -> Fortran Anno
refactorStencilAcceses arrays originalFortran = go "" Nothing originalFortran
  where
    go :: String -> Maybe (Fortran Anno) -> Fortran Anno -> Fortran Anno
    go name topLevel (FSeq _ _ f1 NullStmt {}) = go name topLevel f1
    go _ _ (OriginalSubContainer _ name body) = go name Nothing body
    go name Nothing topLevel@(For anno srcspn cond expr1 expr2 expr3 body)
      | loopBodyStatementsOnly body =
        For
          anno
          srcspn
          cond
          expr1
          expr2
          expr3
          (findBlocksToGuard arrays topLevel)
      | loopBodyOnlyContainsLoop body =
        For anno srcspn cond expr1 expr2 expr3 (go name (Just topLevel) body)
      | otherwise = originalFortran
    go name (Just topLevel) (For anno srcspn cond expr1 expr2 expr3 body)
      | loopBodyStatementsOnly body =
        For
          anno
          srcspn
          cond
          expr1
          expr2
          expr3
          (findBlocksToGuard arrays topLevel)
      | loopBodyOnlyContainsLoop body =
        For anno srcspn cond expr1 expr2 expr3 (go name (Just topLevel) body)
      | otherwise = originalFortran
    go _ _ _ = originalFortran

-- refactorStencilAcceses :: [Array] -> Fortran Anno -> Fortran Anno
-- refactorStencilAcceses arrays fortran =
--   trace
--     (miniPPF fortran ++ "\n------------------------------------------\n")
--     fortran
--  | otherwise = fortran
transformInnerBlocks :: [Array] -> Fortran Anno -> Fortran Anno
transformInnerBlocks arrays f@(If anno srcSpan con ifBody ifElses elseBody) =
  If anno srcSpan con updatedBody updatedElseIfs updatedElseBody
  where
    updatedBody = findBlocksToGuardIfTerminal ifBody
    updatedElseIfs =
      map (\(cond, body) -> (cond, findBlocksToGuardIfTerminal body)) ifElses
    updatedElseBody = fmap findBlocksToGuardIfTerminal elseBody
    findBlocksToGuardIfTerminal wholeBlock =
      if blockStatementsOnly wholeBlock
        then findBlocksToGuard arrays wholeBlock
        else wholeBlock
transformInnerBlocks arrays fortran
  | blockStatementsOnly fortran = findBlocksToGuard arrays fortran
  | otherwise = fortran

blockStatementsOnly :: Fortran Anno -> Bool
blockStatementsOnly fortran =
  case fortran of
    For {} -> False
    If {} -> False
    FSeq _ _ f1 f2 -> blockStatementsOnly f1 && blockStatementsOnly f2
    OriginalSubContainer _ _ body -> blockStatementsOnly body
    _ -> True

-- find blocks of statements leading up to and including an
-- array write. later this block will have all its stencil accesses
-- normalised to the array write.
findBlocksToGuard :: [Array] -> Fortran Anno -> Fortran Anno
findBlocksToGuard arrays wholeBlock =
  block $
  map
    (\(seg, guard) ->
       if guard
         then (osc "this is will a guarded section" . block) seg
         else block seg)
    arrayWritesAndProceedingStatmentBlocks
  where
    allStatements = stmtsQuery wholeBlock
    arrayWritesAndProceedingStatmentBlocks =
      trace
        ("whole block = \n" ++
         miniPPF wholeBlock ++
         "\ntoGuard length = " ++
         show (length toGuardBlocks) ++
         "\nall statemetns = " ++
         concatMap (\s -> miniPPF s ++ "\n") allStatements ++
         "\nstatements = \n" ++
         concatMap
           (\(seg, guard) ->
              (if guard
                 then "guarded\n"
                 else "") ++
              concatMap (\s -> miniPPF s ++ "\n") seg ++
              "\n----------------------\n")
           toGuardBlocks)
        toGuardBlocks
    toGuardBlocks = getBlocks allStatements [] []
    arrayNames = map (getNameFromVarName . arrayVarName) arrays
    getBlocks ::
         [Fortran Anno]
      -> [([Fortran Anno], Bool)]
      -> [Fortran Anno]
      -> [([Fortran Anno], Bool)]
    getBlocks (s:ss) segs currentSeg =
      if isArrayStencilWrite arrayNames s
        then getBlocks ss (segs ++ [(currentSeg ++ [s], True)]) []
        else getBlocks ss segs (currentSeg ++ [s])
    getBlocks [] segs currentSeg = segs ++ [(currentSeg, False)]

normaliseStencilWrite fortran = fortran

guardBlock :: [Array] -> Fortran Anno -> Fortran Anno
guardBlock arrays blockToBeGuarded
  | syntheticIdxArrayStencilWrite arrayWrites blockToBeGuarded =
    guardBlock arrays (normaliseStencilWrite blockToBeGuarded) -- do the normalisation and add then add guards
  | syntheticLopIndicesWritten arrayWrites blockToBeGuarded ||
      syntheticLoopIndicesRead arrayReads blockToBeGuarded =
    blockToBeGuarded -- Add the guards
  | otherwise = blockToBeGuarded
  where
    arrayWrites = getArrayAccesses ArrayWrite arrays blockToBeGuarded
    arrayReads = getArrayAccesses ArrayRead arrays blockToBeGuarded

isArrayStencilWrite :: [String] -> Fortran Anno -> Bool
isArrayStencilWrite arrayNames (Assg _ _ (Var _ _ [(VarName _ name, _)]) _) =
  name `elem` arrayNames

syntheticIdxArrayStencilWrite :: [Expr Anno] -> Fortran Anno -> Bool
syntheticIdxArrayStencilWrite arrayWrites fortran =
  any isStencilIndex usingSynthIdx
  where
    usingSynthIdx =
      filter
        (\(Var _ _ [(VarName _ name, _)]) -> synthIdxPrefix `isPrefixOf` name)
        allIndices
    allIndices = concatMap (\(Var _ _ [(_, indices)]) -> indices) arrayWrites

isStencilIndex expr =
  case expr of
    (Bin _ _ _ v@Var {} c@Con {}) -> True
    _                             -> False

syntheticLopIndicesWritten :: [Expr Anno] -> Fortran Anno -> Bool
syntheticLopIndicesWritten arrayWrites fortran = (not . null) usingSynthIdx
  where
    usingSynthIdx =
      filter
        (\(Var _ _ [(VarName _ name, _)]) -> synthIdxPrefix `isPrefixOf` name)
        allIndices
    allIndices = concatMap (\(Var _ _ [(_, indices)]) -> indices) arrayWrites

syntheticLoopIndicesRead :: [Expr Anno] -> Fortran Anno -> Bool
syntheticLoopIndicesRead arrayReads fortran = (not . null) usingSynthIdx
  where
    usingSynthIdx =
      filter
        (\(Var _ _ [(VarName _ name, _)]) -> synthIdxPrefix `isPrefixOf` name)
        allIndices
    allIndices = concatMap (\(Var _ _ [(_, indices)]) -> indices) arrayReads

stmtsQuery :: Fortran Anno -> [Fortran Anno]
stmtsQuery (OriginalSubContainer _ _ body) = stmtsQuery body
stmtsQuery (FSeq _ _ f1 f2)                = stmtsQuery f1 ++ stmtsQuery f2
stmtsQuery (For _ _ _ _ _ _ body)          = stmtsQuery body
stmtsQuery fortran                         = [fortran]
