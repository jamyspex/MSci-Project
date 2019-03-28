module RefactorSyntheticStencilIndices where

import           Data.Generics
import           Data.List
import           Data.List.Unique
import           Data.Maybe
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

refactorStencilAcceses :: [Array] -> Fortran Anno -> Fortran Anno
refactorStencilAcceses arrays originalFortran
  -- stripControl (findBlocksToGuard arrays) originalFortran
 = go originalFortran
  where
    go :: Fortran Anno -> Fortran Anno
    go (FSeq anno srcSpan f1 f2) = FSeq anno srcSpan (go f1) (go f2)
    go (OriginalSubContainer _ name body) = go body
    go f@(For anno srcspn cond expr1 expr2 expr3 body)
      | blockStatementsOnly body =
        trace
          ("loopBodyStatementsOnly = \n" ++ miniPPF body)
          (For
             anno
             srcspn
             cond
             expr1
             expr2
             expr3
             (findBlocksToGuard arrays body))
      -- | loopBodyOnlyContainsLoop body =
      --   For anno srcspn cond expr1 expr2 expr3 (go body)
      | otherwise = f --For anno srcspn cond expr1 expr2 expr3 (go body)
      -- | otherwise = originalFortran
    --go (Just topLevel) (For anno srcspn cond expr1 expr2 expr3 body)
    --  | loopBodyStatementsOnly body =
    --    For anno srcspn cond expr1 expr2 expr3 (findBlocksToGuard arrays body)
    --  | loopBodyOnlyContainsLoop body =
    --    For anno srcspn cond expr1 expr2 expr3 (go (Just topLevel) body)
    --  | otherwise = originalFortran
    go f = f --originalFortran

-- refactorStencilAcceses :: [Array] -> Fortran Anno -> Fortran Anno
-- refactorStencilAcceses arrays originalFortran = go originalFortran
--   where
--     go :: Fortran Anno -> Fortran Anno
--     go fortran
--       | blockStatementsOnly fortran =
--         trace
--           ("processing = \n" ++ miniPPF fortran)
--           (findBlocksToGuard arrays fortran)
--       | otherwise = stripControl fortran go
stripControl :: (Fortran Anno -> Fortran Anno) -> Fortran Anno -> Fortran Anno
-- stripControl trans (FSeq anno srcSpan f1 f2) =
--   FSeq anno srcSpan (stripControl trans f1) (stripControl trans f2)
stripControl trans (OriginalSubContainer _ name body) = stripControl trans body
stripControl trans fortran
  | blockStatementsOnly fortran =
    case fortran of
      (For anno srcSpan varname expr1 expr2 expr3 body) ->
        For anno srcSpan varname expr1 expr2 expr3 (trans body)
      (If anno srcSpan cond ifBody ifElses elseBody) ->
        If
          anno
          srcSpan
          cond
          (trans ifBody)
          (map (\(cond, body) -> (cond, trans body)) ifElses)
          (fmap trans elseBody)
      _ -> fortran
  | otherwise =
    case fortran of
      (For anno srcSpan varname expr1 expr2 expr3 body) ->
        For anno srcSpan varname expr1 expr2 expr3 (stripControl trans body)
      (If anno srcSpan cond ifBody ifElses elseBody) ->
        If
          anno
          srcSpan
          cond
          (stripControl trans ifBody)
          (map (\(cond, body) -> (cond, stripControl trans body)) ifElses)
          (fmap (stripControl trans) elseBody)
      _ -> fortran

-- stripControl (FSeq anno srcSpan f1 f2) trans =
--   FSeq anno srcSpan (trans f1) (trans f2)
-- stripControl (OriginalSubContainer anno name body) trans =
--   OriginalSubContainer anno name (trans body)
-- stripControl f trans = f -- error ("no case for \n" ++ miniPPF f)
blockStatementsOnly :: Fortran Anno -> Bool
blockStatementsOnly fortran =
  case fortran of
    For {} -> False
    If {} -> False
    FSeq _ _ f1 f2 -> blockStatementsOnly f1 && blockStatementsOnly f2
    OriginalSubContainer _ _ body -> blockStatementsOnly body
    _ -> True
    -- go topLevel@(For anno srcspn cond expr1 expr2 expr3 body)
    --   | loopBodyStatementsOnly body =
    --     For
    --       anno
    --       srcspn
    --       cond
    --       expr1
    --       expr2
    --       expr3
    --       (findBlocksToGuard arrays topLevel)
    --   | loopBodyOnlyContainsLoop body =
    --     For anno srcspn cond expr1 expr2 expr3 (go (Just topLevel) body)
    --   | otherwise = originalFortran
    -- go (For anno srcspn cond expr1 expr2 expr3 body)
    --   | loopBodyStatementsOnly body =
    --     For
    --       anno
    --       srcspn
    --       cond
    --       expr1
    --       expr2
    --       expr3
    --       (findBlocksToGuard arrays topLevel)
    --   | loopBodyOnlyContainsLoop body =
    --     For anno srcspn cond expr1 expr2 expr3 (go (Just topLevel) body)
    --   | otherwise = originalFortran
    -- go _ = originalFortran

-- refactorStencilAcceses :: [Array] -> Fortran Anno -> Fortran Anno
-- refactorStencilAcceses arrays originalFortran = go Nothing originalFortran
--   where
--     go :: Maybe (Fortran Anno) -> Fortran Anno -> Fortran Anno
--     go topLevel (FSeq _ _ f1 NullStmt {}) = go topLevel f1
--     go _ (OriginalSubContainer _ name body) = go Nothing body
--     go Nothing topLevel@(For anno srcspn cond expr1 expr2 expr3 body)
--       | loopBodyStatementsOnly body =
--         For
--           anno
--           srcspn
--           cond
--           expr1
--           expr2
--           expr3
--           (findBlocksToGuard arrays topLevel)
--       | loopBodyOnlyContainsLoop body =
--         For anno srcspn cond expr1 expr2 expr3 (go (Just topLevel) body)
--       | otherwise = originalFortran
--     go (Just topLevel) (For anno srcspn cond expr1 expr2 expr3 body)
--       | loopBodyStatementsOnly body =
--         For
--           anno
--           srcspn
--           cond
--           expr1
--           expr2
--           expr3
--           (findBlocksToGuard arrays topLevel)
--       | loopBodyOnlyContainsLoop body =
--         For anno srcspn cond expr1 expr2 expr3 (go (Just topLevel) body)
--       | otherwise = originalFortran
--     go _ _ = originalFortran
-- blockStatementsOnly :: Fortran Anno -> Bool
-- blockStatementsOnly fortran =
--   case fortran of
--     For {} -> False
--     If {} -> False
--     FSeq _ _ f1 f2 -> blockStatementsOnly f1 && blockStatementsOnly f2
--     OriginalSubContainer _ _ body -> blockStatementsOnly body
--     _ -> True
-- find blocks of statements leading up to and including an
-- array write. later this block will have all its stencil accesses
-- normalised to the array write.
findBlocksToGuard :: [Array] -> Fortran Anno -> Fortran Anno
findBlocksToGuard arrays wholeBlock =
  block $
  map
    (\(seg, guard) ->
       if guard
         then guardBlock arrays (block seg)
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
    getBlocks [] segs currentSeg =
      if tailSegmentShouldBeGuarded arrays currentSeg
        then segs ++ [(currentSeg, True)]
        else segs ++ [(currentSeg, False)]

tailSegmentShouldBeGuarded :: [Array] -> [Fortran Anno] -> Bool
tailSegmentShouldBeGuarded arrays = any checkStatement
  where
    checkStatement :: Fortran Anno -> Bool
    checkStatement statement = (not . null) $ filter isSynthIdx allIndices
      where
        arrayAccesses = getAllArrayAccesses arrays statement
        allIndices = concatMap getArrayIndices arrayAccesses

addGuards :: [(String, Int)] -> Fortran Anno -> Fortran Anno
addGuards guardValues body
  | (not . null) conditions = If nullAnno nullSrcSpan condition body [] Nothing
  | otherwise = body
  where
    condition = combineWithAnd conditions
    conditions = map (\(name, value) -> var name `eq` con value) guardValues

normaliseStencilWriteAndGuard :: [String] -> Fortran Anno -> Fortran Anno
normaliseStencilWriteAndGuard arrayNames fortran =
  addGuards requiredGuards $ updateStencilsInBlock arrayNames deltas fortran
  where
    requiredGuards =
      filter
        (\(name, _) -> synthIdxPrefix `isPrefixOf` name)
        writeStencilOffsets
    writeStencilOffsets = getWriteStencilOffsetsFromExpr arrayNames fortran
    deltas = map (\(name, offset) -> (name, negate offset)) writeStencilOffsets

updateStencilsInBlock ::
     [String] -> [(String, Int)] -> Fortran Anno -> Fortran Anno
updateStencilsInBlock arrayNames deltas = everywhere (mkT updateStencilsQuery)
  where
    updateStencilsQuery :: Expr Anno -> Expr Anno
    updateStencilsQuery expr
      | isMatchingArray expr = updateIndices expr
      | otherwise = expr
    updateIndices :: Expr Anno -> Expr Anno
    updateIndices (Var anno srcSpan [(VarName vnAnno name, indices)]) =
      Var
        anno
        srcSpan
        [(VarName vnAnno name, map (uncurry buildIndex) updatedIndices)]
      where
        currentTuples = map getNameAndOffsetFromExpr indices
        deltasToCurrent = zip deltas currentTuples
        updatedIndices =
          map
            (\((dName, delta), (cName, value)) ->
               if dName == cName
                 then (dName, value + delta)
                 else error "Current name and delta names don't match.")
            deltasToCurrent
    isMatchingArray :: Expr Anno -> Bool
    isMatchingArray (Var _ _ [(VarName _ name, indices)])
      -- trace
      --   ("isMatchingArray: " ++
      --    show result ++
      --    " = " ++
      --    name ++
      --    " `elem` " ++
      --    show arrayNames ++
      --    " && " ++ show indexNames ++ " == " ++ show (map fst deltas))
     = result
      where
        indexNames = map (getNameFromVarName . getVarNameG) indices
        result = name `elem` arrayNames && indexNames == map fst deltas
    isMatchingArray _ = False

getWriteStencilOffsetsFromExpr :: [String] -> Fortran Anno -> [(String, Int)]
getWriteStencilOffsetsFromExpr arrays fortran@(Assg _ _ (Var _ _ [(VarName _ name, indices)]) _)
  | isArrayStencilWrite arrays fortran = map getNameAndOffsetFromExpr indices
  | otherwise = []

getDefaultOffsets :: [Array] -> Fortran Anno -> [(String, Int)]
getDefaultOffsets arrays fortran =
  sortUniq $
  filter (\(name, value) -> synthIdxPrefix `isPrefixOf` name && value == 0) $
  map getNameAndOffsetFromExpr indices
  where
    allArrayAccesses = getAllArrayAccesses arrays fortran
    indices = concatMap (\(Var _ _ [(_, indices)]) -> indices) allArrayAccesses

getNameAndOffsetFromExpr :: Expr Anno -> (String, Int)
getNameAndOffsetFromExpr t@(Bin _ _ (Plus _) loopVar (Con _ _ offset)) =
  ((getNameFromVarName . getVarName) loopVar, readIndex offset)
getNameAndOffsetFromExpr t@(Bin _ _ (Minus _) loopVar (Con _ _ offset)) =
  ((getNameFromVarName . getVarName) loopVar, negate $ readIndex offset)
getNameAndOffsetFromExpr t@(Var _ _ [(loopVar, _)]) =
  (getNameFromVarName loopVar, 0)
getNameAndOffsetFromExpr missing =
  error ("getNameAndOffsetFromExpr pattern miss for: " ++ show missing)

guardBlock :: [Array] -> Fortran Anno -> Fortran Anno
guardBlock arrays blockToBeGuarded
  | syntheticIdxArrayStencilWrite arrayWrites blockToBeGuarded =
    normaliseStencilWriteAndGuard arrayNames blockToBeGuarded -- do the normalisation and add then add guards
  | syntheticLoopIndicesWritten arrayWrites blockToBeGuarded ||
      syntheticLoopIndicesRead arrayReads blockToBeGuarded =
    addGuards (getDefaultOffsets arrays blockToBeGuarded) blockToBeGuarded
  | otherwise = blockToBeGuarded
  where
    arrayWrites = getArrayAccesses ArrayWrite arrays blockToBeGuarded
    arrayReads = getArrayAccesses ArrayRead arrays blockToBeGuarded
    arrayNames = map (getNameFromVarName . arrayVarName) arrays

isArrayStencilWrite :: [String] -> Fortran Anno -> Bool
isArrayStencilWrite arrayNames (Assg _ _ (Var _ _ [(VarName _ name, _)]) _) =
  name `elem` arrayNames
isArrayStencilWrite _ _ = False

syntheticIdxArrayStencilWrite :: [Expr Anno] -> Fortran Anno -> Bool
syntheticIdxArrayStencilWrite arrayWrites fortran
  -- trace
  --   ("syntheticIdxArrayStencilWrite = \n" ++
  --    miniPPF fortran ++ " usingSynthIdx = " ++ show usingSynthIdx)
 = any isStencilIndex usingSynthIdx
  where
    usingSynthIdx = filter isSynthIdx allIndices
    allIndices = concatMap getArrayIndices arrayWrites

getArrayIndices (Var _ _ [(_, indices)]) = indices
getArrayIndices _                        = []

isStencilIndex expr =
  case expr of
    (Bin _ _ _ v@Var {} c@Con {}) -> True
    _                             -> False

syntheticLoopIndicesWritten :: [Expr Anno] -> Fortran Anno -> Bool
syntheticLoopIndicesWritten arrayWrites fortran
  -- trace
  --   ("syntheticLoopIndicesWritten: \nfortran =\n" ++
  --    miniPPF fortran ++
  --    "\n" ++
  --    concatMap (\e -> "\t" ++ miniPP e ++ "\n") usingSynthIdx ++
  --    "\n------------------------------------\n")
 = (not . null) usingSynthIdx
  where
    usingSynthIdx = filter isSynthIdx allIndices
    allIndices = concatMap (\(Var _ _ [(_, indices)]) -> indices) arrayWrites

syntheticLoopIndicesRead :: [Expr Anno] -> Fortran Anno -> Bool
syntheticLoopIndicesRead arrayReads fortran
  -- trace
  --   ("syntheticLoopIndicesRead: \nfortran =\n" ++
  --    miniPPF fortran ++
  --    "\n" ++
  --    concatMap (\e -> "\t" ++ miniPP e ++ "\n") usingSynthIdx ++
  --    "\n------------------------------------\n")
 = (not . null) usingSynthIdx
  where
    usingSynthIdx = filter isSynthIdx allIndices
    allIndices = concatMap (\(Var _ _ [(_, indices)]) -> indices) arrayReads

isSynthIdx idxExpr =
  any (isPrefixOf synthIdxPrefix) $
  map getNameFromVarName $ getAllVarNames idxExpr

stmtsQuery :: Fortran Anno -> [Fortran Anno]
stmtsQuery (OriginalSubContainer _ _ body) = stmtsQuery body
stmtsQuery (FSeq _ _ f1 f2)                = stmtsQuery f1 ++ stmtsQuery f2
stmtsQuery (For _ _ _ _ _ _ body)          = stmtsQuery body
-- stmtsQuery (If _ _ _ ifBody ifElses elseBody) =
--   stmtsQuery ifBody ++
--   concatMap (\(_, body) -> stmtsQuery body) ifElses ++
--   case elseBody of
--     Nothing -> []
--     Just eb -> stmtsQuery eb
-- stmtsQuery (TempGuard _ body)              = stmtsQuery body
stmtsQuery NullStmt {}                     = []
stmtsQuery fortran                         = [fortran]
