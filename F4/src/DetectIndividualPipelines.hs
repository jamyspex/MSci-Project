{-# LANGUAGE RecordWildCards #-}

module DetectIndividualPipelines where

import           Data.Generics
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

splitMergedMethodInPipelines :: SubRec -> IO [SubRec]
splitMergedMethodInPipelines MkSubRec {..} = do
  getTopLevelBlockStatements $ getSubBody subAst
  return []

getTopLevelBlockStatements :: Fortran Anno -> IO [Fortran Anno]
getTopLevelBlockStatements mergedBody = do
  mapM_
    (\nest -> do
       putStrLn $ miniPPF nest
       putStrLn $ show $ getNestingLevelAndIteration nest
       putStrLn $ "\n----------------------------------------\n")
    loopNests
  return []
  -- mapM_
  --   (\nest ->
  --      putStrLn $ miniPPF nest ++
  --   loopNests
  where
    loopNests = getLoopNests mergedBody

getNestingLevelAndIteration :: Fortran Anno -> (String, Int, Int)
getNestingLevelAndIteration (OriginalSubContainer _ name body) =
  descendLoopNest (name, 0, 1) body
  where
    descendLoopNest :: (String, Int, Int) -> Fortran Anno -> (String, Int, Int)
    descendLoopNest current (FSeq _ _ f1 f2) =
      case result1Larger of
        Prelude.GT -> result1
        Prelude.LT -> result2
        _          -> result1
      where
        result1@(_, nl1, ic1) = descendLoopNest current f1
        result2@(_, nl2, ic2) = descendLoopNest current f2
        result1Larger = nl1 `compare` nl2 <> ic1 `compare` ic2
    descendLoopNest (name, nestingLevel, totalIterations) (For _ _ _ initExpr upbExpr _ body) =
      descendLoopNest
        (name, nestingLevel + 1, totalIterations * (upbInt - lwbInt + 1))
        body
      where
        lwbInt = getIntFromExpr initExpr
        upbInt = getIntFromExpr upbExpr
    descendLoopNest result _ = result

getIntFromExpr :: Expr Anno -> Int
getIntFromExpr expr = head $ everything (++) (mkQ [] intConQuery) expr

intConQuery :: Expr Anno -> [Int]
intConQuery expr =
  case expr of
    (Con _ _ val) -> [read val :: Int]
    _             -> [] -- error "More complicated loop expr than u thought"
