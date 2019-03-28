{-# LANGUAGE RecordWildCards #-}

module DetectIndividualPipelines where

import           Data.Generics
import           Data.List
import           Data.List.Index
import           Data.List.Unique
import           Data.List.Utils
import qualified Data.Map             as DMap
import           Data.Tuple.Utils
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

updateSubTablesWithPipelines ::
     SubRec -> SubroutineTable -> IO ([String], SubroutineTable)
updateSubTablesWithPipelines mergedOffload@MkSubRec {..} subTable = do
  (pipelineNames, pipelines) <- splitMergedMethodIntoPipelines mergedOffload
  let pipelinesAdded =
        foldl
          (\map subRec@MkSubRec {..} -> DMap.insert subName subRec map)
          originalMergedDeleted
          pipelines
  return (pipelineNames, pipelinesAdded)
  where
    originalMergedDeleted = DMap.delete subName subTable

-- writeOutReadBackPipeline :: FilePath -> SubRec -> IO SubRec
-- writeOutReadBackPipeline tempDir MkSubRec{..} = do
--
--   where
--     tempFile = tempDir </> subName
splitMergedMethodIntoPipelines :: SubRec -> IO ([String], [SubRec])
splitMergedMethodIntoPipelines MkSubRec {..} = do
  mapM_
    (\(nest, nestingData) -> do
       putStrLn $ miniPPF nest
       putStrLn $ show nestingData
       putStrLn $ "\n----------------------------------------\n")
    topLevelBlocks
  putStrLn $ rule '=' ++ " Pipelines " ++ rule '-'
  mapM_
    (\MkSubRec {..} -> do
       putStrLn subName
       putStrLn $ miniPPProgUnit subAst)
    pipelines
  return (map Utils.subName pipelines, pipelines)
  where
    pipelines = map (buildOne allArgs allDecls) groupedByNestLevel
    allDecls = getDecls subAst
    allArgs = getArgsAsString subAst
    topLevelBlocks = getTopLevelBlockStatements $ getSubBody subAst
    groupedByNestLevel = groupTopLevelBlocks topLevelBlocks

buildOne ::
     [String]
  -> [Decl Anno]
  -> (Int, [(Fortran Anno, (String, Int, Int))])
  -> SubRec
buildOne allArgs allDecls (pipelineNumber, pipelineItems) =
  MkSubRec
    { subName = pipelineName
    , subAst = newSub
    , subSrcFile = ""
    , subSrcLines = []
    , argTranslations = DMap.empty
    , parallelise = True
    }
  where
    pipelineBody = combinePipelineItems $ map fst pipelineItems
    allUsedVars = getUsedVarNames pipelineBody
    pipelineName =
      "pipeline_" ++
      show pipelineNumber ++
      "_" ++ intercalate "_" (uniq $ map (fst3 . snd) pipelineItems)
    newSub =
      Sub
        nullAnno
        nullSrcSpan
        Nothing
        (SubName nullAnno pipelineName)
        (argNode requiredArgs)
        newBlock
    newBlock =
      Block
        nullAnno
        nullUseBlock
        (ImplicitNone nullAnno)
        nullSrcSpan
        (declNode requiredDecls)
        pipelineBody
    requiredArgs = filter (`elem` allUsedVars) allArgs
    requiredDecls =
      filter
        (\decl -> (getNameFromVarName . getVarNameG) decl `elem` allUsedVars)
        allDecls

-- writeOutSubTable :: SubroutineTable -> FilePath -> IO ()
-- writeOutSubTable subTable tempDir = mapM_ writeOne $ DMap.elems subTable
--   where
--     writeOne :: SubRec -> IO ()
--     writeOne MkSubRec {..} = do
--       putStrLn $ "Writing to " ++ tempFileName
--       writeFile tempFileName (miniPPProgUnitNoTruncate subAst)
--       where
--         tempFileName = tempDir </> takeFileName subSrcFile
combinePipelineItems :: [Fortran Anno] -> Fortran Anno
combinePipelineItems items = block built
  where
    (_, built, _) =
      foldl combine (firstName, [], []) (map Just items ++ [Nothing])
    OriginalSubContainer _ firstName _ = head items
    combine ::
         (String, [Fortran Anno], [Fortran Anno])
      -> Maybe (Fortran Anno)
      -> (String, [Fortran Anno], [Fortran Anno])
    combine (currentName, built, currentBlock) (Just (OriginalSubContainer anno name body)) =
      if currentName == name
        then (name, built, currentBlock ++ [body])
        else ( name
             , built ++
               [OriginalSubContainer anno currentName (block currentBlock)]
             , [body])
    combine (currentName, built, currentBlock) Nothing =
      if currentBlock /= []
        then ( currentName
             , built ++
               [OriginalSubContainer nullAnno currentName (block currentBlock)]
             , [])
        else (currentName, built, currentBlock)

getUsedVarNames :: Fortran Anno -> [String]
getUsedVarNames fortran =
  sortUniq $ everything (++) (mkQ [] varNameQuery) fortran
  where
    varNameQuery :: VarName Anno -> [String]
    varNameQuery (VarName _ name) = [name]

groupTopLevelBlocks ::
     [(Fortran Anno, (String, Int, Int))]
  -> [(Int, [(Fortran Anno, (String, Int, Int))])]
groupTopLevelBlocks loopNests =
  indexed $ groupBy (\(_, (_, nl1, _)) (_, (_, nl2, _)) -> nl1 == nl2) loopNests

getTopLevelBlockStatements ::
     Fortran Anno -> [(Fortran Anno, (String, Int, Int))]
getTopLevelBlockStatements mergedBody = results
  where
    results =
      map (\ln -> (ln, getNestingLevelAndIteration ln)) $
      getLoopNests mergedBody

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
