module StencilDetection where

import           Data.Data
import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.Map              as DMap
import           Debug.Trace
import           F95IntrinsicFunctions
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Text.Read
import           Utils

detectStencilsInSubsToBeParallelise :: SubroutineTable -> SubroutineTable
detectStencilsInSubsToBeParallelise srt = updatedSrt
  where
    forOffloadSubs = filter parallelise $ DMap.elems srt
    withStencilsDetect = map detectStencils forOffloadSubs
    updatedSrt =
      foldr
        (\cur acc -> DMap.insert (subName cur) cur acc)
        srt
        withStencilsDetect

detectStencils :: SubRec -> SubRec
detectStencils subrec =
  removeRedundantStencilNodes $ addStencilNodes arraysInSub subrec
  where
    arraysInSub = map arrayFromDecl $ getArrayDecls (subAst subrec)

removeRedundantStencilNodes :: SubRec -> SubRec
removeRedundantStencilNodes subRec =
  subRec {subAst = everywhere (mkT removeStencilQuery) $ subAst subRec}
  where
    removeStencilQuery :: Fortran Anno -> Fortran Anno
    removeStencilQuery (OpenCLStencil anno srcspan stencils body) =
      OpenCLStencil anno srcspan stencils $
      everywhere (mkT removeInnerStenQuery) body
    removeStencilQuery curVal = curVal
    removeInnerStenQuery :: Fortran Anno -> Fortran Anno
    removeInnerStenQuery (OpenCLStencil _ _ _ body) = body
    removeInnerStenQuery curVal                     = curVal

addStencilNodes :: [Array] -> SubRec -> SubRec
addStencilNodes arrays subRec =
  subRec {subAst = everywhere (mkT addNodeQuery) $ subAst subRec}
  where
    addNodeQuery curVal@(OpenCLMap _ _ _ _ _ _ body)
      | (not . null) stencils =
        OpenCLStencil nullAnno nullSrcSpan stencils curVal
      | otherwise = curVal
      where
        stencils = createStencilRecords arrays $ findStencils arrays curVal
    addNodeQuery curVal@(OpenCLReduce _ _ _ _ _ _ _ body)
      | (not . null) stencils =
        OpenCLStencil nullAnno nullSrcSpan stencils curVal
      | otherwise = curVal
      where
        stencils = createStencilRecords arrays $ findStencils arrays curVal
    addNodeQuery curVal = curVal

createStencilRecords :: [Array] -> [[Expr Anno]] -> [Stencil Anno]
createStencilRecords arrays stencils =
  map (uncurry createOneStencilRecord) arrayDeclUsagePairs
  where
    arrayDeclMapItems =
      map (\array -> (getNameFromVarName $ varName array, array)) arrays
    arrayDeclMap = DMap.fromList arrayDeclMapItems
    stencilMapItems =
      map (\stencils -> (getArrayName $ head stencils, stencils)) stencils
    stenMap = DMap.fromList stencilMapItems
    requiredKeys = DMap.keys stenMap
    arrayDeclUsagePairs =
      map (\key -> (arrayDeclMap DMap.! key, stenMap DMap.! key)) requiredKeys

createOneStencilRecord :: Array -> [Expr Anno] -> Stencil Anno
createOneStencilRecord arrayDecl stencilArrayAccesses =
  Stencil nullAnno dimensions (length offsets) offsets arrayVarName
  where
    arrayVarName = getVarName $ head stencilArrayAccesses
    offsets = map (getOffsets . idxVarQuery) stencilArrayAccesses
    dimensions = arrDimensions arrayDecl

getOffsets :: [Expr Anno] -> [StencilIndex]
getOffsets = map getOffset

getOffset :: Expr Anno -> StencilIndex
getOffset t@(Bin _ _ (Plus _) loopVar (Con _ _ offset)) =
  Offset $ readIndex offset
getOffset t@(Bin _ _ (Minus _) loopVar (Con _ _ offset)) =
  Offset (negate $ readIndex offset)
getOffset t@(Var _ _ loopVar) = Offset 0
getOffset t@(Con _ _ val) = Constant $ readIndex val
getOffset missing = error ("getOffset pattern miss for: " ++ show missing)

findStencilAccesses :: [Array] -> Fortran Anno -> [Expr Anno]
findStencilAccesses arrays (OpenCLMap _ _ _ _ _ _ body) =
  findStencilAccesses arrays body
findStencilAccesses arrays (OpenCLReduce _ _ _ _ _ _ _ body) =
  findStencilAccesses arrays body
findStencilAccesses arrays body = arrayAccesses
  where
    arrayAccesses = everything (++) (mkQ [] (getArrayReads arrays)) body

getArrayName = getNameFromVarName . getVarName

sortVarNames :: Expr Anno -> Expr Anno -> Ordering
sortVarNames one two = name1 `compare` name2
  where
    name1 = getArrayName one
    name2 = getArrayName two

getMapsAndFolds :: Fortran Anno -> [Fortran Anno]
getMapsAndFolds = everything (++) (mkQ [] mapFoldQuery)
  where
    mapFoldQuery :: Fortran Anno -> [Fortran Anno]
    mapFoldQuery fortran =
      case fortran of
        map@OpenCLMap {}     -> [map]
        fold@OpenCLReduce {} -> [fold]
        a                    -> []

usesIndexVariablesAndConstantOffset ::
     [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)]
  -> [Expr Anno]
  -> [Expr Anno]
usesIndexVariablesAndConstantOffset idxVars stencilAccesses =
  getStencilsOnly simpleOpsOnly
  where
    getStencilsOnly = map fst
    loopVarNames =
      map (\(varName, _, _, _) -> getNameFromVarName varName) idxVars
    stencilsAndIdxNames = map stencilToStencilIdxNames stencilAccesses
    stencilsUsingLoopVars =
      getStencilsOnly $
      filter
        (\(_, indices) ->
           foldr (\cur acc -> acc && cur `elem` loopVarNames) True indices)
        stencilsAndIdxNames
    stencilsUsingLoopVarsAndIdxExprs =
      map stencilToIndexExprs stencilsUsingLoopVars
    simpleOpsOnly =
      filter
        (\(_, indices) -> all stencilOnlyContainsValidOps indices)
        stencilsUsingLoopVarsAndIdxExprs

stencilOnlyContainsValidOps :: Expr Anno -> Bool
stencilOnlyContainsValidOps = validateExprListContents getSimpleExprsQuery

getSimpleExprsQuery :: Expr Anno -> [Bool]
getSimpleExprsQuery expr =
  case expr of
    Bin {} -> [True]
    Con {} -> [True]
    Var {} -> [True]
    _      -> [False]

stencilToIndexExprs sten = (sten, idxVarQuery sten)

stencilToStencilIdxNames sten =
  ( sten
  , map getNameFromVarName $
    everything (++) (mkQ [] extractVarNamesFromExpr) $ idxVarQuery sten)

getLoopVariables ::
     [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)]
  -> Fortran Anno
  -> [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)]
getLoopVariables vars (OpenCLMap _ _ _ _ loopVariables _ body) =
  getLoopVariables (vars ++ loopVariables) body
getLoopVariables vars (OpenCLReduce _ _ _ _ loopVariables _ _ body) =
  getLoopVariables (vars ++ loopVariables) body
getLoopVariables vars _ =
  removeDuplicates (getNameFromVarName . varNameFromLoopIdx) vars

varNameFromLoopIdx (varname, _, _, _) = varname

findStencils :: [Array] -> Fortran Anno -> [[Expr Anno]]
findStencils arrays fortran = moreThanOneAccess
  where
    loopVariables =
      removeDuplicates (getNameFromVarName . varNameFromLoopIdx) $
      getLoopVariables [] fortran
    allAccesses = findStencilAccesses arrays fortran
    uniqueAccesses = removeDuplicates miniPP allAccesses
    usesLoopVarsOnly =
      usesIndexVariablesAndConstantOffset loopVariables uniqueAccesses
    sortedByVarName = sortBy sortVarNames usesLoopVarsOnly
    groupedByArray = groupBy groupByArray sortedByVarName
    groupByArray var1 var2 = getArrayName var1 == getArrayName var2
    moreThanOneAccess = filter (\grp -> length grp > 1) groupedByArray
