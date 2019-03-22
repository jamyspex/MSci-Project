module AddKernelLoopGuards where

import           Data.Data
import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Data.List
import           Data.List.Index
import qualified Data.Map              as DMap
import           Data.Maybe
import           Debug.Trace
import           F95IntrinsicFunctions
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Text.Read
import           Utils

-- Wrapper function that uses everywhere to operate on the supplied subroutine
addLoopGuards :: SubRec -> SubRec
addLoopGuards subRec =
  subRec
    { subAst =
        everywhere (mkT (addLoopGuardToMapOrReduce declRangeMap arrays)) ast
    }
  where
    ast = subAst subRec
    arrayDecls = getArrayDecls ast
    arrays = map arrayFromDecl arrayDecls
    declRangeMap = getDimensionPositionMap arrayDecls

-- Wrapper to let the main function below work with maps and folds
addLoopGuardToMapOrReduce ::
     DMap.Map (String, Int) (Int, Int)
  -> [Array]
  -> Fortran Anno
  -> Fortran Anno
addLoopGuardToMapOrReduce arrayDeclDimMap arrays fortran@OpenCLMap {} =
  let (OpenCLMap oclAnno oclSrcSpan readVars writtenVars loopVars enclosedLoopVars body) =
        fortran
      newBody =
        addLoopGuardToMapOrReduce' arrayDeclDimMap arrays fortran loopVars body
   in OpenCLMap
        oclAnno
        oclSrcSpan
        readVars
        writtenVars
        loopVars
        enclosedLoopVars
        newBody
addLoopGuardToMapOrReduce arrayDeclDimMap arrays fortran@OpenCLReduce {} =
  let (OpenCLReduce oclAnno oclSrcSpan readVars writtenVars loopVars enclosedLoopVars reductionVars body) =
        fortran
      newBody =
        addLoopGuardToMapOrReduce' arrayDeclDimMap arrays fortran loopVars body
   in OpenCLReduce
        oclAnno
        oclSrcSpan
        readVars
        writtenVars
        loopVars
        enclosedLoopVars
        reductionVars
        newBody
addLoopGuardToMapOrReduce _ _ fortran = fortran

-- Main function used in everywhere query. Replaces OpenCLMap bodies with ones contained in an
-- if statement with appropriate conditions to guard against access to indices that were originally
-- controlled by the loop(s) the OpenCLMap has replaced. Only inserts and if statement if the orignal
-- loops bounds do not match the dimensions of the arrays accessed using the loop variables.
addLoopGuardToMapOrReduce' ::
     DMap.Map (String, Int) (Int, Int)
  -> [Array]
  -> Fortran Anno
  -> [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)]
  -> Fortran Anno
  -> Fortran Anno
addLoopGuardToMapOrReduce' arrayDeclDimMap arrays fortran loopVars body =
  newBody
  where
    allArrayAccesses = getAllArrayAccesses arrays fortran
    loopVarNames = map (\(VarName _ name, _, _, _) -> name) loopVars
    indexPositions =
      validateIndexingAndMakeUnique $
      concatMap (getLoopIndexPosition loopVarNames) allArrayAccesses
    loopVarsWithExprsAsConsts = map getLoopVarAsConstant loopVars
    arrayAccessDimMap =
      buildArrayAccessDimMap loopVarsWithExprsAsConsts indexPositions
    combinedMap = DMap.intersectionWith CMI arrayDeclDimMap arrayAccessDimMap
    requiredConditionList = getRequiredGuards $ DMap.elems combinedMap
    duplicatesRemoved =
      removeDuplicates
        (\(loopVar, bound) -> loopVar ++ show bound)
        requiredConditionList
    singleConditions = map (uncurry buildLoopGuard) duplicatesRemoved
    conditionExpr = combineWithAnd singleConditions
    bodyWithGuards = If nullAnno nullSrcSpan conditionExpr body [] Nothing
    newBody =
      if (not . null) duplicatesRemoved
        then bodyWithGuards
        else body

-- this function ensures that loop variables are used in a consistent order
-- e.g. a loop variable is always used to access the same array in the same position/dimension
-- this may need update so it respects square arrays, e.g. using
-- indices in different orders is valid as long as they have the same range
validateIndexingAndMakeUnique ::
     [(String, String, Maybe Int)] -> [(String, String, Int)]
validateIndexingAndMakeUnique positions =
  if valid
    then unique
    else error "Index ordering not consistent"
  where
    indexNotUsedRemove = filter (\(_, _, pos) -> isJust pos) positions
    maybeRemoved =
      map
        (\(arrayName, idxName, Just pos) -> (arrayName, idxName, pos))
        indexNotUsedRemove
    groupedByArrayNameAndIdxName = groupByArrayAndIndex maybeRemoved
    valid =
      all (\grp -> all (== head grp) (tail grp)) groupedByArrayNameAndIdxName
    unique = map head groupedByArrayNameAndIdxName

groupByArrayAndIndex :: [(String, String, Int)] -> [[(String, String, Int)]]
groupByArrayAndIndex input =
  groupBy
    (\(an1, in1, _) (an2, in2, _) -> an1 == an2 && in1 == in2)
    sortedByArrayName
  where
    sortedByArrayName =
      sortBy
        (\(an1, in1, _) (an2, in2, _) ->
           (an1 `compare` an2) <> (in1 `compare` in2))
        input

-- convert intermediary map entries to useful form
formatMapEntries :: ((String, Int), CombinedMapItem) -> (String, Int, Int)
formatMapEntries ((_, _), cmi) = (loopVarName, accessLWB, accessUPB)
  where
    (loopVarName, (accessLWB, accessUPB)) = accessItem cmi

-- Used to represent whether a bound is an upper or lower
data Bound
  = LWB Int
  | UPB Int
  deriving (Show)

-- Take all the pairs of combined access/decl dimensions and remove those
-- where the dimensions are equal and guards are not required. For those
-- that do require guards return (Loop var name, Bound) tuples to be
-- converted to conditions in the inserted guard.
-- NOTE At the moment this does conditionally insert the guards it just
-- think this makes it easier to derive the driver loop bounds.
getRequiredGuards :: [CombinedMapItem] -> [(String, Bound)]
getRequiredGuards = concatMap processOneCMI
  where
    processOneCMI :: CombinedMapItem -> [(String, Bound)]
    processOneCMI combinedMapItem =
      (loopVarName, LWB accessLWB) : [(loopVarName, UPB accessUPB)]
     -- potentialLowerBound ++ potentialUpperBound
        -- potentialLowerBound =
        --   if declLWB == accessLWB
        --     then []
        --     else [(loopVarName, LWB accessUPB)]
        -- potentialUpperBound =
        --   if declUPB == accessUPB
        --     then []
        --     else [(loopVarName, UPB accessUPB)]
      where
        (declLWB, declUPB) = declItem combinedMapItem
        (loopVarName, (accessLWB, accessUPB)) = accessItem combinedMapItem

-- data type to hold accessed dimensions along side declared dimensions
data CombinedMapItem = CMI
  { declItem   :: (Int, Int)
  , accessItem :: (String, (Int, Int))
  } deriving (Show)

-- Take a loopVar tuple from the OpenCLMap node and convert it to someting more ammeanable.
-- This discards increment atm
getLoopVarAsConstant ::
     (VarName Anno, Expr Anno, Expr Anno, Expr Anno) -> (String, Int, Int)
getLoopVarAsConstant (VarName _ loopVarName, lowerB, upperB, _) =
  (loopVarName, getSingleConstant lowerB, getSingleConstant upperB)

-- take a list of (loop vars name, lower bound, upper bound) and a list of (array name, loop var, maybe position)
-- and build a map of (array name, position) -> (loop var name, (loop var start index, loop var end index))
buildArrayAccessDimMap ::
     [(String, Int, Int)]
  -> [(String, String, Int)]
  -> DMap.Map (String, Int) (String, (Int, Int))
buildArrayAccessDimMap loopVarsAndBounds arrayAccessPos = DMap.fromList mapItems
  where
    loopVarMap =
      DMap.fromList $
      map (\(loopVarName, lb, ub) -> (loopVarName, (lb, ub))) loopVarsAndBounds
    mapItems = concatMap (buildOneItem loopVarMap) arrayAccessPos
    buildOneItem ::
         DMap.Map String (Int, Int)
      -> (String, String, Int)
      -> [((String, Int), (String, (Int, Int)))]
    buildOneItem loopVarMap (arrName, loopVarName, pos) =
      [((arrName, pos), (loopVarName, loopVarMap DMap.! loopVarName))]
  -- buildOneItem _ (_, _, Nothing) = []

-- combines multiple conditions produced by buildLoopGuard with .and.
combineWithAnd :: [Expr Anno] -> Expr Anno
combineWithAnd =
  buildAstSeq
    (Bin nullAnno nullSrcSpan (And nullAnno))
    (NullExpr nullAnno nullSrcSpan)

-- build one condition of the form (<loop variable> <=/>= <const>)
buildLoopGuard :: String -> Bound -> Expr Anno
buildLoopGuard loopVarName bound =
  ParenthesizedExpr nullAnno nullSrcSpan comparison
  where
    (operator, boundVal) =
      case bound of
        UPB boundVal -> (RelLE nullAnno, boundVal)
        LWB boundVal -> (RelGE nullAnno, boundVal)
    const = Con nullAnno nullSrcSpan $ show boundVal
    loopVar = Var nullAnno nullSrcSpan [(VarName nullAnno loopVarName, [])]
    comparison = Bin nullAnno nullSrcSpan operator loopVar const

-- returns a map of (array name, index position/dimension) -> (upperbound, lowerbound)
getDimensionPositionMap :: [Decl Anno] -> DMap.Map (String, Int) (Int, Int)
getDimensionPositionMap decls = DMap.fromList allItems
  where
    allItems = concatMap getDimensionPosition decls

-- returns a list of ((array name, index position/dimension), (upperbound, lowerbound)) tuples
-- for 1 decl statement only. List used accomadate mulit dimensional arrays e.g. second element of
-- first inner tuple will differ
getDimensionPosition :: Decl Anno -> [((String, Int), (Int, Int))]
getDimensionPosition decl = arrayDimensionsMapItem
  where
    name = declNameAsString decl
    arrayDimensionsMapItem =
      imap
        (\pos dims -> ((name, pos), getArrayDimensionConstants dims))
        arrayDimensionsExpr
    arrayDimensionsExpr = (getArrayDimensions . getDeclType) decl
