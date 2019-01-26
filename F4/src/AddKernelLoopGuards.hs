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
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Text.Read
import           Utils

addLoopGuards :: SubRec -> IO (SubRec)
addLoopGuards subRec = do
    putStrLn $ concatMap showUsagePositions positions
    putStrLn $ showDeclRangeMap declRangeMap
    return subRec
    where
        ast = subAst subRec
        arrayDecls = getArrayDecls ast
        arrays = map arrayFromDecl arrayDecls
        allMaps = everything (++) (mkQ [] oclMapQuery) ast
        positions = map (addLoopGuardToMap arrays) allMaps
        declRangeMap = getDimensionPositionMap arrayDecls

showDeclRangeMap :: DMap.Map (String, Int) (Int, Int) -> String
showDeclRangeMap declRangeMap = concatMap (\(key, val) -> show key ++ " ----> " ++ show val ++ "\n") $ DMap.toList declRangeMap


showUsagePositions (fortran, positionList) = miniPPF fortran ++ "\nidx positions\n" ++
                                            (concatMap (\(arrayName, idx, pos) -> arrayName ++ " idx var = " ++
                                            idx ++ " position = " ++ show pos ++ "\n") $ validateIndexingAndMakeUnique positionList)
                                            ++ "\n-----------------------------\n"

-- this may need update so it respects square arrays, e.g. using
-- indices in different orders is valid as long as they have the same range
validateIndexingAndMakeUnique :: [(String, String, Maybe Int)] -> [(String, String, Int)]
validateIndexingAndMakeUnique positions = if valid then unique else error "Index ordering not consistent"
    where
        indexNotUsedRemove = filter (\(_, _, pos) -> isJust pos) positions
        maybeRemoved = map (\(arrayName, idxName, Just pos) -> (arrayName, idxName, pos)) indexNotUsedRemove
        groupedByArrayNameAndIdxName = groupByArrayAndIndex maybeRemoved
        valid = all (\grp -> all (== head grp) (tail grp)) groupedByArrayNameAndIdxName
        unique = map head groupedByArrayNameAndIdxName
-- trace (show groupedByArrayNameAndIdxName)


groupByArrayAndIndex :: [(String, String, Int)] -> [[(String, String, Int)]]
groupByArrayAndIndex input = groupBy (\(an1, in1, _) (an2, in2, _) -> an1 == an2 && in1 == in2) sortedByArrayName
    where
        sortedByArrayName = sortBy (\(an1, in1, _) (an2, in2, _) -> (an1 `compare` an2) <> (in1 `compare` in2)) input

oclMapQuery fortran = case fortran of
                        (OpenCLMap _ _ _ _ _ _ _) -> [fortran]
                        _                         -> []

addLoopGuardToMap :: [Array] -> Fortran Anno -> (Fortran Anno, [(String, String, Maybe Int)])
addLoopGuardToMap arrays fortran@(OpenCLMap _ _ _ _ loopVars _ body) = (bodyWithGuards, indexPositions)
    where
        allArrayAcceses = getAllArrayAccesses arrays fortran
        loopVarNames = map (\(varname, _, _, _) -> varname) loopVars
        indexPositions = concatMap (getLoopIndexPosition loopVarNames) allArrayAcceses
        loopVarsWithExprsAsConsts = map (\(loopVarName, lowerB, upperB, _) ->
            (loopVarName, getSingleConstant lowerB, getSingleConstant upperB)) loopVars
        singleConditions = concatMap (\(loopVarName, lowerB, upperB) ->
            [buildLowB loopVarName lowerB, buildUppB loopVarName upperB]) loopVarsWithExprsAsConsts
        conditionExpr = combineWithAnd singleConditions
        bodyWithGuards = If nullAnno nullSrcSpan conditionExpr body [] Nothing

combineWithAnd :: [Expr Anno] -> Expr Anno
combineWithAnd conditions =
    buildAstSeq (Bin nullAnno nullSrcSpan (And nullAnno)) (NullExpr nullAnno nullSrcSpan) conditions

buildUppB = buildLoopGuard (RelLE nullAnno)

buildLowB = buildLoopGuard (RelGE nullAnno)

buildLoopGuard :: BinOp Anno -> VarName Anno -> Int -> Expr Anno
buildLoopGuard operator loopVarName bound = ParenthesizedExpr nullAnno nullSrcSpan comparison
    where
        const = Con nullAnno nullSrcSpan $ show bound
        loopVar = Var nullAnno nullSrcSpan [(loopVarName, [])]
        comparison = Bin nullAnno nullSrcSpan operator loopVar const

getDimensionPositionMap :: [Decl Anno] -> DMap.Map (String, Int) (Int, Int)
getDimensionPositionMap decls = DMap.fromList allItems
    where
        allItems = concatMap getDimensionPosition decls

getDimensionPosition :: Decl Anno -> [((String, Int), (Int, Int))]
getDimensionPosition decl = arrayDimensionsMapItem
    where
        name = (getNameFromVarName . getVarName) decl
        arrayDimensionsMapItem = imap (\pos dims -> ((name, pos), getArrayDimensionConstants dims)) arrayDimensionsExpr
        arrayDimensionsExpr = (getArrayDimensions . getDeclType) decl

getArrayDimensionConstants :: (Expr Anno, Expr Anno) -> (Int, Int)
getArrayDimensionConstants (expr1, expr2) = (getSingleConstant expr1, getSingleConstant expr2)

getSingleConstant :: Expr Anno -> Int
getSingleConstant expr = case expr of
    (Con _ _ val) -> read val :: Int
    (Unary _ _ _ (Con _ _ val)) -> negate $ read val :: Int
    (NullExpr _ _) -> 1 -- when array declared with starting index omitted, fortran defaults to 1
    _ -> error ("Expr other than constant in array dimensions. \n")

getLoopIndexPosition ::  [VarName Anno] -> Expr Anno -> [(String, String, Maybe Int)]
getLoopIndexPosition varNames (Var _ _ (((VarName _ arrayName), indexList):_)) =
    map (\(VarName _ name) -> (arrayName, name, getIndexPos name indexList 0)) varNames
    where
        getIndexPos :: String -> [Expr Anno] -> Int -> Maybe Int
        getIndexPos loopVariableName [] _ = Nothing
        getIndexPos loopVariableName (idx:idxs) position
            | null $ getAllVarNames idx = getIndexPos loopVariableName idxs (position + 1)
            | (getNameFromVarName . getVarName) idx == loopVariableName = Just position
            | otherwise = getIndexPos loopVariableName idxs (position + 1)

