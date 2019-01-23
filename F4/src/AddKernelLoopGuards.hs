module AddKernelLoopGuards where

import           Data.Data
import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Data.List
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
    putStrLn $ concatMap showPositions positions
    return subRec
    where
        ast = subAst subRec
        arrays = map arrayFromDecl $ getArrayDecls ast
        allMaps = everything (++) (mkQ [] oclMapQuery) ast
        positions = map (addLoopGuardToMap arrays) allMaps

showPositions (fortran, positionList) = miniPPF fortran ++ "\nidx positions\n" ++
                                            concatMap (\(arrayName, idx, pos) -> arrayName ++ " idx var = " ++
                                            idx ++ " position = " ++ show pos ++ "\n") positionList ++ "\n"
                                            ++ (if validateIndexing positionList then "all indexes valid" else "error in indexes")
                                            ++ "\n-----------------------------\n"


-- this may need update so it respects square arrays, e.g. using
-- indices in different orders is valid as long as they have the same range
validateIndexing :: [(String, String, Maybe Int)] -> Bool
validateIndexing positions = all (\grp -> all (== head grp) (tail grp)) groupedByArrayNameAndIdxName
    where
        indexNotUsedRemove = filter (\(_, _, pos) -> isJust pos) positions
        sortedByArrayName = sortBy (\(an1, _, _) (an2, _, _) -> an1 `compare` an2) indexNotUsedRemove
        groupedByArrayNameAndIdxName = groupBy (\(an1, in1, _) (an2, in2, _) -> an1 == an2 && in1 == in2) sortedByArrayName

oclMapQuery fortran = case fortran of
                        (OpenCLMap _ _ _ _ _ _ _) -> [fortran]
                        _                         -> []

addLoopGuardToMap :: [Array] -> Fortran Anno -> (Fortran Anno, [(String, String, Maybe Int)])
addLoopGuardToMap arrays fortran@(OpenCLMap _ _ _ _ loopVars _ body) = (fortran, indexPositions)
    where
        allArrayAcceses = getAllArrayAccesses arrays fortran
        loopVarNames = map (\(varname, _, _, _) -> varname) loopVars
        indexPositions = concatMap (getLoopIndexPosition loopVarNames) allArrayAcceses


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

