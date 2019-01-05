module StencilDetection where

import           Data.Data
import           Data.Generics        (Data, Typeable, everything, everywhere,
                                       gmapQ, gmapT, mkQ, mkT)
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils

data Array = Array {
    varName       :: VarName Anno,
    arrDimensions :: Int
} deriving Show

detectStencils :: SubRec -> IO ()
detectStencils subrec = do
    putStrLn (concatMap (\d -> show d ++ "\n") arraysInSub)
    -- putStrLn (concatMap (\block -> ("\n----------------------------------\n" ++  miniPP d) $ findArrayAccesses arraysInSub subBody)
    putStrLn (concatMap (\set -> ("\n----------------------------------\n" ++ (concatMap (\d -> miniPP d ++ "\n") set))) $ findArrayAccesses arraysInSub subBody)
    where
        arraysInSub = map arrayFromDecl $ getArrayDecls subrec
        subBody = getSubBody $ subAst subrec

arrayFromDecl :: Decl Anno -> Array
arrayFromDecl decl@(Decl _ _ _ typeDecl) = Array { varName = name, arrDimensions = numberOfDimensions}
    where
        numberOfDimensions = length $ getArrayDimensions typeDecl
        name = getVarName decl

getArrayDimensions :: Type Anno -> [(Expr Anno, Expr Anno)]
getArrayDimensions declType = case declType of
                                (ArrayT _ dimensions _ _ _ _) -> dimensions
                                (BaseType _ _ attrs _ _) -> concatMap id $ concatMap getDimensionAttrs attrs
    where
        getDimensionAttrs attr = case attr of
            (Dimension _ dimensions) -> [dimensions]
            _                        -> []

getArrayDecls :: SubRec -> [Decl Anno]
getArrayDecls subrec = arrayDecls
    where
        arrayDecls = filter (isArrayDecl) $ getDecls (subAst subrec)

getDeclType :: Decl Anno -> Type Anno
getDeclType (Decl _ _ _ typeDecl) = typeDecl

isArrayDecl :: Decl Anno -> Bool
isArrayDecl decl = (not . null . getArrayDimensions . getDeclType) decl

findStencilAccessesAndAddNode :: [Array] -> Fortran Anno -> [Expr Anno]
findStencilAccessesAndAddNode arrays (OpenCLMap _ _ _ _ _ _ body) = findStencilAccessesAndAddNode arrays body
findStencilAccessesAndAddNode arrays (OpenCLReduce _ _ _ _ _ _ _ body) = findStencilAccessesAndAddNode arrays body
findStencilAccessesAndAddNode arrays body = arrayAccess
    where
        arrayAccess = getArrayReadsQuery body

getMapsAndFolds :: Fortran Anno -> [Fortran Anno]
getMapsAndFolds fortran = everything (++) (mkQ [] mapFoldQuery) fortran
    where
        mapFoldQuery :: Fortran Anno -> [Fortran Anno]
        mapFoldQuery fortran = case fortran of
            map@(OpenCLMap _ _ _ _ _ _ _)       -> [map]
            fold@(OpenCLReduce _ _ _ _ _ _ _ _) -> [fold]
            a@_                                 -> []

findArrayAccesses :: [Array] -> Fortran Anno -> [[Expr Anno]]
findArrayAccesses arrays fortran = map (findStencilAccessesAndAddNode arrays) mapAndFoldBlocks
    where
        mapAndFoldBlocks = getMapsAndFolds fortran

getArrayReadsQuery :: Fortran Anno -> [Expr Anno]
getArrayReadsQuery fortran = concatMap arrayReadQuery allReadExprs
    where
        arrayReadQuery expr = case expr of
                                var@(Var _ _ [(_, (idx:_))]) -> [var]
                                _                            -> []
        allReadExprs = case fortran of
                        (Assg _ _ _ rhs) -> [rhs]
                        (For _ _ _ start bound incre body) -> (start:bound:incre:(getArrayReadsQuery body))
                        (DoWhile _ _ bound body) -> [bound] ++ getArrayReadsQuery body
                        (FSeq _ _ fst snd) -> getArrayReadsQuery fst ++ getArrayReadsQuery snd
                        (If _ _ cond branch elseIfs elseBranch) ->
                            [cond] ++ getArrayReadsQuery branch ++ elseBranchResult
                            ++ branchConds ++ concatMap getArrayReadsQuery branchBodys
                            where
                                (branchConds, branchBodys) = unzip elseIfs
                                elseBranchResult = case elseBranch of
                                    (Just body) -> getArrayReadsQuery body
                                    _           -> []
                        (NullStmt _ _) -> []
                        missing@_ -> error ("Unimplemented Fortran Statement " ++ show missing)


