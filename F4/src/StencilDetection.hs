module StencilDetection where

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
    putStrLn (concatMap (\d -> show d ++ "\n") $ arraysInSub)
    where
        arraysInSub = map arrayFromDecl $ getArrayDecls subrec

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

findStencilAccessesAndAddNode :: [Array] -> Fortran Anno -> Fortran Anno
findStencilAccessesAndAddNode arrays (OpenCLMap _ _ _ _ _ _ body) =  findStencilAccessesAndAddNode arrays body
findStencilAccessesAndAddNode arrays (OpenCLReduce _ _ _ _ _ _ _ body) = findStencilAccessesAndAddNode arrays body
findStencilAccessesAndAddNode arrays body = body

--     where
--         arrayAccess = body

-- get


-- Node to represent the data needed for an OpenCL map kernel
-- [VarName p]                           -- List of arguments to kernel that are READ
-- [VarName p]                           -- List of arguments to kernel that are WRITTEN
-- [(VarName p, Expr p, Expr p, Expr p)] -- Loop variables of nested maps
-- [VarName p] -- Loop variables of enclosing iterative loops
-- (Fortran p)                           -- Body of kernel code
-- | OpenCLReduce p SrcSpan
-- [VarName p]                           -- List of arguments to kernel that are READ
-- [VarName p]                           -- List of arguments to kernel that are WRITTEN
-- [(VarName p, Expr p, Expr p, Expr p)] -- Loop variables of nested reductions
-- [VarName p] -- Loop variables of enclosing iterative loops
-- [(VarName p, Expr p)]                 -- List of variables that are considered 'reduction variables' along with their initial values
-- (Fortran p)   )

