module Utils where

import           Data.Generics
import qualified Data.Map             as DMap
import           Language.Fortran
import           LanguageFortranTools

removeDuplicates :: Ord a => (b -> a) -> [b] -> [b]
removeDuplicates getKey input = DMap.elems uniqueMap
    where
        pairsForMap = map (\item -> (getKey item, item)) input
        uniqueMap = foldr addToMap DMap.empty pairsForMap
        addToMap :: Ord a => (a, b) -> DMap.Map a b -> DMap.Map a b
        addToMap (key, val) map = DMap.insert key val map


getAttrs typeDecl = case typeDecl of
    (BaseType _ _ attrs _ _) -> attrs
    (ArrayT _ _ _ attrs _ _) -> attrs

getSubBody :: ProgUnit Anno -> Fortran Anno
getSubBody (Sub _ _ _ _ _ (Block _ _ _ _ _ fortran)) = fortran
getSubBody (Module _ _ _ _ _ _ progUnits) = head $ map getSubBody progUnits

getDecls :: ProgUnit Anno -> [Decl Anno]
getDecls (Sub _ _ _ _ _ (Block _ _ _ _ decls _))  = everything (++) (mkQ [] getDeclsQuery) decls
getDecls (Module _ _ _ _ _ _ progUnits) = concatMap getDecls progUnits

getDeclsQuery :: Decl Anno -> [Decl Anno]
getDeclsQuery decl = case decl of
                        (Decl _ _ _ _) -> [decl]
                        _              -> []

getDeclNames :: ProgUnit Anno -> [String]
getDeclNames  (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) = map (getNameFromVarName . getVarName) declStatements
    where
        declStatements = everything (++) (mkQ [] declNameQuery) $ decls

getVarName decl = head $ everything (++) (mkQ [] extractVarNamesFromExpr) decl
getNameFromVarName (VarName _ name) = name

declNameQuery :: Decl Anno -> [Expr Anno]
declNameQuery decl = case decl of
                                (Decl _ _ ((expr, _, _):_) _) -> [expr] --everything (++) (mkQ [] extractVarNamesFromExpr) expr
                                _                             -> []

extractVarNamesFromExpr :: Expr Anno -> [VarName Anno]
extractVarNamesFromExpr expr = case expr of
                            Var _ _ varnameList -> map (\(varname, _) -> varname) varnameList
                            _ -> []

buildAstSeq :: (a -> a -> a) -> a -> [a] -> a
buildAstSeq _ nullNode [] = nullNode
buildAstSeq _ _ (statement:[]) = statement
buildAstSeq constructor nullNode (statement:statements) = constructor statement (buildAstSeq constructor nullNode statements)

readIndex :: String -> Int
readIndex = (round . read)

data Array = Array {
    varName       :: VarName Anno,
    arrDimensions :: Int
} deriving Show

getArrayReadsQuery :: [Array] -> Fortran Anno -> [Expr Anno]
getArrayReadsQuery arrays fortran = allReadExprs
    where
        arrayNames = map (\array -> let (VarName _ name) = (varName array) in name) arrays
        arrayReadQuery expr = case expr of
                                var@(Var _ _ ((VarName _ name, (idx:_)):_)) ->
                                    if (name `elem` arrayNames) then [var] else []
                                _                            -> []
        allReadExprs = everything (++) (mkQ [] arrayReadQuery) readExprsFromFortran
        readExprsFromFortran = case fortran of
                        (Assg _ _ _ rhs) -> [rhs]
                        (For _ _ _ start bound incre body) -> (start:bound:incre:(recursiveCall body))
                        (DoWhile _ _ bound body) -> [bound] ++ recursiveCall body
                        (FSeq _ _ fst snd) -> recursiveCall fst ++ recursiveCall snd
                        (If _ _ cond branch elseIfs elseBranch) ->
                            [cond] ++ recursiveCall branch ++ elseBranchResult
                            ++ branchConds ++ concatMap recursiveCall branchBodys
                            where
                                (branchConds, branchBodys) = unzip elseIfs
                                elseBranchResult = case elseBranch of
                                    (Just body) -> recursiveCall body
                                    _           -> []
                        (NullStmt _ _) -> []
                        -- (OpenCLStencil _ _ _ body) -> recursiveCall body
                        -- (OpenCLMap _ _ _ _ _ _ body) -> recursiveCall body
                        -- (OpenCLReduce _ _ _ _ _ _ _ body) -> recursiveCall body
                        missing@_ -> [] --error ("Unimplemented Fortran Statement " ++ miniPPF missing)
        recursiveCall = getArrayReadsQuery arrays

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

getArrayDecls :: ProgUnit Anno -> [Decl Anno]
getArrayDecls progUnit = arrayDecls
    where
        arrayDecls = filter (isArrayDecl) $ getDecls progUnit

getDeclType :: Decl Anno -> Type Anno
getDeclType (Decl _ _ _ typeDecl) = typeDecl

isArrayDecl :: Decl Anno -> Bool
isArrayDecl decl = (not . null . getArrayDimensions . getDeclType) decl
