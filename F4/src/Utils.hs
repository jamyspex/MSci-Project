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
