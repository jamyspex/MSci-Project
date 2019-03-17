{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module ScalarizeKernels where

import           AddMemoryAccessKernels
import           Data.Generics
import           Data.List
import           Data.List.Utils
import qualified Data.Map               as DMap
import qualified Data.Set               as Set
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Pipeline
import           SmartCacheCodeGen
import           Utils

scalarizeKernels :: [PipelineStage] -> IO [PipelineStage]
scalarizeKernels =
  mapM (\(kern, sc, memAccess) -> scalarizeKernel (kern, sc, memAccess))

scalarizeKernel :: PipelineStage -> IO PipelineStage
scalarizeKernel (m@Map {..}, sc, ma) = do
  putStrLn $
    "Kernel = \n" ++
    miniPPProgUnit fortran ++
    "\nStreams = \n" ++ miniPPProgUnit withArrayArgsDeleted
  return (m {fortran = withArrayArgsDeleted}, sc, ma)
  where
    arrays = map arrayFromDecl $ getArrayDecls fortran
    availableStreams = inputStreams ++ outputStreams
    body = getSubBody fortran
    withArraysAccessesConverted =
      convertArrayAccesses arrays availableStreams fortran
    replacementMap = matchStreamingVarsToArrayDecls availableStreams arrays
    withArrayDeclarationsReplaced =
      replaceArrayDeclarations replacementMap withArraysAccessesConverted
    withArrayArgsDeleted =
      deleteArrayArgs availableStreams withArrayDeclarationsReplaced

deleteArrayArgs :: [Stream Anno] -> ProgUnit Anno -> ProgUnit Anno
deleteArrayArgs streams kernel =
  Sub
    subAnno
    subSrcSpan
    returnType
    name
    (Arg nullAnno newArgsNode nullSrcSpan)
    block
  where
    allArgs = getArgs kernel
    replacedByStream =
      Set.fromList $ map (\(Stream _ arrayName _ _) -> arrayName) streams
    arrayArgsRemoved =
      filter
        (\(ArgName _ name) -> name `Set.notMember` replacedByStream)
        allArgs
    newArgsNode =
      buildAstSeq (ASeq nullAnno) (NullArg nullAnno) arrayArgsRemoved
    (Sub subAnno subSrcSpan returnType name _ block) = kernel

replaceArrayDeclarations ::
     DMap.Map String (Array, [Stream Anno]) -> ProgUnit Anno -> ProgUnit Anno
replaceArrayDeclarations replacementMap kernel = newProgUnit
  where
    allDecls = getDecls kernel
    arrayDeclRemoved =
      map (replaceArrayDeclWithStreamingVarDecls replacementMap) allDecls
    (Sub subAnno subSrcSpan returnType name args block) = kernel
    (Block blockAnno uses implicit blockSrcSpan decls body) = block
    newDecls =
      concatMap
        (\decl ->
           if isArrayDecl decl
             then replaceArrayDeclWithStreamingVarDecls replacementMap decl
             else [decl])
        allDecls
    newDeclNode =
      buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) newDecls
    newBlock = Block blockAnno uses implicit blockSrcSpan newDeclNode body
    newProgUnit = Sub subAnno subSrcSpan returnType name args newBlock

matchStreamingVarsToArrayDecls ::
     [Stream Anno] -> [Array] -> DMap.Map String (Array, [Stream Anno])
matchStreamingVarsToArrayDecls streams arrays = matched
  where
    arrayNameToStreamMap =
      DMap.fromList $
      map (\grp -> ((fst . head) grp, map snd grp)) groupedByArray
    groupedByArray =
      groupBy (\(n1, _) (n2, _) -> n1 == n2) $
      sortBy (\(n1, _) (n2, _) -> n1 `compare` n2) $
      map (\s@(Stream _ arrayName _ _) -> (arrayName, s)) streams
    arrayNameToArrayMap =
      DMap.fromList $
      map (\arr -> ((getNameFromVarName . arrayVarName) arr, arr)) arrays
    matched =
      DMap.intersectionWith
        (\arr stream -> (arr, stream))
        arrayNameToArrayMap
        arrayNameToStreamMap

replaceArrayDeclWithStreamingVarDecls ::
     DMap.Map String (Array, [Stream Anno]) -> Decl Anno -> [Decl Anno]
replaceArrayDeclWithStreamingVarDecls replacementMap currentDecl =
  map makeDecl $ uniq $ map (\(Stream name _ _ _) -> name) streams
  where
    (array, streams) = replacementMap DMap.! declName
    (Decl _ _ _ fortranType) = currentDecl
    (baseType, attrs, kind, len) =
      ( getBaseType fortranType
      , getAttrs fortranType
      , getKind fortranType
      , getLen fortranType)
    attrsWithDimensionsRemove =
      filter
        (\case
           (Dimension _ _) -> False
           (Intent _ _) -> False
           _ -> True)
        attrs
    updatedType = BaseType nullAnno baseType attrsWithDimensionsRemove kind len
    makeDecl name =
      Decl
        nullAnno
        nullSrcSpan
        [(var name, NullExpr nullAnno nullSrcSpan, Nothing)]
        updatedType
    declName = declNameAsString currentDecl

-- convert expressions using arrays to use streaming variables
convertArrayAccesses ::
     [Array] -> [Stream Anno] -> ProgUnit Anno -> ProgUnit Anno
convertArrayAccesses arrays streams kernel = allArrayAccessExprsReplaced
  where
    allArrayAccessExprsReplaced =
      everywhere (mkT (subsitutionStreamVars arrays streams)) kernel

subsitutionStreamVars :: [Array] -> [Stream Anno] -> Expr Anno -> Expr Anno
subsitutionStreamVars arrays streams expr
  | isArrayAccess arrays expr =
    var $ convertArrayAccessToStreamVarName arrays expr
  | otherwise = expr

-- check if expression is an array access
isArrayAccess :: [Array] -> Expr Anno -> Bool
isArrayAccess arrays expr =
  (not . null) $ everything (++) (mkQ [] (arrayExprQuery arrays)) expr

-- convert a(j, k) to a_j_k or b(j-1, k+1) to b_jm1_kp1
convertArrayAccessToStreamVarName :: [Array] -> Expr Anno -> String
convertArrayAccessToStreamVarName arrays expr
  | isArrayAccess arrays expr = concat $ arrayName : map convertIndex indices
  | otherwise = error ("Not an array access: " ++ miniPP expr)
  where
    (Var _ _ ((VarName _ arrayName, indices):_)) = expr
    indiceSuffixs = map convertIndex indices
    convertIndex :: Expr Anno -> String
    convertIndex (Var _ _ ((name, _):_)) = "_" ++ getNameFromVarName name
    convertIndex (Bin _ _ (Plus _) loopVar (Con _ _ val)) =
      "_" ++ (getNameFromVarName . getVarName) loopVar ++ "p" ++ val
    convertIndex (Bin _ _ (Minus _) loopVar (Con _ _ val)) =
      "_" ++ (getNameFromVarName . getVarName) loopVar ++ "m" ++ val
