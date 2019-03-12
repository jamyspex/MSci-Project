{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module ScalarizeKernels where

import           AddMemoryAccessKernels
import           Data.Generics
import           Data.List
import qualified Data.Map               as DMap
import qualified Data.Set               as Set
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Pipeline
import           SmartCacheCodeGen
import           Utils

scalarizeKernels :: [PipelineStage] -> IO [PipelineStage]
scalarizeKernels pipeline = do
  putStrLn "hellow"
  mapM
    (\(kern, sc, memAccess) -> scalarizeKernel (kern, sc, memAccess))
    pipeline

scalarizeKernel :: PipelineStage -> IO PipelineStage
scalarizeKernel (m@Map {..}, sc, ma) = do
  putStrLn $
    "Kernel = \n" ++
    miniPPProgUnit fortran ++
    "\nStreams = \n" ++ miniPPProgUnit withArrayArgsDeleted
  return (m, sc, ma)
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
replaceArrayDeclarations replacementMap kernel =
  trace ("map size = " ++ show (DMap.size replacementMap)) newProgUnit
  where
    allDecls = getDecls kernel
    arrayDeclRemoved =
      map (replaceArrayDeclWithStreamingVarDecls replacementMap) $
      trace ("length allDecls = " ++ show (length allDecls)) allDecls
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
      buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) $
      trace ("length newDecls = " ++ show (length newDecls)) newDecls
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
      map (\s@(Stream _ arrayName _ _) -> (arrayName, s)) streams
    arrayNameToArrayMap =
      DMap.fromList $
      map (\arr -> ((getNameFromVarName . varName) arr, arr)) arrays
    matched =
      DMap.intersectionWith
        (\arr stream -> (arr, stream))
        arrayNameToArrayMap
        arrayNameToStreamMap

replaceArrayDeclWithStreamingVarDecls ::
     DMap.Map String (Array, [Stream Anno]) -> Decl Anno -> [Decl Anno]
replaceArrayDeclWithStreamingVarDecls replacementMap currentDecl =
  map (\(Stream name _ _ _) -> makeDecl name) streams
  where
    (_, streams) = replacementMap DMap.! declName
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
           _ -> True)
        attrs
    updatedType = BaseType nullAnno baseType attrsWithDimensionsRemove kind len
    makeDecl name =
      Decl
        nullAnno
        nullSrcSpan
        [(makeVar name, NullExpr nullAnno nullSrcSpan, Nothing)]
        updatedType
    declName = declNameAsString currentDecl

getBaseType (BaseType _ baseType _ _ _) = baseType
getBaseType (ArrayT _ _ baseType _ _ _) = baseType

getKind (BaseType _ _ _ kind _) = kind
getKind (ArrayT _ _ _ _ kind _) = kind

getLen (BaseType _ _ _ _ len) = len
getLen (ArrayT _ _ _ _ _ len) = len

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
    makeVar $ convertArrayAccessToStreamVarName arrays expr
  | otherwise = expr

makeVar name = Var nullAnno nullSrcSpan [(VarName nullAnno name, [])]

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
