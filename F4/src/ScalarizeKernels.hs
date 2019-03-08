{-# LANGUAGE RecordWildCards #-}

module ScalarizeKernels where

import           AddMemoryAccessKernels
import           Data.Generics
import qualified Data.Map               as DMap
import qualified Data.Set               as Set
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Pipeline
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
    "\nStreaming variables inserted = \n" ++ miniPPF withArraysAccessesConverted
  return (m, sc, ma)
  where
    arrays = map arrayFromDecl $ getArrayDecls fortran
    availableStreams = inputStreams ++ outputStreams
    body = getSubBody fortran
    withArraysAccessesConverted =
      convertArrayAccesses arrays availableStreams body

convertArrayAccesses :: [Array] -> [Stream Anno] -> Fortran Anno -> Fortran Anno
convertArrayAccesses arrays streams body = allArrayAccessExprsReplaced
  where
    allArrayAccessExprsReplaced =
      everywhere (mkT (subsitutionStreamVars arrays streams)) body

subsitutionStreamVars :: [Array] -> [Stream Anno] -> Expr Anno -> Expr Anno
subsitutionStreamVars arrays streams expr
  | isArrayAccess arrays expr =
    makeVar $ convertArrayAccessToStreamVarName arrays streams expr
  | otherwise = expr

makeVar name = Var nullAnno nullSrcSpan [(VarName nullAnno name, [])]

isArrayAccess :: [Array] -> Expr Anno -> Bool
isArrayAccess arrays expr =
  (not . null) $ everything (++) (mkQ [] (arrayExprQuery arrays)) expr

convertArrayAccessToStreamVarName ::
     [Array] -> [Stream Anno] -> Expr Anno -> String
convertArrayAccessToStreamVarName arrays availableStreams expr =
  if valid
    then requiredStreamName
    else error ("Stream " ++ requiredStreamName ++ " not available.")
  where
    (Var _ _ ((VarName _ arrayName, indices):_)) = expr
    availableStreamNames = map getStreamName availableStreams
    valid = requiredStreamName `elem` availableStreamNames
    requiredStreamName = concat $ arrayName : map convertIndex indices
    convertIndex :: Expr Anno -> String
    convertIndex (Var _ _ ((name, _):_)) = "_" ++ getNameFromVarName name
    convertIndex (Bin _ _ (Plus _) loopVar (Con _ _ val)) =
      "_" ++ (getNameFromVarName . getVarName) loopVar ++ "p" ++ val
    convertIndex (Bin _ _ (Minus _) loopVar (Con _ _ val)) =
      "_" ++ (getNameFromVarName . getVarName) loopVar ++ "m" ++ val
