{-# Language RecordWildCards, TupleSections #-}

module ScalarizeKernels where

import           Data.Generics
import           LanguageFortranTools
import           Language.Fortran
import           AddMemoryAccessKernels
import           Utils
import           MiniPP
import           Pipeline
import qualified Data.Map                      as DMap
import qualified Data.Set                      as Set


scalarizeKernels :: [PipelineStage] -> IO [PipelineStage]
scalarizeKernels pipeline = do
  putStrLn "hellow"
  mapM (\(kern, sc, memAccess) -> scalarizeKernel (kern, sc, memAccess))
       pipeline


scalarizeKernel :: PipelineStage -> IO PipelineStage
scalarizeKernel (m@Map {..}, sc, ma) = do
  putStrLn
    $  "Kernel = \n"
    ++ miniPPProgUnit fortran
    ++ "\nStreams = \n"
    ++ concatMap (\s -> "\t" ++ s ++ "\n") withArraysAccessesConverted
  return (m, sc, ma)
 where
  arrays           = map arrayFromDecl $ getArrayDecls fortran
  availableStreams = inputStreams ++ outputStreams
  body             = getSubBody fortran
  withArraysAccessesConverted =
    convertArrayAccesses arrays availableStreams body

convertArrayAccesses :: [Array] -> [Stream Anno] -> Fortran Anno -> [String] -- Fortran Anno
convertArrayAccesses arrays streams body = allArraysAsStreams
 where
  allArrayAccessExprs = getAllArrayAccesses arrays body
  allArraysAsStreams =
    map (convertArrayAccessToStreamVarName arrays) allArrayAccessExprs

isArrayAccess :: [Array] -> Expr Anno -> Bool
isArrayAccess arrays expr =
  (not . null) $ everything (++) (mkQ [] (arrayExprQuery arrays)) expr

convertArrayAccessToStreamVarName :: [Array] -> Expr Anno -> String
convertArrayAccessToStreamVarName arrays expr
  | isArrayAccess arrays expr = concat $ arrayName : map convertIndex indices
  | otherwise                 = error ("Not an array access: " ++ miniPP expr)
 where
  (Var _ _ ((VarName _ arrayName, indices) : _)) = expr
  indiceSuffixs = map convertIndex indices
  convertIndex :: Expr Anno -> String
  convertIndex (Var _ _ ((name, _) : _)) = "_" ++ getNameFromVarName name
  convertIndex (Bin _ _ (Plus _) loopVar (Con _ _ val)) =
    "_" ++ (getNameFromVarName . getVarName) loopVar ++ "p" ++ val
  convertIndex (Bin _ _ (Minus _) loopVar (Con _ _ val)) =
    "_" ++ (getNameFromVarName . getVarName) loopVar ++ "m" ++ val





