{-# LANGUAGE RecordWildCards #-}
module LinkReductionVars where

import           Debug.Trace
import           Data.Generics
import qualified Data.Set                      as Set
import           Utils

-- Similar to adding transit streams but this time linking reduction 
-- vars between kernels producing them and the kernels using them 

linkReductionVars :: [Kernel] -> IO [Kernel]
linkReductionVars kernels = do
  mapM_ print withInputReduceVarsUpdated
  return withInputReduceVarsUpdated
 where
  allProducedReductionVars = concatMap outputReductionVars kernels
  withInputReduceVarsUpdated =
    addInputReductionVariables allProducedReductionVars kernels


addInputReductionVariables :: [String] -> [Kernel] -> [Kernel]
addInputReductionVariables allReductionVars kernels = snd
  $ foldl go (initialAvailableReduceVars, kernels) allReductionVars
 where
  initialAvailableReduceVars = Set.fromList allReductionVars
  go :: (Set.Set String, [Kernel]) -> String -> (Set.Set String, [Kernel])
  go (availableStreams, kernels) reductionVarName = foldl
    updateAllKernelsWithReduceVar
    (availableStreams, [])
    kernels
   where
    updateAllKernelsWithReduceVar
      :: (Set.Set String, [Kernel]) -> Kernel -> (Set.Set String, [Kernel])
    updateAllKernelsWithReduceVar (avail, updatedKernels) k = if not required
      then (avail, updatedKernels ++ [k])
      else
        ( updAvail
        , updatedKernels ++ [addOneInputReductionVar reductionVarName k]
        )
     where
      available = Set.member reductionVarName avail
      required  = usesVariable reductionVarName k
        && not (producesReductionVariable reductionVarName k)
      updAvail = Set.delete reductionVarName avail
    addOneInputReductionVar :: String -> Kernel -> Kernel
    addOneInputReductionVar reductionVar kernel = if reductionVarUsed
      then kernel
        { inputReductionVars = reductionVar : inputReductionVars kernel
        }
      else kernel
      where reductionVarUsed = usesVariable reductionVarName kernel

usesVariable name Kernel {..} = name `elem` getDeclNames body

producesReductionVariable name Kernel {..} =
  name `elem` everything (++) (mkQ [] getReductionVarNameQuery) body
