{-# Language TupleSections #-}
module AddSynthesisedLoopVars where

import           Data.List
import           MiniPP
import           Debug.Trace
import           Data.Maybe
import           Language.Fortran
import qualified Data.Map                      as DMap
import           LanguageFortranTools
import           Utils
import           DetectDriverLoopSize

-- This module synthesises and adds the statements needed to calculate
-- the original loopVariables from the driver loop. This allows them
-- to be used in the previously added loop guards.

synthesiseLoopVars :: [Kernel] -> IO [Kernel]
synthesiseLoopVars kernels = do
  updatedKernels <- mapM addToOneKernel validatedKernels
  return updatedKernels
 where
  pipelineIsValid  = allStreamsSameSize kernels
  validatedKernels = if pipelineIsValid
    then kernels
    else error
      (  "Pipeline "
      ++ concatMap (\k -> kernelName k ++ " -> ") kernels
      ++ "contains streams of different sizes!"
      )


addToOneKernel :: Kernel -> IO Kernel
addToOneKernel kernel = do
  print $ "Kernel = " ++ kernelName kernel
  print $ getLoopVarToRange kernelCode
  return kernel
 where
  streamDimensions = (getStreamDimensions . head . outputs) kernel
  kernelCode       = body kernel

-- Get a list of all the loop variables and the dimensions of the arrays 
-- they are used to access then validate these are all the same for each variable
-- and if they are return one tuple for each variable.
-- The returned tuples have the following meaning:
--      (loop nesting level in original code 0 = least nested, 
--      loop variable name, 
--      range of array dimension the variable is used to iteration over)
getLoopVarToRange :: ProgUnit Anno -> [(Int, String, (Int, Int))]
getLoopVarToRange kernelCode = if valid
  then map
    ( (\(loopVarName, dims) ->
        (fromJust $ elemIndex loopVarName loopVars, loopVarName, dims)
      )
    . head
    )
    varDimListGrouped
  else error
    "loop variables used to access different arrays of different dimensions"
 where
  valid = all (\grp -> all (== head grp) grp) varDimListGrouped
  varDimListGrouped =
    groupBy (\(lv1, _) (lv2, _) -> lv1 == lv2) variableDimList
  variableDimList =
    sortBy (\(lv1, _) (lv2, _) -> lv1 `compare` lv2)
      $ concat
      $ DMap.elems
      $ DMap.intersectionWith buildLoopVarRangeItem declMap accessMap
  kernelBody       = getSubBody kernelCode
  loopVars         = map getNameFromVarName $ getLoopVars kernelBody
  allArrayAccesses = getAllArrayAccesses allArrays kernelBody
  allArrays        = map arrayFromDecl allArrayDecls
  allArrayDecls    = getArrayDecls kernelCode
  accessMap        = foldl
    (\map idxPosItem@(arrayName, _, _) -> DMap.adjust
      (\idxPosItemList -> idxPosItem : idxPosItemList)
      arrayName
      map
    )
    emptyAccessMap
    loopIndexPos
  emptyAccessMap = DMap.fromList $ map (, []) $ DMap.keys declMap
  declMap        = DMap.fromList
    $ map (\d -> ((getNameFromVarName . getVarNameG) d, d)) allArrayDecls
  loopIndexPos = concatMap (getLoopIndexPosition loopVars) allArrayAccesses
  buildLoopVarRangeItem
    :: Decl Anno -> [(String, String, Maybe Int)] -> [(String, (Int, Int))]
  buildLoopVarRangeItem decl = concatMap go
   where
    go :: (String, String, Maybe Int) -> [(String, (Int, Int))]
    go (_, _, Nothing) = []
    go (arrayName, loopVarName, Just position) =
      [(loopVarName, arrayDims !! position)]
      where arrayDims = getArrayDeclDimensions decl

allStreamsSameSize :: [Kernel] -> Bool
allStreamsSameSize kernels = valid
 where
  allStreams = concatMap inputs kernels ++ concatMap outputs kernels
  allSizes   = map getStreamDimensions allStreams
  valid      = all (== head allSizes) allSizes
