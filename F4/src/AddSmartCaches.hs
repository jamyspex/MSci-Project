{-# Language RecordWildCards #-}

module AddSmartCaches where

import           StencilBufferSizeDetection
import qualified Data.Map                      as DMap
import           Data.Maybe
import           Data.Tuple
import           Utils
import           Data.List.Index
import           Data.Ix
import           Data.List
import           Debug.Trace
import qualified KernelExtraction              as K
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Pipeline
import           Text.Printf

-- data SmartCacheInfo = SCI {
--     stencil :: Stencil,
--
--                           }

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
insertSmartCaches :: [K.Kernel] -> IO ()
insertSmartCaches = mapM_ processOneKernel

-- THE PLAN :
-- 1) Assume all the arrays are square and of equal side
-- 2) Work out the most disparate StencilIndices
-- 3) Find the index of the indices that differ e.g. (0, -1) and (0, 1) the index that differs is 1
-- 4) Use the array dimensions of that index to work out the size of the smart cache needed
-- THOUGHTS
-- * The size of the smart cache is = (the difference in the most disparate indices * the dimensions that different?
-- + any difference in other index)
-- * For arrays of different sizes I think the loop the smart cache needs to do is just the number
-- of elements in the array + the size of the smartcache
-- * Does it make a difference which direction the stride of the stencil is? no dont think so

processOneKernel :: K.Kernel -> IO ()
processOneKernel k = do
  putStrLn "--------------------------------\n"
  print k
  mapM_
    (\s -> do
      let (scSize, reach) =
            calculateStencilSizeAndEndPoints (defaultIterationOrder 2) s
      printf "%s : stencilReach = %s smartCacheSize = %d\n"
             (getStreamName s)
             (show reach)
             scSize
      getSmartCacheOutputVars s k
    )
    requiredStencilStreams
  putStrLn "================================\n"
  return ()
  where requiredStencilStreams = filter isStencil $ K.inputStreams k


getStreamName (K.Stream name _ _         ) = name
getStreamName (K.StencilStream name _ _ _) = name

-- getKernelsRequiringSmartCaches :: K.Kernel -> Bool
-- getKernelsRequiringSmartCaches kern = numberOfStenStreams > 0
--     where
--         isStencilStream = concatMap isStencil $ K.inputStreams kern
--         numberOfStenStreams = length isStencilStream


isStencil stream = case stream of
  K.Stream{}        -> False
  K.StencilStream{} -> True

-- Used to workout the size of the smart cache used to buffer a stream
getSmartCacheSize :: K.Stream Anno -> Int
getSmartCacheSize stenStream@(K.StencilStream name _ arrayDimens stencil) =
  fst (calculateStencilSizeAndEndPoints (defaultIterationOrder 2) stenStream)
 -- sizeInDim * stencilReach
 -- where
 --  (stenIdx1, stenIdx2) = getLargestStencilReach stenStream
 --  (Stencil _ stencilDimens _ stencilIndices _) = stencil
 --  differentIndex = findLargestDifferentIndex stenIdx1 stenIdx2
 --  stencilReach =
 --    abs (stenIdx1 !! differentIndex) + abs (stenIdx2 !! differentIndex)
 --  (lwb, upb) = arrayDimens !! differentIndex
 --  -- TODO need to do something here to account for stencils that go up
 --  -- the column e.g.
 --  --       X              X
 --  --    X  X  X works but X  X  X doesn't as you need 2 extra smache spaces
 --  --       X                    X
 --  sizeInDim  = upb - lwb


-- returns the stencil index that differs the most
-- between the argument indices
findLargestDifferentIndex :: [Int] -> [Int] -> Int
findLargestDifferentIndex i1 i2 = go (i1, i2) 0 0 0
 where
  go (i1 : i1s, i2 : i2s) idx largestSoFar largestIdxSoFar =
    if thisDifference > largestSoFar
      then go (i1s, i2s) (idx + 1) thisDifference idx
      else go (i1s, i2s) (idx + 1) largestSoFar largestIdxSoFar
    where thisDifference = abs i1 + abs i2
  go ([], []) _ _ largestIdxSoFar = largestIdxSoFar

-- this function takes a stencilStream and constructs the follow
-- list of typles [((lower bound, upper bound), (most extreme pair of opposite
-- stencil points)) in the testData example this would be [((0, 400), (Offset -1, Offset 1)),
-- ((1, 500), (Offset -2, Offset 2))]
-- This function checks that we're only dealing with Offset stencilIndices then returns Ints
getLargestStencilReach :: K.Stream Anno -> ([Int], [Int])
getLargestStencilReach stream@(K.StencilStream _ _ arrayDimens stencil) =
  if valid
    then (map unwrapStenIdx stenIdx1, map unwrapStenIdx stenIdx2)
    else error
      (  "Stencil indices invalid: sameLength = "
      ++ show sameLength
      ++ " allOffsets = "
      ++ show allOffsets
      )
 where
  (Stencil _ _ _ stencilIndices _) = stencil
  valid                            = allOffsets && sameLength
  sameLength = all (\s -> length s == length arrayDimens) stencilIndices
  allOffsets                       = all isOffset $ concat stencilIndices
  isOffset i = case i of
    Offset _ -> True
    _        -> False
  (stenIdx1, stenIdx2) = getMostDistantStencilPoints stencilIndices
  unwrapStenIdx (Offset val) = val

-- find the most distant stencil points
getMostDistantStencilPoints
  :: [[StencilIndex]] -> ([StencilIndex], [StencilIndex])
getMostDistantStencilPoints input = head sorted
 where
  allPairs = [ (x, y) | x <- input, y <- input, x /= y ]
  sorted   = sortBy
    (\(p1s1, p1s2) (p2s1, p2s2) ->
      compare (calculateDistance p2s1 p2s2) (calculateDistance p1s1 p1s2)
    )
    allPairs

-- Calculate the distance between two stencil indices
calculateDistance :: [StencilIndex] -> [StencilIndex] -> Int
calculateDistance i1 i2 = distance
 where
  valid      = length i1 == length i2
  -- This is here to make it respect the stream direction so what dimension the streaming is preformed in
  -- e.g. along dimension 1 then dimension 2 then dimension 3 up to dimension N. A more concrete example
  -- would be in the case of a 2D array do you stream cols then rows or rows then cols?
  -- In an ideal world this would be chosen by the compiler after it works out the smallest
  -- overall stencil usage based on trying different possiblities but that seems like "Future Work" to me
  streamDir  = (0, 0) -- This means compare only the left most column of any coords
  coordPairs = map (\idx -> (i1 !! idx, i2 !! idx)) (range streamDir)
  distance =
    foldr (\(Offset v1, Offset v2) acc -> acc + (v1 - v2) ^ 2) 0 coordPairs

getSmartCacheOutputVars :: K.Stream Anno -> K.Kernel -> IO () --[String]
getSmartCacheOutputVars (K.StencilStream name _ dims sten) kern = do
  putStrLn
    (  "============\n"
    ++ name
    ++ "\n"
    ++ concatMap (\l -> show l ++ "\n") loopVarPos
    ++ show (getOutputVariableNames loopVarPos sten)
    ++ "\n--------\n"
    )
  return ()
  where loopVarPos = getLoopVarPositions name kern

-- 1) Get the loop vars used to index an array
-- 2) Order them by the order they are used to access the array e.g. eta(j, k) -> [j, k]
-- 3) Using the Stencil used to access that array generate the output variables 
-- a smart cache buffering that array needs to emit
getOutputVariableNames :: [(String, Maybe Int)] -> Stencil Anno -> [String]
getOutputVariableNames loopVarsAndPosition (Stencil _ _ _ stenIndices (VarName _ name))
  = map snd outputVariables
 where
  loopVarPosMap =
    DMap.fromList $ map (swap . (\(f, s) -> (f, fromJust s))) $ filter
      (isJust . snd)
      loopVarsAndPosition
  outputVariables = map (foldl buildVarName (0, name)) stenIndices
  buildVarName :: (Int, String) -> StencilIndex -> (Int, String)
  buildVarName (pos, outVarName) cur = case cur of
    Offset _ ->
      ( pos + 1
      , outVarName ++ "_" ++ (loopVarPosMap DMap.! pos) ++ convertStenIdx cur
      )
    _ -> (pos + 1, outVarName ++ "_" ++ convertStenIdx cur)
  convertStenIdx (Offset val) | val == 0 = ""
                              | val < 0  = "m" ++ (show . abs) val
                              | val > 0  = "p" ++ show val
  convertStenIdx (Constant val) = show val

-- Get the position loopvars are used in the kernel body in a
-- specific stencil. At this point we have already check that 
-- loop var usage is consistent across arrays in a kernel thanks to
-- validateIndexingAndMakeUnique in AddKernelLoopGuards.hs
getLoopVarPositions :: String -> K.Kernel -> [(String, Maybe Int)]
getLoopVarPositions stencilArrayName K.Kernel {..} =
  map (\(_, loopV, mPos) -> (loopV, mPos))
    $ getLoopIndexPosition loopVars arrayAccess
 where
  stencilArray
    = filter
        (\arr -> let (VarName _ name) = varName arr in name == stencilArrayName
        )
      $ map arrayFromDecl
      $ getArrayDecls body
  arrayAccess = head $ getArrayReads stencilArray (getSubBody body)

