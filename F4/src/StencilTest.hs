module StencilTest where

import           Control.Exception
import           Text.Printf
import           Debug.Trace
import           Data.Ord
import           Data.List
import           Data.Ix
import           LanguageFortranTools
import           Language.Fortran
import qualified KernelExtraction              as K

defaultIterationOrder dims = range (0, dims - 1)

-- Helper method to print all the sizes of all the different
-- combination of stencil points in a stream 
printResults stream =
  putStrLn
    $ concatMap
        (\(scSize, (start, end)) -> printf
          "Smart cache size = %d start index = %s end index = %s\n"
          scSize
          (show start)
          (show end)
        )
    $ sortStencils
    $ calculateStencilSizeAndEndPoints (defaultIterationOrder 3) stream

-- Sorts the results from calculateStencilSize by number of block
-- and then by the number of 0s in the indices. If multiple potential
-- end pairs require the same number of blocks to buffer the stencil 
-- prefer the one with more zeros in the indices as this will include 
-- all the other potential pairs of the same size.
sortStencils = sortBy
  (\(scSize1, (start1, end1)) (scSize2, (start2, end2)) ->
    scSize2
      `compare` scSize1
      <> (count (/= 0) (start1 ++ end1) `compare` count (/= 0) (start2 ++ end2))
  )

count pred = length . filter pred

scSizeOnly sten =
  let results = calculateStencilSizeAndEndPoints (defaultIterationOrder 3) sten
      (area, indices) = maximumBy (comparing fst) results
  in  area


-- This method is used to calculate the size of smart cache required to 
-- buffer a stencil stream in order to produce an output stream from each of its points.
-- It also returns the end points of the stencil used to calculate that size.
-- It is designed to work for arrays of any number of dimensions.
-- It works by using the iteration order list to determine the "significance" of
-- the streams dimensions. The diagram below shows how using (defaultIterationOrder 3) 
-- treats the indices of a 3D stream. 
--
--                Dim 2
--      -------------------------
--      | \ 
--      |  \
--      |   \
--   D  |    \
--   i  |     \  D
--   m  |      \  i
--      |       \  m
--   1  |        \
--      |         \  3
--      |          \
--      |           \
--      |            \
--
-- The function works by working out the distance between all the possible pairs of 
-- stencil indices and then selecting the best using the sortStencils function.  
-- To calculate the size of the smart cache required if buffer between to specific
-- points A(0, -2, -1) and B(0, 2, 1) the function starts with the most significant 
-- indices e.g. -1 & 1 (when using defaultIterationOrder 3) and calculates the 
-- differences between them. This is then used along with the stream dimensions to 
-- calculate the size of smart cache required. The function then considers the next most
-- significant index in this case -2 and 2 and repeats the process 
calculateStencilSizeAndEndPoints
  :: [Int] -> K.Stream Anno -> [(Int, ([Int], [Int]))]
calculateStencilSizeAndEndPoints iterationOrder (K.StencilStream _ _ arrayDimens stencil)
  = stencilSizesAndIndexPairs
 where
  (Stencil _ stencilDimens _ stencilIndices _) = stencil
  stencilIndicesInts                           = stripStenIndex stencilIndices
  allIndexPairs =
    [ (x, y) | x <- stencilIndicesInts, y <- stencilIndicesInts, x /= y ]
  smallIndexFirstOnly =
    filter (\(x, y) -> compareIndices x y iterationOrder == LT) allIndexPairs
  stencilSizesAndIndexPairs = map go smallIndexFirstOnly
  go (l1, l2) =
    let initial = ((l1, l2), True, 0)
        ((ol1, ol2), _, totArea) =
          foldl combineReaches initial (reverse iterationOrder)
        offset = head ol1 - head ol2
    in  (abs totArea + 1, (ol1, ol2))
-- last move index is the most significant therefore its impossible for it to
-- be subsumed by another index value so skip the check to see if it is > 0 when 
-- adding its buffer contribution 
  combineReaches
    :: (([Int], [Int]), Bool, Int) -- ((indice components), first iteration, area)
    -> Int                         -- iteration order  
    -> (([Int], [Int]), Bool, Int) -- ((indice components), False, total area)
  combineReaches ((idx1, idx2), firstIter, areaSoFar) component =
    if firstIter || i1 < 0 || i2 > 0
      then
        ((idx1, idx2), False, areaSoFar + calculateReach component (idx1, idx2))
      else ((idx1, idx2), False, areaSoFar)
   where
    i1 = idx1 !! component
    i2 = idx2 !! component
  calculateReach :: Int -> ([Int], [Int]) -> Int
  calculateReach pos (ind1, ind2) = numBlocks
   where
    numBlocks   = indexDiff * totalBlocks
    indexDiff   = ind2CurComp - ind1CurComp
    ind1CurComp = ind1 !! pos
    ind2CurComp = ind2 !! pos
    totalBlocks = foldl dimensionProductFold 1 (take pos iterationOrder)
    dimensionProductFold blocks cur = ((upb - lwb) + 1) * blocks
      where (lwb, upb) = arrayDimens !! cur

stripStenIndex = map (map (\(Offset v) -> v))

-- elementwise comparison of indices based on their 
-- significance as indicated by iterationOrder
compareIndices :: [Int] -> [Int] -> [Int] -> Ordering
compareIndices i1 i2 iterationOrder = if sameLength
  then orderExpr
  else error "indices of different lengths"
 where
  sameLength = length i1 == length i2
  orderExpr  = foldl (\acc cur -> acc <> ((i1 !! cur) `compare` (i2 !! cur)))
                     EQ
                     iterationOrder

-- test method

test stream@(K.StencilStream _ _ _ stencil) numBlocksShouldBe startShouldBeIdx endShouldBeIdx
  = numBlocksShouldBe
    == numBlocksIs
    && startShouldBe
    == startIs
    && endShouldBe
    == endIs
 where
  (Stencil _ _ _ stencilIndices _) = stencil
  stencilIndicesInts               = stripStenIndex stencilIndices
  startShouldBe                    = stencilIndicesInts !! startShouldBeIdx
  endShouldBe                      = stencilIndicesInts !! endShouldBeIdx
  (numBlocksIs, (startIs, endIs)) =
    head $ sortStencils $ calculateStencilSizeAndEndPoints
      (defaultIterationOrder 3)
      stream

assertions = assert
  (  test crossTestData3D_8x8x8                            129 2 6
  && test crossTestData3D_10x6x8                           121 2 6
  && test crossWithExtraPointsToBeIgnoredTestData3D_10x6x8 121 2 7
  && test crossWithExtraPointsNotIgnoredTestData3D_10x6x8  141 3 8
  && test nonSymetricTestData3D_10x6x8                     119 6 2
  && test nonSymetricLarger_10x6x8                         123 2 6
  && test nonSymetricTestDataExtremities_10x6x8            119 1 0
  && test extremesCrossTestData3D_10x6x8                   121 0 1
  && test testData3Darray1D_10x6x8                         3   1 0
  && test testData3Darray2D_10x6x8                         21  1 0
  )
  "Assertions passed"

-- Test data

crossTestData3D_8x8x8 = K.StencilStream
  "test"
  K.Float
  [(1, 8), (1, 8), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset 0, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset 0, Offset 0, Offset (-1)]
    , [Offset 0, Offset 0, Offset 0]
    , [Offset 1, Offset 0, Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    , [Offset 0, Offset 0, Offset 1]
    ]
    (VarName nullAnno "test")
  )


crossTestData3D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset 0, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset 0, Offset 0, Offset (-1)]
    , [Offset 0, Offset 0, Offset 0]
    , [Offset 1, Offset 0, Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    , [Offset 0, Offset 0, Offset 1]
    ]
    (VarName nullAnno "test")
  )


extremesCrossTestData3D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil nullAnno
           3
           2
           [[Offset 0, Offset 0, Offset (-1)], [Offset 0, Offset 0, Offset 1]]
           (VarName nullAnno "test")
  )

crossWithExtraPointsToBeIgnoredTestData3D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset 0, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset 0, Offset 0, Offset (-1)]
    , [Offset 0, Offset 1, Offset (-1)]
    , [Offset 0, Offset 0, Offset 0]
    , [Offset 1, Offset 0, Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    , [Offset 0, Offset 0, Offset 1]
    , [Offset 0, Offset (-1), Offset 1]
    ]
    (VarName nullAnno "test")
  )


crossWithExtraPointsNotIgnoredTestData3D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset 0, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset 0, Offset 0, Offset (-1)]
    , [Offset 0, Offset (-1), Offset (-1)]
    , [Offset 0, Offset 0, Offset 0]
    , [Offset 1, Offset 0, Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    , [Offset 0, Offset 0, Offset 1]
    , [Offset 0, Offset 1, Offset 1]
    ]
    (VarName nullAnno "test")
  )


extremitiesOfCrossNotIgnore_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [[Offset 0, Offset (-1), Offset (-1)], [Offset 0, Offset 1, Offset 1]]
    (VarName nullAnno "test")
  )

nonSymetricTestData3D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset 0, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset 1, Offset 0, Offset (-1)]
    , [Offset 0, Offset 0, Offset 0]
    , [Offset 1, Offset 0, Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    , [Offset (-1), Offset 0, Offset 1]
    ]
    (VarName nullAnno "test")
  )

testData3Darray1D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil nullAnno
           3
           2
           [[Offset 1, Offset 0, Offset 0], [Offset (-1), Offset 0, Offset 0]]
           (VarName nullAnno "test")
  )

testData3Darray2D_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil nullAnno
           3
           2
           [[Offset 0, Offset 1, Offset 0], [Offset 0, Offset (-1), Offset 0]]
           (VarName nullAnno "test")
  )

nonSymetricLarger_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset 0, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset (-1), Offset 0, Offset (-1)]
    , [Offset 0, Offset 0, Offset 0]
    , [Offset 1, Offset 0, Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    , [Offset 1, Offset 0, Offset 1]
    ]
    (VarName nullAnno "test")
  )

nonSymetricTestDataExtremities_10x6x8 = K.StencilStream
  "test"
  K.Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [[Offset 1, Offset 0, Offset (-1)], [Offset (-1), Offset 0, Offset 1]]
    (VarName nullAnno "test")
  )
