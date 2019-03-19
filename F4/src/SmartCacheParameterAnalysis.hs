{-# LANGUAGE RecordWildCards, LambdaCase #-}

module SmartCacheParameterAnalysis where

import           Data.Maybe
import           Control.Exception
import           Data.Ix
import           Safe
import           Data.List
import           Data.Ord
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           Text.Printf
import           Utils

-- default array ordering
-- down columns -> along rows -> back to front
defaultIterationOrder dims = range (0, dims - 1)

-- Helper method to print all the sizes of all the different
-- combination of stencil points in a stream
printResults stream =
  putStrLn
    $ concatMap
        (\(scSize, (start, end)) -> printf
          "Smart cache size = %d start index = %s end index = %s offset = %d\n"
          scSize
          (show start)
          (show end)
        )
    $ sortStencils
    $ calculateSmartCacheSizeForAllPairsOfStencilPoints
        (defaultIterationOrder 3)
        stream

printSmartCacheDetailsForStream stream =
  print $ calculateSmartCacheDetailsForStream Nothing
                                              (defaultIterationOrder 3)
                                              stream

-- Sorts the results from calculateSmartCacheSizeForAllPairsOfStencilPoints by number of block
-- and then by the number of 0s in the indices. If multiple potential
-- end pairs require the same number of blocks to buffer the stencil
-- prefer the one with more zeros in the indices as this will include
-- all the other potential pairs of the same size.
sortStencils :: [(Int, ([Int], [Int]))] -> [(Int, ([Int], [Int]))]
sortStencils = sortBy
  (\(scSize1, (start1, end1)) (scSize2, (start2, end2)) ->
    scSize2
      `compare` scSize1
      <> (count (/= 0) (start1 ++ end1) `compare` count (/= 0) (start2 ++ end2))
  )

count pred = length . filter pred

scSizeOnly sten =
  let SmartCacheDetailsForStream {..} = calculateSmartCacheDetailsForStream
        Nothing
        (defaultIterationOrder 3)
        sten
  in  requiredBufferSize

hasConstantStenValues :: Stream Anno -> Bool
hasConstantStenValues (StencilStream _ _ _ _ (Stencil _ _ _ stencilIndices _))
  = or $ concatMap
    (map
      (\case
        Constant _ -> True
        Offset   _ -> False
      )
    )
    stencilIndices


-- to work out the maxPosOffset and maxNegOffset fields find the largest reach from
-- the central point backwards and central point forwards. If the central point (0, 0, 0, ...)
-- is not present already in the stream add it.
calculateSmartCacheDetailsForStream
  :: Maybe Kernel -> [Int] -> Stream Anno -> SmartCacheDetailsForStream
calculateSmartCacheDetailsForStream kernel itOrder sten
  | hasConstantStenValues sten = DummySmartCacheDetailsForStream {stream = sten}
  | otherwise = SmartCacheDetailsForStream
    { requiredBufferSize    = maxNumBlocks
    , startIndex            = maxStart
    , endIndex              = maxEnd
    , startToPointDistances = pairsFromStart
    , maxPosOffset          = getMaxOffset all fst centralIdx
    , maxNegOffset          = getMaxOffset all snd centralIdx
    }
 where
  (StencilStream name arrayName valueType dims (Stencil anno dimension numPoints coords varName))
    = sten
  centralIdx             = replicate dimension (Offset 0)
  centralIdxStriped      = map (\(Offset val) -> val) centralIdx
  centralPointNeedsAdded = centralIdx `notElem` coords
  updatedCoords =
    if centralPointNeedsAdded then centralIdx : coords else coords
  toProcess = StencilStream
    name
    arrayName
    valueType
    dims
    (Stencil anno dimension (length updatedCoords) updatedCoords varName)
  all = calculateSmartCacheSizeForAllPairsOfStencilPoints itOrder toProcess
  (maxNumBlocks, (maxStart, maxEnd)) =
    ( headNote
          (  "SmartCacheParameterAnalysis: line 89 : stream name = "
          ++ getStreamName sten
          ++ "\nkernel = \n"
          ++ show (fromJust kernel)
          )
      . sortStencils
      )
      all
  pairsFromStart =
    map (\(size, (_, point)) -> (point, size))
      $ filter
          (\(_, (start, end)) ->
            not centralPointNeedsAdded
              || (start /= centralIdxStriped && end /= centralIdxStriped)
          )
      $ filter (\(_, (start, _)) -> start == maxStart) all

getMaxOffset
  :: [(Int, ([Int], [Int]))]
  -> (([Int], [Int]) -> [Int])
  -> [StencilIndex]
  -> Int
getMaxOffset all select centralIdx = if (not . null) filtered
  then
    let (offset, _) =
          (headNote "SmartCacheParameterAnalysis: line 103" . sortStencils)
            filtered
    in  offset
  else 0
 where
  stripStencilIndex = map (\(Offset val) -> val)
  filtered =
    filter (\(_, coords) -> select coords == stripStencilIndex centralIdx) all

-- This method is used to calculate the size of smart cache required to
-- buffer a stencil stream in order to produce an output stream from each of its points.
-- It also returns the end points of the stencil used to calculate that size.
-- It is designed to work for arrays of any number of dimensions.
-- It works by using the iteration order list to determine the "significance" of
-- the streams dimensions. The diagram below shows how using (defaultIterationOrder 3)
-- treats the indices of a 3D stream.
--
--                Dim 2
--      ------------------------>
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
--      v            \
--            (increasing this direction)
--
-- The function works by working out the distance between all the possible pairs of
-- stencil indices and then selecting the best using the sortStencils function.
-- To calculate the size of the smart cache required if buffer between to specific
-- points A(0, -2, -1) and B(0, 2, 1) the function starts with the most significant
-- indices e.g. -1 & 1 (when using defaultIterationOrder 3) and calculates the
-- differences between them. This is then used along with the stream dimensions to
-- calculate the size of smart cache required. The function then considers the next most
-- significant index in this case -2 and 2 and repeats the process
calculateSmartCacheSizeForAllPairsOfStencilPoints
  :: [Int] -> Stream Anno -> [(Int, ([Int], [Int]))]
calculateSmartCacheSizeForAllPairsOfStencilPoints iterationOrder (StencilStream _ _ _ arrayDimens stencil)
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
        offset =
          headNote "SmartCacheParameterAnalysis: line 164" ol1
            - headNote "SmartCacheParameterAnalysis: line 165" ol2
    in  (abs ((totArea + 1) - max 0 offset), (ol1, ol2))
  combineReaches
    :: (([Int], [Int]), Bool, Int) -- ((index components), first iteration, area)
    -> Int -- axes
    -> (([Int], [Int]), Bool, Int) -- ((index components), False, total area)
  combineReaches ((idx1, idx2), firstIter, areaSoFar) component =
    if firstIter || i1 < 0 || i2 > 0
      then
        ((idx1, idx2), False, areaSoFar + calculateReach component (idx1, idx2))
      else ((idx1, idx2), False, areaSoFar)
   where
    i1 = trace "line 204" idx1 !! component
    i2 = trace "line 205" idx2 !! component
  calculateReach :: Int -> ([Int], [Int]) -> Int
  calculateReach pos (ind1, ind2) = numBlocks
   where
    numBlocks   = indexDiff * totalBlocks
    indexDiff   = ind2CurComp - ind1CurComp
    ind1CurComp = trace "line 211" ind1 !! pos
    ind2CurComp = trace "line 212" ind2 !! pos
    totalBlocks = foldl dimensionProductFold 1 (take pos iterationOrder)
    dimensionProductFold blocks cur = ((upb - lwb) + 1) * blocks
      where (lwb, upb) = trace "line 215" arrayDimens !! cur

-- fold over all the different array axes adding any buffer contributions from the difference
-- to the total buffer size. The function has to check whether the difference in lower order
-- axes is subsumed by another index value or not. In the first iteration there no higher order
-- index that could subsume the difference so skip the check to see if it is > 0
stripStenIndex :: [[StencilIndex]] -> [[Int]]
stripStenIndex = map (map stripOne)
 where
  stripOne v = case v of
    Offset val -> val
    _          -> 0

-- elementwise comparison of indices based on their
-- significance as indicated by iterationOrder
compareIndices :: [Int] -> [Int] -> [Int] -> Ordering
compareIndices i1 i2 iterationOrder = if sameLength
  then orderExpr
  else error "indices of different lengths"
 where
  sameLength = length i1 == length i2
  orderExpr  = trace
    (  "iteration order = "
    ++ show iterationOrder
    ++ " i1 = "
    ++ show i1
    ++ " i2 = "
    ++ show i2
    )
    (foldl
      (\acc cur -> trace
        (  "cur = "
        ++ show cur
        ++ " i1 !! cur = "
        ++ show (i1 !! cur)
        ++ " i2 !! cur = "
        ++ show (i2 !! cur)
        )
        (acc <> ((i1 !! cur) `compare` (i2 !! cur)))
      )
      EQ
      (reverse $ take (length i1) iterationOrder)
    )

-- test method, assertions and test data
test stream@(StencilStream _ _ _ _ stencil) numBlocksShouldBe startShouldBeIdx endShouldBeIdx maxPosOffsetShouldBe maxNegOffsetShouldBe
  = trace
      (  "num blocks: "
      ++ show numBlocksShouldBe
      ++ " == "
      ++ show requiredBufferSize
      ++ "\n"
      ++ "start index: "
      ++ show startShouldBe
      ++ " == "
      ++ show startIndex
      ++ "\n"
      ++ "end index: "
      ++ show endShouldBe
      ++ " == "
      ++ show endIndex
      ++ "\n"
      ++ "length stencilIndices: "
      ++ show (length stencilIndices)
      ++ " == "
      ++ show (length startToPointDistances + 1)
      ++ "\n"
      ++ "max neg offset: "
      ++ show maxNegOffsetShouldBe
      ++ " == "
      ++ show maxNegOffset
      ++ "\n"
      ++ "max pos offset: "
      ++ show maxPosOffsetShouldBe
      ++ " == "
      ++ show maxPosOffset
      ++ "\n"
      )
    $  numBlocksShouldBe
    == requiredBufferSize
    && startShouldBe
    == startIndex
    && endShouldBe
    == endIndex
    && length stencilIndices
    == (length startToPointDistances + 1)
    && maxNegOffsetShouldBe
    == maxNegOffset
    && maxPosOffsetShouldBe
    == maxPosOffset
 where
  (Stencil _ _ _ stencilIndices _) = stencil
  stencilIndicesInts               = stripStenIndex stencilIndices
  startShouldBe                    = stencilIndicesInts !! startShouldBeIdx
  endShouldBe                      = stencilIndicesInts !! endShouldBeIdx
  res@SmartCacheDetailsForStream {..} =
    calculateSmartCacheDetailsForStream Nothing (defaultIterationOrder 3) stream

assertions = assert
  (  test crossTestData3D_8x8x8                            129 2 6 65 65
  && test crossTestData3DZeroBasedIndex_8x8x8              163 2 6 82 82
  && test crossTestData3D_10x6x8                           121 2 6 61 61
  && test crossWithExtraPointsToBeIgnoredTestData3D_10x6x8 121 2 7 61 61
  && test crossWithExtraPointsNotIgnoredTestData3D_10x6x8  141 3 8 71 71
  && test nonSymetricTestData3D_10x6x8                     119 2 6 60 60
  && test nonSymetricLarger_10x6x8                         123 2 6 62 62
  && test nonSymetricTestDataExtremities_10x6x8            119 0 1 60 60
  && test extremesCrossTestData3D_10x6x8                   121 0 1 61 61
  && test testData3Darray1D_10x6x8                         3   1 0 2  2
  && test testData3Darray2D_10x6x8                         21  1 0 11 11
  && test nonSymetricalLargerThan1Offset                   35  0 1 18 18
  )
  "Assertions passed"

-- Test data
nonSymetricalLargerThan1Offset = StencilStream
  "test"
  "test"
  Float
  [(1, 8), (1, 8), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [ [Offset (-1), Offset (-2), Offset 0]
    , [Offset 1, Offset 2, Offset 0]
    , [Offset 0, Offset (-1), Offset 0]
    , [Offset 0, Offset 1, Offset 0]
    ]
    (VarName nullAnno "test")
  )

crossTestData3DZeroBasedIndex_8x8x8 = StencilStream
  "test"
  "test"
  Float
  [(0, 8), (0, 8), (0, 8)]
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

crossTestData3D_8x8x8 = StencilStream
  "test"
  "test"
  Float
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

crossTestData3D_10x6x8 = StencilStream
  "test"
  "test"
  Float
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

extremesCrossTestData3D_10x6x8 = StencilStream
  "test"
  "test"
  Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil nullAnno
           3
           2
           [[Offset 0, Offset 0, Offset (-1)], [Offset 0, Offset 0, Offset 1]]
           (VarName nullAnno "test")
  )

crossWithExtraPointsToBeIgnoredTestData3D_10x6x8 = StencilStream
  "test"
  "test"
  Float
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

crossWithExtraPointsNotIgnoredTestData3D_10x6x8 = StencilStream
  "test"
  "test"
  Float
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

extremitiesOfCrossNotIgnore_10x6x8 = StencilStream
  "test"
  "test"
  Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [[Offset 0, Offset (-1), Offset (-1)], [Offset 0, Offset 1, Offset 1]]
    (VarName nullAnno "test")
  )

nonSymetricTestData3D_10x6x8 = StencilStream
  "test"
  "test"
  Float
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

testData3Darray1D_10x6x8 = StencilStream
  "test"
  "test"
  Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil nullAnno
           3
           2
           [[Offset 1, Offset 0, Offset 0], [Offset (-1), Offset 0, Offset 0]]
           (VarName nullAnno "test")
  )

testData3Darray2D_10x6x8 = StencilStream
  "test"
  "test"
  Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil nullAnno
           3
           2
           [[Offset 0, Offset 1, Offset 0], [Offset 0, Offset (-1), Offset 0]]
           (VarName nullAnno "test")
  )

nonSymetricLarger_10x6x8 = StencilStream
  "test"
  "test"
  Float
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

nonSymetricTestDataExtremities_10x6x8 = StencilStream
  "test"
  "test"
  Float
  [(1, 10), (1, 6), (1, 8)]
  (Stencil
    nullAnno
    3
    2
    [[Offset 1, Offset 0, Offset (-1)], [Offset (-1), Offset 0, Offset 1]]
    (VarName nullAnno "test")
  )
