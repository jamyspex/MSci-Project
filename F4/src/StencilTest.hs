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


crossTestData3D_6x10x8 = K.StencilStream
  "test"
  K.Float
  [(1, 6), (1, 10), (1, 8)]
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


crossWithExtraPointsToBeIgnoredTestData3D_6x10x8 = K.StencilStream
  "test"
  K.Float
  [(1, 6), (1, 10), (1, 8)]
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


crossWithExtraPointsNotIgnoredTestData3D_6x10x8 = K.StencilStream
  "test"
  K.Float
  [(1, 6), (1, 10), (1, 8)]
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

nonSymetricTestData3D_8x8x8 = K.StencilStream
  "test"
  K.Float
  [(1, 8), (1, 8), (1, 8)]
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

assertions = assert
  (  areaOnly crossTestData3D_8x8x8
  == 129
  && areaOnly crossTestData3D_6x10x8
  == 121
  && areaOnly crossWithExtraPointsToBeIgnoredTestData3D_6x10x8
  == 121
 -- && areaOnly crossWithExtraPointsNotIgnoredTestData3D_6x10x8
 -- == 141
  )
  "Assertions passed"

defaultIterationOrder dims = range (0, dims - 1)

printResults stream =
  putStrLn
    $ concatMap
        (\(area, (i1, i2)) -> printf "Area = %d index 1 = %s index 2 = %s\n"
                                     area
                                     (show i1)
                                     (show i2)
        )
    $ sortBy
        (\(area1, (i1idx1, i1idx2)) (area2, (i2idx1, i2idx2)) ->
          area1
            `compare` area2
            <>        (         count (/= 0) (i2idx1 ++ i2idx2)
                      `compare` count (/= 0) (i1idx1 ++ i1idx2)
                      )
           -- <>        compareIndices i1idx1 i2idx1 (defaultIterationOrder 3)
           -- <>        compareIndices i1idx2 i2idx2 (defaultIterationOrder 3)
        )
    $ calculateStencilSize (defaultIterationOrder 3) stream

count pred = length . filter pred

areaOnly sten =
  let results         = calculateStencilSize (defaultIterationOrder 3) sten
      (area, indices) = maximumBy (comparing fst) results
  in  area

calculateStencilSize :: [Int] -> K.Stream Anno -> [(Int, ([Int], [Int]))]
calculateStencilSize iterationOrder stenStream@(K.StencilStream name _ arrayDimens stencil)
  = stencilSizesAndIndexPairs
 where
  (Stencil _ stencilDimens _ stencilIndices _) = stencil
  stencilIndicesInts = map (map (\(Offset v) -> v)) stencilIndices
  allIndexPairs =
    [ (x, y) | x <- stencilIndicesInts, y <- stencilIndicesInts, x /= y ]
  smallIndexFirstOnly =
    filter (\(x, y) -> compareIndices x y iterationOrder == LT) allIndexPairs
  stencilSizesAndIndexPairs = map go smallIndexFirstOnly
  go (l1, l2) =
    let initial = ((l1, l2), True, 0)
        ((ol1, ol2), _, totArea) =
          foldl combineReaches initial (reverse iterationOrder)
    in  (totArea + 1, (ol1, ol2))
-- last move index is the most significant therefore its impossible for it to
-- be subsumed by another index value so skip the check to see if it is > 0 when 
-- adding its buffer contribution 
  combineReaches
    :: (([Int], [Int]), Bool, Int) -- ((indice components), first iteration, area)
    -> Int                         -- iteration order  
    -> (([Int], [Int]), Bool, Int) -- ((indice components), False, total area)
  combineReaches ((idx1, idx2), firstIter, areaSoFar) component =
    if firstIter || i1 > 0 || i2 < 0
      then ((idx1, idx2), False, areaSoFar + calculateReach component (i1, i2))
      else ((idx1, idx2), False, areaSoFar)
   where
    i1 = idx1 !! component
    i2 = idx2 !! component
  calculateReach :: Int -> (Int, Int) -> Int
  calculateReach pos (lm1, lm2) =
  --  trace
  --  (  "calculateReach: pos = "
  --  ++ show pos
  --  ++ " num blocks = "
  --  ++ show numBlocks
  --  ++ " index 1 = "
  --  ++ show lm1
  --  ++ " index 2 = "
  --  ++ show lm2
  --  ++ " dimension diff = "
  --  ++ show indexDiff
  --  )
                                  numBlocks
   where
    numBlocks = indexDiff * productOfOtherDimensions
    indexDiff = abs lm1 + abs lm2
    productOfOtherDimensions =
      foldl dimensionProductFold 1 (take pos iterationOrder)
    dimensionProductFold acc cur = ((upb - lwb) + 1) * acc
      where (lwb, upb) = arrayDimens !! cur



compareIndices :: [Int] -> [Int] -> [Int] -> Ordering
compareIndices i1 i2 iterationOrder = if sameLength
  then orderExpr
  else error "indices of different lengths"
 where
  sameLength = length i1 == length i2
  orderExpr  = foldl (\acc cur -> acc <> ((i1 !! cur) `compare` (i2 !! cur)))
                     EQ
                     iterationOrder

