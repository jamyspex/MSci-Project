module StencilTest where

import           Control.Exception
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


crossWithExtraLMIPointsToBeIgnoredTestData3D_6x10x8 = K.StencilStream
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


crossWithExtraLMIPointsNotIgnoredTestData3D_6x10x8 = K.StencilStream
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
  (  calculateStencilSize (defaultIterationOrder 3) crossTestData3D_8x8x8
  == 129
  && calculateStencilSize (defaultIterationOrder 3) crossTestData3D_6x10x8
  == 121
  && calculateStencilSize
       (defaultIterationOrder 3)
       crossWithExtraLMIPointsToBeIgnoredTestData3D_6x10x8
  == 121
  )
  "Assertions passed"

defaultIterationOrder dims = range (0, dims - 1)

calculateStencilSize :: [Int] -> K.Stream Anno -> Int
calculateStencilSize iterationOrder stenStream@(K.StencilStream name _ arrayDimens stencil)
  = area
 where
  (Stencil _ stencilDimens _ stencilIndices _) = stencil
  stencilIndicesInts  = map (map (\(Offset v) -> v)) stencilIndices
  lastMoveIndex       = last iterationOrder
  lastMoveIndexValues = map (!! lastMoveIndex) stencilIndicesInts
  allPairsOfLastMoveIndex =
    [ (x, y) | x <- lastMoveIndexValues, y <- lastMoveIndexValues, x /= y ]
  stencilSizesAndLMIPairs = map calculateReach allPairsOfLastMoveIndex
  (area, points)          = maximumBy (comparing fst) stencilSizesAndLMIPairs
  calculateReach :: (Int, Int) -> (Int, (Int, Int))
  calculateReach (lm1, lm2) =
    (lastMoveIndexDifference * productOfOtherDimensions + 1, (lm1, lm2))
   where
    lastMoveIndexDifference = abs lm1 + abs lm2
    productOfOtherDimensions =
      foldl dimensionProductFold 1 $ init iterationOrder
    dimensionProductFold acc cur = (upb - lwb + 1) * acc
      where (lwb, upb) = arrayDimens !! cur


