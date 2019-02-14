module AddSmartCaches where

import           Data.Ix
import           Data.List
import           Debug.Trace
import qualified KernelExtraction     as K
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Pipeline


-- data SmartCacheInfo = SCI {
--     stencil :: Stencil,
--
--                           }

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
insertSmartCaches :: [K.Kernel] -> IO ()
insertSmartCaches kernels = do
    mapM_ processOneKernel kernels
    return ()
    where
        stenSize = getSmartCacheSize testData

-- THE PLAN :
-- 1) Assume all the arrays are square and of equal side
-- 2) Work out the most disparate StencilIndices
-- 3) Find the index of the indices that differ e.g. (0, -1) and (0, 1) the index that differs is 1
-- 4) Use the array dimensions to work out the size of the smart cache needed

processOneKernel :: K.Kernel -> IO ()
processOneKernel k = do
    putStrLn "--------------------------------\n"
    print k
    mapM_ (\s -> putStrLn (getStreamName s ++ " : " ++ show (getLargestStencilReach s))) requiredStencilStreams
    putStrLn "================================\n"
    return ()
    where
        requiredStencilStreams = filter isStencil $ K.inputStreams k

getStreamName (K.Stream name _ _)          = name
getStreamName (K.StencilStream name _ _ _) = name

-- getKernelsRequiringSmartCaches :: K.Kernel -> Bool
-- getKernelsRequiringSmartCaches kern = numberOfStenStreams > 0
--     where
--         isStencilStream = concatMap isStencil $ K.inputStreams kern
--         numberOfStenStreams = length isStencilStream


isStencil stream = case stream of
                          K.Stream {}        -> False
                          K.StencilStream {} -> True

testData = K.StencilStream "test" K.Float [(0, 400), (1, 500), (0, 300)]
    (Stencil nullAnno 3 2 [[Offset (-1), Offset (-2), Offset 0], [Offset 1, Offset 2, Offset 0],
                           [Offset 0, Offset (-1), Offset 0], [Offset 0, Offset 1, Offset 0]]
                           (VarName nullAnno "test"))
-- Used to workout the size of the smart cache used to buffer a stream
getSmartCacheSize :: K.Stream Anno -> Int
getSmartCacheSize (K.StencilStream name _ arrayDimens stencil) = trace
    ("name: " ++ name ++ "\n" ++ show arrayDimens ++ "\n" ++ show stencil ++ "\n") 0

    where
        (Stencil _ stencilDimens _ stencilIndices _) = stencil

-- this function takes a stencilStream and constructs the follow
-- list of typles [((lower bound, upper bound), (most extreme pair of opposite
-- stencil points)) in the testData example this would be [((0, 400), (Offset -1, Offset 1)),
-- ((1, 500), (Offset -2, Offset 2))]
getLargestStencilReach :: K.Stream Anno -> ([StencilIndex], [StencilIndex])
getLargestStencilReach stream@(K.StencilStream _ _ arrayDimens stencil) =
    if valid then
        mostDistantPair
    else
        error ("Stencil indices invalid: sameLength = " ++ show sameLength ++ " allOffsets = " ++ show allOffsets)
    where
        (Stencil _ _ _ stencilIndices _) = stencil
        valid = allOffsets && sameLength
        sameLength = all (\s -> length s == length arrayDimens) stencilIndices
        allOffsets =  all isOffset $ concat stencilIndices
        isOffset i = case i of Offset _ -> True; _ -> False;
        mostDistantPair = getMostDistantStencilPoints stencilIndices
        -- idxdDimens = indexed arrayDimens

-- sort the
getMostDistantStencilPoints :: [[StencilIndex]] -> ([StencilIndex], [StencilIndex])
getMostDistantStencilPoints input = head sorted
    where
        allPairs = [(x, y) | x <- input, y <-input, x /= y]
        sorted = sortBy (\(p1s1, p1s2) (p2s1, p2s2) ->
            compare (calculateDistance p2s1 p2s2) (calculateDistance p1s1 p1s2)) allPairs

-- Calculate the distance between two stencil indices
calculateDistance :: [StencilIndex] -> [StencilIndex] -> Int
calculateDistance i1 i2 = distance
    where
        valid = length i1 == length i2
        -- This is here to make it respect the stream direction so what dimension the streaming is preformed in
        -- e.g. along dimension 1 then dimension 2 then dimension 3 up to dimension N. A more concrete example
        -- would be in the case of a 2D array do you stream cols then rows or rows then cols?
        -- In an ideal world this would be chosen by the compiler after it works out the smallest
        -- overall stencil usage based on trying different possiblities but that seems like "Future Work" to me
        streamDir = (0, 0) -- This means compare only the left most column of any coords
        coordPairs = map (\idx -> (i1!!idx, i2!!idx)) (range streamDir)
        distance = foldr (\(Offset v1, Offset v2) acc -> acc + (v1 - v2)^2) 0 coordPairs

-- getOneStencilReachItem :: [[StencilIndex]] -> ((Int, Int), Int) -> (Int, (Int, Int), (StencilIndex, StencilIndex))
-- getOneStencilReachItem stencilIndices ((lwb, upb), idx) =
--     where
--         addItem cur acc = if (cur!!idx) `elem` acc then acc else acc ++ cur
--         stencilsOnThisDimension = foldr addItem [] stencilIndices
--         allPairs = [(x, y) | x <- stencilsOnThisDimension, y <- stencilsOnThisDimension]
--         allPairsAndDifference = map (\(of1@(Offset v1),of2@(Offset v2)) -> ((abs v1) + (abs v2), of1, of2)) allPairs
--         largestReach = foldr updateLargest  (0, (Offset 0, Offset 0)) allPairsAndDifference
--         updateLargest (c@(curDif, (of1, of2)) ac@(acDif, _) = if curDif > acDif then c else ac)
