module AddSmartCaches where

import           Data.Ix
import           Data.List
import           Debug.Trace
import qualified KernelExtraction     as K
import           Language.Fortran
import           LanguageFortranTools
import           Pipeline
-- data Pipeline a = Map {
--         inputStreams  :: [Stream],
--         outputStreams :: [Stream],
--         kernelName    :: String,
--         body          :: ProgUnit Anno,
--         nextStage     :: Pipeline a,
--         sharedData    :: a
--     } | Reduce {
--         inputStreams  :: [Stream],
--         outputStreams :: [Stream],
--         kernelName    :: String,
--         body          :: ProgUnit Anno,
--         nextStage     :: Pipeline a,
--         sharedData    :: a
--     } | SmartCache {
--         inputStreams   :: [Stream],
--         stencils       :: [Stencil Anno],
--         outputStreams  :: [Stream],
--         smartCachename :: String,
--         nextStage      :: Pipeline a,
--         sharedData     :: a
--     } | MemoryReader {
--         memMapToOutputStreams :: [(FPGAMemArray, Stream)],
--         nextStage             :: Pipeline a,
--         memReaderName         :: String,
--         sharedData            :: a
--     } | MemoryWriter {
--         inputStreamsToMemMap :: [(Stream, FPGAMemArray)],
--         memWriterName        :: String,
--         sharedData           :: a
--     } deriving Show


-- data Kernel = Kernel {
--     inputStreams        :: [Stream Anno],
--     outputStreams       :: [Stream Anno],
--     kernelName          :: String,
--     outputReductionVars :: [String],
--     body                :: ProgUnit Anno,
--     order               :: Int
-- }

-- This module anlayses the list of kernel subroutines and their required
-- input streams. If it finds StencilStream inputs it constructs an appropriate smart cache
-- and inserts it into the list with the appropriate position value set.
insertSmartCaches :: [K.Kernel] -> IO ()
insertSmartCaches kernels = do
    putStrLn output
    print $ getStencilReachWithArrayDimens testData
    return ()
    where
        output = concatMap (\k -> "\n" ++ show k ++ "\n" ++ "stencil size = " ++ show stenSize) kernelsRequiringSmartCache
        kernelsRequiringSmartCache = filter getKernelsRequiringSmartCaches kernels
        stenSize = getSmartCacheSize testData

getKernelsRequiringSmartCaches :: K.Kernel -> Bool
getKernelsRequiringSmartCaches kern = numberOfStenStreams > 0
    where
        isStencil stream = case stream of
                          K.Stream {}        -> []
                          K.StencilStream {} -> [True]
        isStencilStream = concatMap isStencil $ K.inputStreams kern
        numberOfStenStreams = length isStencilStream


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
getStencilReachWithArrayDimens :: K.Stream Anno -> ([StencilIndex], [StencilIndex]) --[((Int, Int), (StencilIndex, StencilIndex))]
getStencilReachWithArrayDimens stream@(K.StencilStream _ _ arrayDimens stencil) =
    if trace ("allOffsets = " ++ show allOffsets ++ " sameLength = " ++ show sameLength) valid then
        trace ("most distant points = " ++ show mostDistantPair) mostDistantPair
    else
        error "Stencil indices invalid"
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


calculateDistance :: [StencilIndex] -> [StencilIndex] -> Int
calculateDistance i1 i2 = distance
    where
        valid = length i1 == length i2
        coordPairs = map (\idx -> (i1!!idx, i2!!idx)) (range (0,length i1 - 1))
        distance = foldr (\((Offset v1), (Offset v2)) acc -> acc + (v1 - v2)^2) 0 coordPairs

-- getOneStencilReachItem :: [[StencilIndex]] -> ((Int, Int), Int) -> (Int, (Int, Int), (StencilIndex, StencilIndex))
-- getOneStencilReachItem stencilIndices ((lwb, upb), idx) =
--     where
--         addItem cur acc = if (cur!!idx) `elem` acc then acc else acc ++ cur
--         stencilsOnThisDimension = foldr addItem [] stencilIndices
--         allPairs = [(x, y) | x <- stencilsOnThisDimension, y <- stencilsOnThisDimension]
--         allPairsAndDifference = map (\(of1@(Offset v1),of2@(Offset v2)) -> ((abs v1) + (abs v2), of1, of2)) allPairs
--         largestReach = foldr updateLargest  (0, (Offset 0, Offset 0)) allPairsAndDifference
--         updateLargest (c@(curDif, (of1, of2)) ac@(acDif, _) = if curDif > acDif then c else ac)
