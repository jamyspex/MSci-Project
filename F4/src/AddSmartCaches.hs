module AddSmartCaches where

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
    where
        output = concatMap (\k -> "\n" ++ show k ++ "\n" ++ "stencil size = " ++ show stenSize) kernelsRequiringSmartCache
        kernelsRequiringSmartCache = filter getKernelsRequiringSmartCaches kernels
        stenSize = getSmartCacheSize testData

getKernelsRequiringSmartCaches :: K.Kernel -> Bool
getKernelsRequiringSmartCaches kern = numberOfStenStreams > 0
    where
        isStencil stream = case stream of
                          K.Stream _ _ _          -> []
                          K.StencilStream _ _ _ _ -> [True]
        isStencilStream = concatMap isStencil $ K.inputStreams kern
        numberOfStenStreams = length isStencilStream


testData = K.StencilStream "test" K.Float [(0, 400), (1, 500), (0, 300)]
    (Stencil nullAnno 3 2 [[Offset (-1), Offset 0, Offset 0], [Offset 1, Offset 0, Offset 0]] (VarName nullAnno "test"))
-- Used to workout the size of the smart cache used to buffer a stream
getSmartCacheSize :: K.Stream Anno -> Int
getSmartCacheSize (K.StencilStream name _ arrayDimens stencil) = trace
    ("name: " ++ name ++ "\n" ++ show arrayDimens ++ "\n" ++ show stencil ++ "\n") 0

    where
        (Stencil _ stencilDimens _ stencilIndices _) = stencil

 -- getIndice
