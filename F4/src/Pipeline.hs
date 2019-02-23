module Pipeline where

import           Language.Fortran
import           LanguageFortranTools
import           SmartCacheParameterAnalysis
import           Utils

-- Data type used to represent a processing pipeline
-- for output to a Fortran kernel file
data Pipeline a = Map {
        inputStreams  :: [Stream Anno],
        outputStreams :: [Stream Anno],
        kernelName    :: String,
        fortran       :: ProgUnit Anno,
        nextStage     :: Pipeline a,
        sharedData    :: a
    } | Reduce {
        inputStreams  :: [Stream Anno],
        outputStreams :: [Stream Anno],
        kernelName    :: String,
        body          :: ProgUnit Anno,
        nextStage     :: Pipeline a,
        sharedData    :: a
    } | SmartCache {
        inputStreams   :: [Stream Anno],
        outputStreams  :: [Stream Anno],
        smartCacheName :: String,
        smartCacheSize :: Int,
        cacheLines     :: [SmartCacheDetailsForStream],
        nextStage      :: Pipeline a,
        sharedData     :: a
    } | MemoryReader {
        memMapToOutputStreams :: [(FPGAMemArray, Stream Anno)],
        nextStage             :: Pipeline a,
        memReaderName         :: String,
        sharedData            :: a
    } | MemoryWriter {
        inputStreamsToMemMap :: [(Stream Anno, FPGAMemArray)],
        memWriterName        :: String,
        sharedData           :: a
    } deriving Show

-- -- Data type used to represent a stream flowing between kernels
-- -- will likely directly map to a pipe in OpenCL
-- data Stream = Stream String StreamValueType deriving Show

-- data StreamValueType = Float deriving Show

newtype FPGAMemArray = FPGAMemArray {
    name :: String
} deriving Show

newtype SharedPipelineData = SPD {
    driverLoopSize :: Int
} deriving Show
