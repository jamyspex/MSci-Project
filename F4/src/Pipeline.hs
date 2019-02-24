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
        name          :: String,
        fortran       :: ProgUnit Anno,
        nextStage     :: Pipeline a,
        sharedData    :: a
    } | Reduce {
        inputStreams  :: [Stream Anno],
        outputStreams :: [Stream Anno],
        name          :: String,
        fortran       :: ProgUnit Anno,
        nextStage     :: Pipeline a,
        sharedData    :: a
    } | SmartCache {
        inputStreams   :: [Stream Anno],
        outputStreams  :: [Stream Anno],
        smartCacheName :: String,
        smartCacheSize :: Int,
        cacheLines     :: [SmartCacheItem],
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
    } | NullStage deriving Show

-- -- Data type used to represent a stream flowing between kernels
-- -- will likely directly map to a pipe in OpenCL
-- data Stream = Stream String StreamValueType deriving Show

-- data StreamValueType = Float deriving Show

newtype FPGAMemArray = FPGAMemArray {
    arrayName :: String
} deriving Show

data SharedPipelineData = SPD {
    driverLoopSize :: Int
} | NullPipeLineData deriving Show
