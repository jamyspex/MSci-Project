module Pipeline where

import           KernelExtraction
import           Language.Fortran
import           LanguageFortranTools

-- Data type used to represent a processing pipeline
-- for output to a Fortran kernel file
data Pipeline a = Map {
        inputStreams  :: [Stream Anno],
        outputStreams :: [Stream Anno],
        kernelName    :: String,
        body          :: ProgUnit Anno,
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
        stencils       :: [Stencil Anno],
        outputStreams  :: [Stream Anno],
        smartCachename :: String,
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

data FPGAMemArray = FPGAMemArray {
    name :: String
} deriving Show

data SharedPipelineData = SPD {
    driverLoopSize :: Int
} deriving Show
