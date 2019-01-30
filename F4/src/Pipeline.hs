module Pipeline where

import           Language.Fortran
import           LanguageFortranTools

-- Data type used to represent a processing pipeline
-- for output to a Fortran kernel file
data Pipeline a = Kernel {
        inputStreams  :: [Stream],
        outputStreams :: [Stream],
        kernelName    :: String,
        body          :: Fortran Anno,
        nextStage     :: Pipeline a,
        sharedData    :: a
    } | SmartCache {
        inputStreams   :: [Stream],
        stencils       :: [Stencil Anno],
        outputStreams  :: [Stream],
        smartCachename :: String,
        nextStage      :: Pipeline a,
        sharedData     :: a
    } | MemoryReader {
        memMapToOutputStreams :: [(FPGAMemArray, Stream)],
        nextStage             :: Pipeline a,
        memReaderName         :: String,
        sharedData            :: a
    } | MemoryWriter {
        inputStreamsToMemMap :: [(Stream, FPGAMemArray)],
        memWriterName        :: String,
        sharedData           :: a
    } deriving Show

-- Data type used to represent a stream flowing between kernels
-- will likely directly map to a pipe in OpenCL
data Stream = Stream String StreamValueType deriving Show

data StreamValueType = Float deriving Show

data FPGAMemArray = FPGAMemArray {
    name :: String
} deriving Show

data SharedPipelineData = SPD {
    driverLoopSize :: Int
} deriving Show
