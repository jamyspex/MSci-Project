{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Pipeline where

import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           SmartCacheParameterAnalysis
import           Utils

-- Data type used to represent a processing pipeline
-- for output to a Fortran kernel file
data PipelineItem a
  = Map { inputStreams  :: [Stream Anno]
        , outputStreams :: [Stream Anno]
        , name          :: String
        , fortran       :: ProgUnit Anno
        , nextStage     :: PipelineItem a
        , sharedData    :: a }
  | Reduce { inputStreams  :: [Stream Anno]
           , outputStreams :: [Stream Anno]
           , name          :: String
           , fortran       :: ProgUnit Anno
           , nextStage     :: PipelineItem a
           , sharedData    :: a }
  | SmartCache { inputStreams   :: [Stream Anno]
               , outputStreams  :: [Stream Anno]
               , name           :: String
               , smartCacheSize :: Int
               , cacheLines     :: [SmartCacheItem]
               , nextStage      :: PipelineItem a
               , sharedData     :: a }
  | MemoryReader { memToOutputStreams :: [(FPGAMemArray, Stream Anno)]
                 , nextStage          :: PipelineItem a
                 , name               :: String
                 , sharedData         :: a }
  | MemoryWriter { inputStreamsToMem :: [(Stream Anno, FPGAMemArray)]
                 , name              :: String
                 , sharedData        :: a }
  | NullStage

instance Show (PipelineItem SharedPipelineData) where
  show Map {..} =
    rule '~' ++
    "This is a map kernel.\nName:" ++
    name ++
    hl ++
    "Input Streams:\n" ++
    printAllStreams inputStreams ++
    "OutputStreams:\n" ++
    printAllStreams outputStreams ++
    hl ++ miniPPProgUnit fortran ++ hl ++ show sharedData ++ rule '~'
  show Reduce {..} =
    rule '~' ++
    "This is a reduce kernel.\nName:" ++
    name ++
    hl ++
    "Input Streams:\n" ++
    printAllStreams inputStreams ++
    "Output Streams:\n" ++
    printAllStreams outputStreams ++
    hl ++ miniPPProgUnit fortran ++ hl ++ show sharedData ++ rule '~'
  show SmartCache {..} =
    rule '~' ++
    "This is a smart cache kernel.\nName: " ++
    name ++
    "\nSize: " ++
    show smartCacheSize ++
    "\nCache Lines:\n" ++
    concatMap
      (\cl -> (unlines . map ("\t" ++) . lines . show) cl ++ "\n")
      cacheLines ++
    "\n" ++
    "Input Streams:\n" ++
    printAllStreams inputStreams ++
    "Output Streams:\n" ++ printAllStreams outputStreams ++ rule '~'
  show MemoryReader {..} =
    rule '~' ++
    "This is a memory reader kernel.\nName: " ++
    name ++
    hl ++
    "Memory to streams:\n" ++
    concatMap
      (\(mem, stream) -> "\t" ++ arrayName mem ++ " --> " ++ show stream ++ "\n")
      memToOutputStreams ++
    rule '~'
  show MemoryWriter {..} =
    rule '~' ++
    "This is a memory writer kernel.\nName: " ++
    name ++
    hl ++
    "Streams to memory:\n" ++
    concatMap
      (\(stream, mem) -> "\t" ++ show stream ++ " --> " ++ arrayName mem ++ "\n")
      inputStreamsToMem ++
    rule '~'

printAllStreams = concatMap (\s -> "\t" ++ printStream s ++ "\n")

-- -- Data type used to represent a stream flowing between kernels
-- -- will likely directly map to a pipe in OpenCL
-- data Stream = Stream String StreamValueType deriving Show
-- data StreamValueType = Float deriving Show
newtype FPGAMemArray = FPGAMemArray
  { arrayName :: String
  } deriving (Show)

data SharedPipelineData
  = SPD { driverLoopSize :: Int }
  | NullPipeLineData
  deriving (Show)
