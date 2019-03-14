{-# Language LambdaCase, RecordWildCards #-}
module MemoryAccessCodeGen where

import qualified Data.Map                      as DMap
import           Data.List.Index
import           MiniPP
import           FortranDSL
import           CodeGenUtils
import           Utils
import           Language.Fortran
import           LanguageFortranTools

generateMemoryAccess
  :: [PipelineStage] -> IO [(ProgUnit Anno, KernelCallingData)]
generateMemoryAccess pipeline = do
  mapM_ showProgUnitWithCallingData (memoryReaders ++ memoryWriters)
  return []
 where
  memoryAccess  = concatMap (\(_, _, memAc) -> memAc) pipeline
  memoryReaders = map generateMemoryReader $ filter
    (\case
      MemoryReader{} -> True
      _              -> False
    )
    memoryAccess
  memoryWriters = map generateMemoryWriter $ filter
    (\case
      MemoryWriter{} -> True
      _              -> False
    )
    memoryAccess

-- TODO add validation to ensure exactly 1 written pipe and 1 stream
generateMemoryReader
  :: PipelineItem SharedPipelineData -> (ProgUnit Anno, KernelCallingData)
generateMemoryReader memRead@MemoryReader {..} = if valid
  then (kernel, callingData)
  else error ("More than one output stream from memory reader " ++ name)
 where
  valid = length writtenPipes == 1 && length memToOutputStreams == 1
  loopVarName            = "index"
  driverLoopBoundVarName = "nLoop"
  kernel                 = sub name decls mainLoop arrayArg
  callingData =
    KCD {argPositions = imap (\idx (ArgName _ name) -> (idx, name)) arrayArg}
  arrayArg = [argName arrayName]
  decls    = declNode
    [ memBufferDecl
    , readOutVarDecl
    , intParam driverLoopBoundVarName (driverLoopUpperBound sharedData)
    , intDecl loopVarName
    ]
  mainLoop = for loopVarName
                 (driverLoopLowerBound sharedData + 1)
                 (var driverLoopBoundVarName)
                 pipeWriteCode
  memBufferName = arrayName
  memBufferDecl = bufferDecl arrayName
                             [(1, getFlattenArrayUpperBound mem)]
                             (getFortranTypeForStream stream)
  readOutVarName        = streamName
  readOutVarDecl = typedDecl readOutVarName (getFortranTypeForStream stream)
  Pipe _ _ pipeName _ _ = head writtenPipes
  pipeWriteCode         = block $ generatePipeWrite (var loopVarName)
                                                    readOutVarName
                                                    memBufferName
                                                    pipeName
  (mem, stream@(Stream streamName arrayName _ _)) = head memToOutputStreams

getFlattenArrayUpperBound :: FPGAMemArray -> Int
getFlattenArrayUpperBound FPGAMemArray {..} =
  foldl (\acc (lwb, upb) -> ((upb - lwb) + 1) * acc) 1 dimensions

generateMemoryWriter
  :: PipelineItem SharedPipelineData -> (ProgUnit Anno, KernelCallingData)
generateMemoryWriter memWriter@MemoryWriter {..} = (kernel, callingData)
 where
  loopVarName            = "index"
  driverLoopBoundVarName = "nLoop"
  kernel                 = sub name decls mainLoop arrayArg
  callingData =
    KCD {argPositions = imap (\idx (ArgName _ name) -> (idx, name)) arrayArg}
  arrayArg = pipeReadArgs
  decls    = declNode
    ( intParam driverLoopBoundVarName (driverLoopUpperBound sharedData)
    : intDecl loopVarName
    : concat pipeReadDecls
    )
  mainLoop = for loopVarName
                 (driverLoopLowerBound sharedData + 1)
                 (var driverLoopBoundVarName)
                 (block $ concat pipeReadCode)
  (pipeReadCode, pipeReadDecls, pipeReadArgs) =
    unzip3 $ map (generatePipeReadMemWriter loopVarName) generationData
  generationData = matchPipesToStreams memWriter

generatePipeReadMemWriter
  :: String
  -> (Pipe, (Stream Anno, FPGAMemArray))
  -> ([Fortran Anno], [Decl Anno], ArgName Anno)
generatePipeReadMemWriter loopVarName (Pipe _ _ pipeName _ pipeStream, (s@(Stream streamName arrayName _ _), fpgaMem))
  = if valid
    then
      ( fortran
      , [ typedDecl readInVarName (getFortranTypeForStream s)
        , bufferDecl arrayName
                     [(1, getFlattenArrayUpperBound fpgaMem)]
                     (getFortranTypeForStream s)
        ]
      , argName arrayName
      )
    else error
      (  "pipeStream name doesn't equal matched stream name! ("
      ++ getStreamName pipeStream
      ++ " /= "
      ++ streamName
      ++ ")"
      )
 where
  valid = getStreamName pipeStream == streamName
  readInVarName = streamName ++ "_read_in"
  fortran = generatePipeRead (var loopVarName) pipeName readInVarName arrayName

matchPipesToStreams
  :: PipelineItem SharedPipelineData -> [(Pipe, (Stream Anno, FPGAMemArray))]
matchPipesToStreams MemoryWriter {..} = DMap.elems combined
 where
  pipeMap = DMap.fromList
    $ map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) readPipes
  inputStreamMap = DMap.fromList
    $ map (\(s, fpgaMem) -> (getStreamName s, (s, fpgaMem))) inputStreamsToMem
  combined = DMap.intersectionWith
    (\pipe streamFPGAMem -> (pipe, streamFPGAMem))
    pipeMap
    inputStreamMap
