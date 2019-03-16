{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module MemoryAccessCodeGen where

import           CodeGenUtils
import           Data.Char
import           Data.List.Index
import qualified Data.Map             as DMap
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

generateMemoryAccess ::
     [PipelineStage] -> IO [(ProgUnit Anno, KernelCallingData)]
generateMemoryAccess pipeline = do
  mapM_ showProgUnitWithCallingData (memoryReaders ++ memoryWriters)
  return (memoryReaders ++ memoryWriters)
  where
    memoryAccess = concatMap (\(_, _, memAc) -> memAc) pipeline
    memoryReaders =
      map generateMemoryReader $
      filter
        (\case
           MemoryReader {} -> True
           _ -> False)
        memoryAccess
    memoryWriters =
      map generateMemoryWriter $
      filter
        (\case
           MemoryWriter {} -> True
           _ -> False)
        memoryAccess

-- TODO add validation to ensure exactly 1 written pipe and 1 stream
generateMemoryReader ::
     PipelineItem SharedPipelineData -> (ProgUnit Anno, KernelCallingData)
generateMemoryReader memRead@MemoryReader {..} =
  if valid
    then (kernel, callingData)
    else error ("More than one output stream from memory reader " ++ name)
  where
    valid = length writtenPipes == 1 && length memToOutputStreams == 1
    loopVarName = "index"
    driverLoopBoundVarName = "nLoop"
    kernel = sub name decls mainLoop arrayArg
    callingData =
      KCD {argPositions = imap (\idx (ArgName _ name) -> (idx, name)) arrayArg}
    arrayArg = [argName memBufferName]
    decls = declNode (memBufferDecl : readOutVarDecl : loopVarDecls)
    memBufferName = originalArrayName
    memBufferDecl =
      bufferDecl memBufferName dimensions (getFortranTypeForStream stream)
    readOutVarName = streamName
    readOutVarDecl = typedDecl readOutVarName (getFortranTypeForStream stream)
    loopVars =
      reverse $ take (length dimensions) $ map (\i -> [chr i]) [97 .. 122]
    (mainLoop, loopVarDecls) =
      foldl buildDriverLoopNest (writeCode, []) $ zip dimensions loopVars
    writeCode =
      block $
      generateArrayReadPipeWrite arrayName readOutVarName loopVars pipeName
    Pipe _ _ pipeName _ _ = head writtenPipes
    pipeWriteCode =
      block $
      generatePipeWrite (var loopVarName) readOutVarName memBufferName pipeName
    (FPGAMemArray {..}, stream@(Stream streamName originalArrayName _ _)) =
      head memToOutputStreams

generateArrayReadPipeWrite ::
     String -> String -> [String] -> String -> [Fortran Anno]
generateArrayReadPipeWrite arrayName readOutName loopVars pipeName =
  [ assign (var readOutName) (arrayVar arrayName $ map var (reverse loopVars))
  , writePipe [pipeName, readOutName]
  ]

buildDriverLoopNest ::
     (Fortran Anno, [Decl Anno])
  -> ((Int, Int), String)
  -> (Fortran Anno, [Decl Anno])
buildDriverLoopNest (innerLoop, currentDecls) ((lwb, upb), loopVarName) =
  (loop, intDecl loopVarName : currentDecls)
  where
    loop = for loopVarName lwb (con upb) innerLoop

getFlattenArrayUpperBound :: FPGAMemArray -> Int
getFlattenArrayUpperBound FPGAMemArray {..} =
  foldl (\acc (lwb, upb) -> ((upb - lwb) + 1) * acc) 1 dimensions

generateMemoryWriter ::
     PipelineItem SharedPipelineData -> (ProgUnit Anno, KernelCallingData)
generateMemoryWriter memWriter@MemoryWriter {..} =
  if valid
    then (kernel, callingData)
    else error "All buffer dimensions not equal in memory writer"
  where
    loopVarName = "index"
    driverLoopBoundVarName = "nLoop"
    kernel = sub name decls mainLoop arrayArg
    callingData =
      KCD {argPositions = imap (\idx (ArgName _ name) -> (idx, name)) arrayArg}
    arrayArg = pipeReadArgs
    decls = declNode (intDecl loopVarName : concat pipeReadDecls)
    valid = all (== head allBuffDimensions) allBuffDimensions
    allBuffDimensions = map (dimensions . snd) inputStreamsToMem
    loopVars =
      reverse $
      take ((length . head) allBuffDimensions) $ map (\i -> [chr i]) [97 .. 122]
    (mainLoop, loopVarDecls) =
      foldl buildDriverLoopNest (block $ concat pipeReadCode, []) $
      zip (head allBuffDimensions) loopVars
    (pipeReadCode, pipeReadDecls, pipeReadArgs) =
      unzip3 $ map (generatePipeReadMemWriterBody loopVars) generationData
    generationData = matchPipesToStreams memWriter

generatePipeReadMemWriterBody ::
     [String]
  -> (Pipe, (Stream Anno, FPGAMemArray))
  -> ([Fortran Anno], [Decl Anno], ArgName Anno)
generatePipeReadMemWriterBody loopVars (Pipe _ _ pipeName _ pipeStream, (s@(Stream streamName arrayName _ _), fpgaMem)) =
  if valid
    then ( fortran
         , [ typedDecl readInVarName (getFortranTypeForStream s)
           , bufferDecl
               arrayName
               (dimensions fpgaMem)
               (getFortranTypeForStream s)
           ]
         , argName arrayName)
    else error
           ("pipeStream name doesn't equal matched stream name! (" ++
            getStreamName pipeStream ++ " /= " ++ streamName ++ ")")
  where
    valid = getStreamName pipeStream == streamName
    readInVarName = streamName ++ "_read_in"
    fortran =
      generateArrayWritePipeRead arrayName readInVarName loopVars pipeName

generateArrayWritePipeRead ::
     String -> String -> [String] -> String -> [Fortran Anno]
generateArrayWritePipeRead arrayName readInName loopVars pipeName =
  [ readPipe [pipeName, readInName]
  , assign (arrayVar arrayName $ map var (reverse loopVars)) (var readInName)
  ]

matchPipesToStreams ::
     PipelineItem SharedPipelineData -> [(Pipe, (Stream Anno, FPGAMemArray))]
matchPipesToStreams MemoryWriter {..} = DMap.elems combined
  where
    pipeMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) readPipes
    inputStreamMap =
      DMap.fromList $
      map (\(s, fpgaMem) -> (getStreamName s, (s, fpgaMem))) inputStreamsToMem
    combined =
      DMap.intersectionWith
        (\pipe streamFPGAMem -> (pipe, streamFPGAMem))
        pipeMap
        inputStreamMap
