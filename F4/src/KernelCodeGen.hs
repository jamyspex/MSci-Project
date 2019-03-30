{-# LANGUAGE RecordWildCards #-}

module KernelCodeGen where

import           CodeGenUtils
import           Data.Generics
import           Data.List.Index
import qualified Data.Map             as DMap
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           Utils

generateKernels :: [PipelineStage] -> IO [(ProgUnit Anno, KernelCallingData)]
generateKernels pipeline = do
  mapM_ showProgUnitWithCallingData kernels
  return kernels
  where
    kernels = map (\(k, _, _) -> generateKernelCode k) pipeline

generateKernelCode ::
     PipelineItem SharedPipelineData -> (ProgUnit Anno, KernelCallingData)
generateKernelCode mapKern@Map {..} = (kernel, callingData)
  where
    kernel = sub name decls mainLoop loopVarArgsRemoved
    callingData =
      KCD
        { argPositions =
            imap (\idx (ArgName _ name) -> (idx, name)) loopVarArgsRemoved
        , kernelName = name
        , subroutineName = originalSubName
        }
    loopVarName = driverLoopIndexName sharedData
    driverLoopBoundVarName = "nloop"
    mainLoop =
      for
        loopVarName
        (driverLoopLowerBound sharedData + 1)
        (var driverLoopBoundVarName)
        (block [block pipeReads, kernelBodyWithoutAnnos, block pipeWrites])
    pipeReads = generatePipeReadsKernel mapKern
    pipeWrites = generatePipeWritesKernel mapKern
    kernelBodyWithoutAnnos = stripOpenCLAnnos fortran
    decls =
      declNode $
      intParam driverLoopBoundVarName (driverLoopUpperBound sharedData) :
      intDecl loopVarName : getDecls fortran
    args = getArgs fortran
    loopVars = getLoopVarNames fortran
    loopVarArgsRemoved =
      filter (\(ArgName _ name) -> name `notElem` loopVars) args
generateKernelCode reduceKern@Reduce {..} = (kernel, callingData)
  where
    kernel = sub name decls mainLoop loopVarArgsRemoved
    callingData =
      KCD
        { argPositions =
            imap (\idx (ArgName _ name) -> (idx, name)) loopVarArgsRemoved
        , kernelName = name
        , subroutineName = originalSubName
        }
    loopVarName = driverLoopIndexName sharedData
    driverLoopBoundVarName = "nloop"
    mainLoop =
      block
        [ for
            loopVarName
            (driverLoopLowerBound sharedData + 1)
            (var driverLoopBoundVarName)
            (block [block pipeReads, kernelBodyWithoutAnnos])
        , for
            loopVarName
            (driverLoopLowerBound sharedData + 1)
            (var driverLoopBoundVarName)
            (block pipeWrites)
        ]
    pipeReads = generatePipeReadsKernel reduceKern
    pipeWrites = generatePipeWritesKernel reduceKern
    kernelBodyWithoutAnnos = stripOpenCLAnnos fortran
    decls =
      declNode $
      intParam driverLoopBoundVarName (driverLoopUpperBound sharedData) :
      intDecl loopVarName : getDecls fortran
    args = getArgs fortran
    loopVars = getLoopVarNames fortran
    loopVarArgsRemoved =
      filter (\(ArgName _ name) -> name `notElem` loopVars) args

getLoopVarNames :: ProgUnit Anno -> [String]
getLoopVarNames kernelBody = map getNameFromVarName loopVars
  where
    loopVars = everything (++) (mkQ [] getLoopVars) kernelBody

stripOpenCLAnnos :: ProgUnit Anno -> Fortran Anno
stripOpenCLAnnos kernelBody = annosRemoved
  where
    body = getSubBody kernelBody
    annosRemoved = everywhere (mkT removeAnnosQuery) body
    removeAnnosQuery :: Fortran Anno -> Fortran Anno
    removeAnnosQuery fortran =
      case fortran of
        (OpenCLMap _ _ _ _ _ _ body)      -> body
        (OpenCLReduce _ _ _ _ _ _ _ body) -> body
        (OpenCLStencil _ _ _ body)        -> body
        _                                 -> fortran

getInReductionVarStreams :: PipelineItem SharedPipelineData -> [Stream Anno]
getInReductionVarStreams Map {..} =
  map buildDummyStreamFromReductionVar inputReduceVariables
getInReductionVarStreams Reduce {..} =
  map buildDummyStreamFromReductionVar inputReduceVariables
getInReductionVarStreams _ = []

generatePipeReadsKernel :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeReadsKernel kernel =
  concatMap (generatePipeAccessKernel ReadPipe) $ DMap.elems combined
  where
    pipesMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) $
      readPipes kernel
    inputStreamsMap =
      DMap.fromList $
      map (\outputStream -> (getStreamName outputStream, outputStream)) $
      inputStreams kernel ++ getInReductionVarStreams kernel
    combined =
      DMap.intersectionWith (\pipe sci -> (pipe, sci)) pipesMap inputStreamsMap

data PipeAction
  = ReadPipe
  | WritePipe

generatePipeAccessKernel :: PipeAction -> (Pipe, Stream Anno) -> [Fortran Anno]
generatePipeAccessKernel ReadPipe (Pipe _ _ pipeName _ _, stream) =
  [call "read_pipe" [pipeName, getStreamName stream]]
generatePipeAccessKernel WritePipe (Pipe _ _ pipeName _ _, stream) =
  [call "write_pipe" [pipeName, getStreamName stream]]

getOutReductionVarStreams :: PipelineItem SharedPipelineData -> [Stream Anno]
getOutReductionVarStreams Reduce {..} =
  map buildDummyStreamFromReductionVar outputReduceVariables
getOutReductionVarStreams _ = []

generatePipeWritesKernel :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeWritesKernel kernel =
  concatMap (generatePipeAccessKernel WritePipe) $ DMap.elems combined
  where
    pipesMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) $
      writtenPipes kernel
    outputStreamsMap =
      DMap.fromList $
      map (\outputStream -> (getStreamName outputStream, outputStream)) $
      outputStreams kernel ++ getOutReductionVarStreams kernel
    combined =
      DMap.intersectionWith (\pipe sci -> (pipe, sci)) pipesMap outputStreamsMap
