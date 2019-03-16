{-# LANGUAGE RecordWildCards #-}

module KernelCodeGen where

import           CodeGenUtils
import           Data.Generics
import           Data.List.Index
import qualified Data.Map             as DMap
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
        }
    loopVarName = driverLoopIndexName sharedData
    driverLoopBoundVarName = "nloop"
    mainLoop =
      for
        loopVarName
        (driverLoopLowerBound sharedData + 1)
        (var driverLoopBoundVarName)
        (block [pipeReads, kernelBodyWithoutAnnos, pipeWrites])
    pipeReads = block $ generatePipeReadsMapKernel mapKern
    pipeWrites = block $ generatePipeWritesMapKernel mapKern
    kernelBodyWithoutAnnos = stripOpenCLAnnos fortran
    decls =
      declNode $
      intParam driverLoopBoundVarName (driverLoopUpperBound sharedData) :
      intDecl loopVarName : getDecls fortran
    args = getArgs fortran
    loopVars = getLoopVarNames fortran
    loopVarArgsRemoved =
      filter (\(ArgName _ name) -> name `notElem` loopVars) args
generateKernelCode Reduce {..} = error "Reduce code gen not yet implemented"

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

generatePipeReadsMapKernel :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeReadsMapKernel Map {..} =
  concatMap (generatePipeAccessKernel "read") $ DMap.elems combined
  where
    pipesMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) readPipes
    outputStreamsMap =
      DMap.fromList $
      map
        (\outputStream -> (getStreamName outputStream, outputStream))
        inputStreams
    combined =
      DMap.intersectionWith (\pipe sci -> (pipe, sci)) pipesMap outputStreamsMap

generatePipeAccessKernel :: String -> (Pipe, Stream Anno) -> [Fortran Anno]
generatePipeAccessKernel action (Pipe _ _ pipeName _ _, stream) =
  [call (action ++ "Pipe") [pipeName, getStreamName stream]]

generatePipeWritesMapKernel :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeWritesMapKernel Map {..} =
  concatMap (generatePipeAccessKernel "write") $ DMap.elems combined
  where
    pipesMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) writtenPipes
    outputStreamsMap =
      DMap.fromList $
      map
        (\outputStream -> (getStreamName outputStream, outputStream))
        outputStreams
    combined =
      DMap.intersectionWith (\pipe sci -> (pipe, sci)) pipesMap outputStreamsMap
