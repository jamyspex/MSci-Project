{-# LANGUAGE LambdaCase #-}

module BuildDeviceModule where

import           CodeGenUtils
import qualified Data.List.Utils      as LU
import           FortranDSL
import           KernelCodeGen
import           Language.Fortran
import           LanguageFortranTools
import           MemoryAccessCodeGen
import           MiniPP
import           SmartCacheCodeGen
import           Utils

buildDeviceModule ::
     [PipelineStage] -> IO (String, ProgUnit Anno, [KernelCallingData])
buildDeviceModule pipeline = do
  putStrLn $ rule '-' ++ " Pipe Init Sub " ++ rule '-'
  putStrLn $ miniPPProgUnit deviceModule
  return (moduleName, deviceModule, callingData)
  where
    deviceModule = fortranModule moduleName pipeDecls (pipeInitSub : kernels)
    kernels = concat kernelsSubLists
    callingData = concat kernelCallingDataPerKernel
    (kernelCallingDataPerKernel, kernelsSubLists) =
      unzip $ map generateStage pipeline
    allPipes =
      concatMap
        (\(k, sm, ma) ->
           writtenPipes k ++
           concatMap writtenPipes sm ++ concatMap writtenPipes ma)
        pipeline
    pipeInitSub = generatePipeInitSubRoutine allPipes
    pipeDecls = declNode $ map generatePipeDecl allPipes
    kernelsOnly = map (\(k, _, _) -> k) pipeline
    moduleName =
      LU.join "_" (LU.uniq $ map (head . LU.split "_" . name) kernelsOnly) ++
      "_device_code"

pipeInitSubName = "pipe_initialisation"

generateStage :: PipelineStage -> ([KernelCallingData], [ProgUnit Anno])
generateStage (kernel, smartCache, memAccess) =
  ( memWriteKCD ++ memReadKCD ++ smartCacheCallingData ++ [kernelCallingData]
  , memoryReaderKernels ++
    smartCacheKernel ++ [computeKernel] ++ memoryWriterKernels)
  where
    (computeKernel, kernelCallingData) = generateKernelCode kernel
    (smartCacheKernel, smartCacheCallingData) =
      maybe
        ([], [])
        (\s ->
           let (sm, kcd) = generateSmartCache s
            in ([sm], [kcd]))
        smartCache
    (memoryReaderKernels, memReadKCD) =
      unzip $
      map generateMemoryReader $
      filter
        (\case
           MemoryReader {} -> True
           _ -> False)
        memAccess
    (memoryWriterKernels, memWriteKCD) =
      unzip $
      map generateMemoryWriter $
      filter
        (\case
           MemoryWriter {} -> True
           _ -> False)
        memAccess

generatePipeDecl :: Pipe -> Decl Anno
generatePipeDecl (Pipe _ _ pipeName _ _) = intDecl pipeName

generatePipeInitSubRoutine :: [Pipe] -> ProgUnit Anno
generatePipeInitSubRoutine pipes = pipeInitSubroutine
  where
    pipeInitSubroutine =
      sub pipeInitSubName (NullDecl nullAnno nullSrcSpan) body []
    body = block $ map generatePipeInitCall pipes

generatePipeInitCall :: Pipe -> Fortran Anno
generatePipeInitCall (Pipe _ _ pipeName streamValueType _) =
  call (getCallName streamValueType) [pipeName]
  where
    getCallName streamValueType =
      case streamValueType of
        Float -> "ocl_pipe_real"
        _     -> "ocl_pipe_int"
