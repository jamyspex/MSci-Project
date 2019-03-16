{-# LANGUAGE LambdaCase #-}

module BuildDeviceModule where

import           CodeGenUtils
import           FortranDSL
import           KernelCodeGen
import           Language.Fortran
import           LanguageFortranTools
import           MemoryAccessCodeGen
import           MiniPP
import           SmartCacheCodeGen
import           Utils

buildDeviceModule :: [PipelineStage] -> IO (ProgUnit Anno)
buildDeviceModule pipeline = do
  putStrLn $ rule '-' ++ " Pipe Init Sub " ++ rule '-'
  putStrLn $ miniPPProgUnit pipeInitSub
  return pipeInitSub
  where
    allPipes =
      concatMap
        (\(k, sm, ma) ->
           writtenPipes k ++
           concatMap writtenPipes sm ++ concatMap writtenPipes ma)
        pipeline
    pipeInitSub = generatePipeInitSubRoutine allPipes
    pipeDecls = map generatePipeDecl allPipes

pipeInitSubName = "pipe_initialisation"

generateStage :: PipelineStage -> ([KernelCallingData], [ProgUnit Anno])
generateStage (kernel, smartCache, memAccess) =
  ( []
  , memoryReaderKernels ++ smartCache ++ computeKernel ++ memoryWriterKernels)
  where
    computeKernel = generateKernelCode kernel
    smartCache = fmap generateSmartCache smartCache
    memoryReaderKernels =
      generateMemoryAccess $
      filter
        (\case
           MemoryReader {} -> True
           _ -> False)
        memAccess
    memoryWriterKernels =
      generateMemoryAccess $
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
