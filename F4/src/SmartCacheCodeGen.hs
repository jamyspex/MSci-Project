{-# LANGUAGE RecordWildCards #-}

module SmartCacheCodeGen where

import           CodeGenUtils
import qualified Data.Map             as DMap
import           Data.Maybe
import           Data.String.Utils
import           Data.Tuple.Utils
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

generateSmartCaches :: [PipelineStage] -> IO ()
generateSmartCaches pipeline = mapM_ generateAndPrintSmartCache smartCaches
  where
    smartCaches = mapMaybe (\(_, smartCache, _) -> smartCache) pipeline

generateAndPrintSmartCache ::
     PipelineItem SharedPipelineData -> IO (ProgUnit Anno)
generateAndPrintSmartCache smc@SmartCache {..} = do
  putStrLn $ rule '~' ++ name ++ " " ++ rule '~'
  putStrLn $ miniPPProgUnit smartCache
  putStrLn $ rule '-'
  print kcd
  putStrLn $ rule '-'
  return smartCache
  where
    (smartCache, kcd) = generateSmartCache smc

generateSmartCache ::
     PipelineItem SharedPipelineData -> (ProgUnit Anno, KernelCallingData)
generateSmartCache smc@SmartCache {..} =
  (smartCache, KCD {subroutineName = "", kernelName = name, argPositions = []})
  where
    decls = generateDecls smc
    (body, extraDecls) = buildSmartCacheBody smc -- FIXME need to pass the driver loop bounds here
    smartCache = sub name (declNode (decls ++ extraDecls)) body []

toStringDecl :: [Decl Anno] -> String
toStringDecl decls =
  miniPPD $ buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) decls

toStringFortran :: [Fortran Anno] -> String
toStringFortran code =
  miniPPF $
  buildAstSeq (FSeq nullAnno nullSrcSpan) (NullStmt nullAnno nullSrcSpan) code

buildSmartCacheBody ::
     PipelineItem SharedPipelineData -> (Fortran Anno, [Decl Anno])
buildSmartCacheBody smc@SmartCache {..} =
  ( for
      mainLoopVarName
      (driverLoopLowerBound + 1)
      (var mainLoopBoundName)
      mainLoopBody
  , requiredDecls ++ controlDecls)
  where
    SPD {..} = sharedData
    mainLoopBody =
      buildAstSeq
        (FSeq nullAnno nullSrcSpan)
        (NullStmt nullAnno nullSrcSpan)
        ([compIdxCalc] ++ shiftLoopCode ++ [readBlock, writeBlock])
    compIdxCalc =
      assign
        (var compIndexName)
        (minus (var mainLoopVarName) (var maxPosOffsetParamName))
    pipeReads = generatePipeReads smc
    (shiftLoopCode, requiredDecls) =
      generateShiftLoop smc smartCacheSizeParamName
    pipeWrites = generatePipeWrites smc
    readBlock = ifLE mainLoopVarName (var driverLoopSizeName) (block pipeReads)
    writeBlock = ifGECon compIndexName 0 (block pipeWrites)
    controlDecls =
      [ intDecl mainLoopVarName
      , intDecl compIndexName
      , intParam mainLoopBoundName mainLoopBoundVal
      , intParam smartCacheSizeParamName smartCacheSize
      , intParam maxPosOffsetParamName maxPosOffset
      , intParam maxNegOffsetParamName maxNegOffset
      , intParam driverLoopSizeName driverLoopUpperBound
      ]
    mainLoopBoundVal = driverLoopUpperBound + maxPosOffset
    maxPosOffset = maximum (map maxPositiveOffset cacheLines)
    maxNegOffset = maximum (map maxNegativeOffset cacheLines)
    driverLoopSizeName = "driverLoopSize"
    compIndexName = "compIndex"
    mainLoopBoundName = "nloop"
    mainLoopVarName = "count"
    smartCacheSizeParamName = "smartCacheSize"
    maxPosOffsetParamName = "maxPositiveOffset"
    maxNegOffsetParamName = "maxNegativeOffset"

generateDecls :: PipelineItem SharedPipelineData -> [Decl Anno]
generateDecls smache@SmartCache {..} = readInDecls ++ bufferDecls ++ outVarDecls
  where
    readInDecls = generateReadInVars smache
    outVarDecls = concatMap generateOutVarDecls cacheLines
    bufferDecls = generateBufferDecls smache

-- generate something like this
-- call readPipe(wet_read_in, dyn_1_smart_cache__dyn_1__wet_j_k__pipe)
-- wet_buffer[smart cache size - 1] = wet_read_in
-- call memFence(CLK_CHANNEL_MEM_FENCE)
--
generatePipeReads :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeReads SmartCache {..} =
  concatMap
    (\(pipeName, arrayName) ->
       generatePipeReadCon
         assignmentIdx
         pipeName
         (arrayName ++ "_read_in")
         (arrayName ++ "_buffer"))
    toProcess
  where
    toProcess =
      map
        (\(Pipe _ _ name _ _, SmartCacheItem {..}) ->
           (name, getArrayNameFromStream inputStream)) $
      DMap.elems combined
    assignmentIdx = smartCacheSize
    pipesMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) readPipes
    smartCacheItemMap =
      DMap.fromList $
      map (\s@SmartCacheItem {..} -> (getStreamName inputStream, s)) cacheLines
    combined =
      DMap.intersectionWith
        (\pipe sci -> (pipe, sci))
        pipesMap
        smartCacheItemMap

generatePipeWriteBlock :: PipelineItem SharedPipelineData -> Fortran Anno
generatePipeWriteBlock smartCache = block $ generatePipeWrites smartCache

generatePipeWrites :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeWrites SmartCache {..} = fortran
  where
    fortran =
      concatMap
        (\(Pipe _ _ pipeName _ _, (arrayName, (streamName, bufIdx, _))) ->
           generatePipeWriteCon
             bufIdx
             streamName
             (arrayName ++ "_buffer")
             pipeName) $
      DMap.elems combined
    pipesMap =
      DMap.fromList $
      map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) writtenPipes
    smartCacheOutputStreamMap =
      DMap.fromList $
      concatMap
        (\SmartCacheItem {..} ->
           map
             (\t -> (fst3 t, (getArrayNameFromStream inputStream, t)))
             outputStreamNamesAndBufferIndex)
        cacheLines
    combined =
      DMap.intersectionWith
        (\pipe sci -> (pipe, sci))
        pipesMap
        smartCacheOutputStreamMap

generateShiftLoop ::
     PipelineItem SharedPipelineData -> String -> ([Fortran Anno], [Decl Anno])
generateShiftLoop SmartCache {..} smcSizeVariableName =
  ( [ pragma "unroll"
    , for loopVarName 1 (var smcSizeVariableName `minus` con 1) loopBody
    ]
  , [intDecl loopVarName])
  where
    generateShift :: SmartCacheItem -> Fortran Anno
    generateShift SmartCacheItem {..} =
      arrayVar (arrName ++ "_buffer") [var loopVarName] `assign`
      arrayVar (arrName ++ "_buffer") [var loopVarName `plus` con 1]
      where
        arrName = getArrayNameFromStream inputStream
    loopVarName = "i"
    shifts = map generateShift cacheLines
    loopBody =
      buildAstSeq
        (FSeq nullAnno nullSrcSpan)
        (NullStmt nullAnno nullSrcSpan)
        shifts

generateReadInVars :: PipelineItem SharedPipelineData -> [Decl Anno]
generateReadInVars SmartCache {..} = map generateReadInVar cacheLines
  where
    generateReadInVar :: SmartCacheItem -> Decl Anno
    generateReadInVar SmartCacheItem {..} =
      typedDecl readInName (getFortranTypeForStream inputStream)
      where
        readInName = arrayName ++ "_read_in"
        Stream _ arrayName streamValueType _ = inputStream

generateOutVarDecls :: SmartCacheItem -> [Decl Anno]
generateOutVarDecls SmartCacheItem {..} =
  map
    (\name -> typedDecl name (getFortranTypeForStream inputStream))
    outStreamNames
  where
    outStreamNames = map fst3 outputStreamNamesAndBufferIndex

generateBufferDecls :: PipelineItem SharedPipelineData -> [Decl Anno]
generateBufferDecls SmartCache {..} = map generateBufferDecl cacheLines
  where
    generateBufferDecl :: SmartCacheItem -> Decl Anno
    generateBufferDecl SmartCacheItem {..} =
      bufferDecl
        (arrayName ++ "_buffer")
        [(1, smartCacheSize)]
        (getFortranTypeForStream inputStream)
      where
        Stream _ arrayName streamValueType _ = inputStream
