{-# LANGUAGE RecordWildCards #-}

module SmartCacheCodeGen where

import qualified Data.Map                      as DMap
import           CodeGenUtils
import           Data.Maybe
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

generateSmartCaches :: [PipelineStage] -> IO ()
generateSmartCaches pipeline = mapM_ generateSmartCache smartCaches
  where smartCaches = mapMaybe (\(_, smartCache, _) -> smartCache) pipeline

generateSmartCache :: PipelineItem SharedPipelineData -> IO (ProgUnit Anno)
generateSmartCache smc@SmartCache {..} = do
  putStrLn $ rule '~' ++ name ++ " " ++ rule '~'
  putStrLn $ miniPPProgUnit smartCache
  putStrLn $ rule '-'
  return smartCache
 where
  decls              = generateDecls smc
  (body, extraDecls) = buildSmartCacheBody smc -- FIXME need to pass the driver loop bounds here
  smartCache         = sub name (declNode (decls ++ extraDecls)) body

toStringDecl :: [Decl Anno] -> String
toStringDecl decls =
  miniPPD $ buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) decls

toStringFortran :: [Fortran Anno] -> String
toStringFortran code = miniPPF $ buildAstSeq (FSeq nullAnno nullSrcSpan)
                                             (NullStmt nullAnno nullSrcSpan)
                                             code

buildSmartCacheBody
  :: PipelineItem SharedPipelineData -> (Fortran Anno, [Decl Anno])
buildSmartCacheBody smc@SmartCache {..} =
  ( for mainLoopVarName
        driverLoopLowerBound
        (var mainLoopBoundName)
        mainLoopBody
  , requiredDecls ++ controlDecls
  )
 where
  SPD {..}     = sharedData
  mainLoopBody = buildAstSeq
    (FSeq nullAnno nullSrcSpan)
    (NullStmt nullAnno nullSrcSpan)
    ([compIdxCalc] ++ shiftLoopCode ++ [readBlock, writeBlock])
  compIdxCalc = assign
    (var compIndexName)
    (minus (var mainLoopVarName) (var maxPosOffsetParamName))
  pipeReads = generatePipeReads smc
  (shiftLoopCode, requiredDecls) =
    generateShiftLoop smc smartCacheSizeParamName
  pipeWrites = generatePipeWrites smc
  readBlock =
    ifLT mainLoopVarName (var smartCacheSizeParamName) (block pipeReads)
  writeBlock = ifGECon compIndexName 0 (block pipeWrites)
  controlDecls =
    [ intDecl mainLoopVarName
    , intDecl compIndexName
    , intParam mainLoopBoundName       mainLoopBoundVal
    , intParam smartCacheSizeParamName smartCacheSize
    , intParam maxPosOffsetParamName   maxPosOffset
    , intParam maxNegOffsetParamName   maxNegOffset
    ]
  mainLoopBoundVal        = driverLoopUpperBound + maxPosOffset
  maxPosOffset            = maximum (map maxPositiveOffset cacheLines)
  maxNegOffset            = maximum (map maxNegativeOffset cacheLines)
  compIndexName           = "compIndex"
  mainLoopBoundName       = "nloop"
  mainLoopVarName         = "count"
  smartCacheSizeParamName = "smartCacheSize"
  maxPosOffsetParamName   = "maxPositiveOffset"
  maxNegOffsetParamName   = "maxNegativeOffset"

generateDecls :: PipelineItem SharedPipelineData -> [Decl Anno]
generateDecls smache@SmartCache {..} =
  readInDecls ++ bufferDecls ++ outVarDecls
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
generatePipeReads SmartCache {..} = concatMap
  (uncurry $ generatePipeRead assignmentIdx)
  toProcess
 where
  toProcess =
    map
        (\(Pipe _ _ name _ _, SmartCacheItem {..}) ->
          (name, getArrayNameFromStream inputStream)
        )
      $ DMap.elems combined
  assignmentIdx = smartCacheSize - 1
  pipesMap      = DMap.fromList
    $ map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) readPipes
  smartCacheItemMap = DMap.fromList $ map
    (\s@SmartCacheItem {..} -> (getStreamName inputStream, s))
    cacheLines
  combined = DMap.intersectionWith (\pipe sci -> (pipe, sci))
                                   pipesMap
                                   smartCacheItemMap

generatePipeWriteBlock :: PipelineItem SharedPipelineData -> Fortran Anno
generatePipeWriteBlock smartCache =
  buildAstSeq (FSeq nullAnno nullSrcSpan) (NullStmt nullAnno nullSrcSpan)
    $ generatePipeWrites smartCache

generatePipeWrites :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeWrites SmartCache {..} = fortran
 where
  fortran =
    concatMap
        (\(Pipe _ _ pipeName _ _, (arrayName, (streamName, bufIdx))) ->
          generatePipeWrite bufIdx streamName arrayName pipeName
        )
      $ DMap.elems combined
  pipesMap = DMap.fromList
    $ map (\p@(Pipe _ _ _ _ stream) -> (getStreamName stream, p)) writtenPipes
  smartCacheOutputStreamMap = DMap.fromList $ concatMap
    (\SmartCacheItem {..} -> map
      (\t -> (fst t, (getArrayNameFromStream inputStream, t)))
      outputStreamNamesAndBufferIndex
    )
    cacheLines
  combined = DMap.intersectionWith (\pipe sci -> (pipe, sci))
                                   pipesMap
                                   smartCacheOutputStreamMap

generateShiftLoop
  :: PipelineItem SharedPipelineData -> String -> ([Fortran Anno], [Decl Anno])
generateShiftLoop SmartCache {..} smcSizeVariableName =
  ( [ pragma "unroll"
    , for loopVarName 0 (minus (var smcSizeVariableName) (con 1)) loopBody
    ]
  , [intDecl loopVarName]
  )
 where
  generateShift :: SmartCacheItem -> Fortran Anno
  generateShift SmartCacheItem {..} = assign
    (arrayVar (arrName ++ "_buffer") [var loopVarName])
    (arrayVar (arrName ++ "_buffer") [plus (var loopVarName) (con 1)])
    where arrName = getArrayNameFromStream inputStream
  loopVarName = "i"
  shifts      = map generateShift cacheLines
  loopBody    = buildAstSeq (FSeq nullAnno nullSrcSpan)
                            (NullStmt nullAnno nullSrcSpan)
                            shifts

generateReadInVars :: PipelineItem SharedPipelineData -> [Decl Anno]
generateReadInVars SmartCache {..} = map generateReadInVar cacheLines
 where
  generateReadInVar :: SmartCacheItem -> Decl Anno
  generateReadInVar SmartCacheItem {..} = Decl
    nullAnno
    nullSrcSpan
    [(var readInName, nullExpr, Nothing)]
    readInType
   where
    readInName = arrayName ++ "_read_in"
    readInType = BaseType nullAnno
                          (getFortranTypeForStream streamValueType)
                          []
                          nullExpr
                          nullExpr
    Stream _ arrayName streamValueType _ = inputStream

generateOutVarDecls :: SmartCacheItem -> [Decl Anno]
generateOutVarDecls SmartCacheItem {..} = map
  (\name -> Decl nullAnno nullSrcSpan [(var name, nullExpr, Nothing)] streamType
  )
  outStreamNames
 where
  outStreamNames = map fst outputStreamNamesAndBufferIndex
  streamType     = BaseType nullAnno
                            (getFortranTypeForStream streamValueType)
                            []
                            nullExpr
                            nullExpr
  Stream _ _ streamValueType _ = inputStream

generateBufferDecls :: PipelineItem SharedPipelineData -> [Decl Anno]
generateBufferDecls SmartCache {..} = map generateBufferDecl cacheLines
 where
  generateBufferDecl :: SmartCacheItem -> Decl Anno
  generateBufferDecl SmartCacheItem {..} = Decl
    nullAnno
    nullSrcSpan
    [(var bufName, nullExpr, Nothing)]
    bufType
   where
    bufName = arrayName ++ "_buffer"
    bufType = BaseType nullAnno
                       (getFortranTypeForStream streamValueType)
                       [Dimension nullAnno [(con 1, con smartCacheSize)]]
                       nullExpr
                       nullExpr
    Stream _ arrayName streamValueType _ = inputStream

getFortranTypeForStream streamValueType = case streamValueType of
  Float -> Real nullAnno
  _     -> Integer nullAnno