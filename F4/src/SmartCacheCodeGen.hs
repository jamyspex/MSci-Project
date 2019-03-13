{-# LANGUAGE RecordWildCards #-}

module SmartCacheCodeGen where

import           FortranDSL
import qualified Data.Map                      as DMap
import           Data.Maybe
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

generateSmartCaches :: [PipelineStage] -> IO ()
generateSmartCaches pipeline = mapM_ generateSmartCache smartCaches
  where smartCaches = mapMaybe (\(_, smartCache, _) -> smartCache) pipeline

generateSmartCache :: PipelineItem SharedPipelineData -> IO (ProgUnit Anno)
generateSmartCache smc@SmartCache {..} = do
  putStrLn $ rule '~' ++ " Decls for " ++ name ++ " " ++ rule '~'
  putStrLn $ toStringDecl (generateDecls smc)
  putStrLn $ rule '-'
  putStrLn $ toStringFortran (generatePipeReads smc)
  putStrLn $ rule '-'
  putStrLn $ (toStringFortran . fst) (generateShiftLoop smc)
  putStrLn $ rule '-'
  putStrLn $ toStringFortran (generatePipeWrites smc)
  return (NullProg nullAnno nullSrcSpan)

toStringDecl :: [Decl Anno] -> String
toStringDecl decls =
  miniPPD $ buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) decls


toStringFortran :: [Fortran Anno] -> String
toStringFortran code = miniPPF $ buildAstSeq (FSeq nullAnno nullSrcSpan)
                                             (NullStmt nullAnno nullSrcSpan)
                                             code

generateSmartCacheControlDecls :: PipelineItem SharedPipelineData -> [Decl Anno]
generateSmartCacheControlDecls SmartCache {..} =
  [intDecl "count", intDecl "compIndex", intParam "nloop" nloopVal]
  where nloopVal = smartCacheSize + maximum (map maxPositiveOffset cacheLines)

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

generatePipeWrite :: Int -> String -> String -> String -> [Fortran Anno]
generatePipeWrite sourceIdx variableName arrayName pipeName =
  [ comment ("write pipe " ++ pipeName)
  , assign (var variableName) (arrayVar bufferName [con sourceIdx])
  , call "writePipe" [pipeName, variableName]
  , call "memFence"  ["CLK_CHANNEL_MEM_FENCE"]
  ]
  where bufferName = arrayName ++ "_buffer"

generatePipeRead :: Int -> String -> String -> [Fortran Anno]
generatePipeRead assignmentIdx pipeName streamName =
  [ comment ("read pipe " ++ pipeName)
  , call "readPipe" [pipeName, readInVar]
  , assign (arrayVar buffer [con assignmentIdx]) (var readInVar)
  , call "memFence" ["CLK_CHANNEL_MEM_FENCE"]
  ]
 where
  readInVar = streamName ++ "_read_in"
  buffer    = streamName ++ "_buffer"

generateShiftLoop
  :: PipelineItem SharedPipelineData -> ([Fortran Anno], [Decl Anno])
generateShiftLoop SmartCache {..} =
  ( [pragma "unroll", for loopVarName 0 (smartCacheSize - 1) loopBody]
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
