{-# LANGUAGE RecordWildCards #-}

module SmartCacheCodeGen where

import qualified Data.Map             as DMap
import           Data.Maybe
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

generateSmartCaches :: [PipelineStage] -> IO ()
generateSmartCaches pipeline = mapM_ generateSmartCache smartCaches
  where
    smartCaches = mapMaybe (\(_, smartCache, _) -> smartCache) pipeline

generateSmartCache :: PipelineItem SharedPipelineData -> IO (ProgUnit Anno)
generateSmartCache smc@SmartCache {..} = do
  putStrLn $ rule '~' ++ " Decls for " ++ name ++ " " ++ rule '~'
  putStrLn $ miniPPD (generateDecls smc)
  putStrLn $ rule '-'
  putStrLn $ miniPPF (generatePipeReadLoop smc)
  putStrLn $ rule '-'
  putStrLn $ miniPPF (generateShiftLoop smc)
  return (NullProg nullAnno nullSrcSpan)

generateDecls :: PipelineItem SharedPipelineData -> Decl Anno
generateDecls smache@SmartCache {..} =
  buildAstSeq
    (DSeq nullAnno)
    (NullDecl nullAnno nullSrcSpan)
    (readInDecls ++ bufferDecls ++ outVarDecls)
  where
    readInDecls = generateReadInVars smache
    outVarDecls = concatMap generateOutVarDecls cacheLines
    bufferDecls = generateBufferDecls smache

generateMainLoop :: PipelineItem SharedPipelineData -> Fortran Anno
generateMainLoop SmartCache {..} = NullStmt nullAnno nullSrcSpan

generatePipeReadLoop :: PipelineItem SharedPipelineData -> Fortran Anno
generatePipeReadLoop smartCache =
  buildAstSeq (FSeq nullAnno nullSrcSpan) (NullStmt nullAnno nullSrcSpan) $
  generatePipeReads smartCache

-- generate something like this
-- call readPipe(wet_read_in, dyn_1_smart_cache__dyn_1__wet_j_k__pipe)
-- wet_buffer[smart cache size - 1] = wet_read_in
-- call memFence(CLK_CHANNEL_MEM_FENCE)
--
generatePipeReads :: PipelineItem SharedPipelineData -> [Fortran Anno]
generatePipeReads SmartCache {..} =
  concatMap generatePipeRead $ DMap.elems combined
  where
    generatePipeRead :: (Pipe, SmartCacheItem) -> [Fortran Anno]
    generatePipeRead (Pipe pipeName _ _ _ _, SmartCacheItem {..}) =
      [ comment ("read pipe " ++ pipeName)
      , call "readPipe" [readInVar, pipeName]
      , assign
          (arrayVar (arrayName ++ "_buffer") [con assignmentIdx])
          (var readInVar)
      , call "memFence" ["CLK_CHANNEL_MEM_FENCE"]
      ]
      where
        readInVar = arrayName ++ "_read_in"
        (Stream streamName arrayName _ _) = inputStream
    assignmentIdx = smartCacheSize - 1
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

generateShiftLoop :: PipelineItem SharedPipelineData -> Fortran Anno
generateShiftLoop SmartCache {..} =
  for loopVarName 0 (smartCacheSize - 1) loopBody
  where
    generateShift :: SmartCacheItem -> Fortran Anno
    generateShift SmartCacheItem {..} =
      assign
        (arrayVar (arrName ++ "_buffer") [var loopVarName])
        (arrayVar (arrName ++ "_buffer") [plus (var loopVarName) (con 1)])
      where
        arrName = getArrayNameFromStream inputStream
    loopVarName = "i"
    shifts = map generateShift cacheLines
    loopBody =
      buildAstSeq
        (FSeq nullAnno nullSrcSpan)
        (NullStmt nullAnno nullSrcSpan)
        shifts

for loopVar initial limit =
  For
    nullAnno
    nullSrcSpan
    (varName loopVar)
    (con initial)
    (lessThan (var loopVar) (con limit))
    (con 1)

plus = Bin nullAnno nullSrcSpan (Plus nullAnno)

lessThan = Bin nullAnno nullSrcSpan (RelLT nullAnno)

assign = Assg nullAnno nullSrcSpan

varName = VarName nullAnno

argList args =
  ArgList nullAnno $
  buildAstSeq
    (ESeq nullAnno nullSrcSpan)
    (NullExpr nullAnno nullSrcSpan)
    argsAsVars
  where
    argsAsVars = map var args

call name args = Call nullAnno nullSrcSpan (var name) (argList args)

generateReadInVars :: PipelineItem SharedPipelineData -> [Decl Anno]
generateReadInVars SmartCache {..} = map generateReadInVar cacheLines
  where
    generateReadInVar :: SmartCacheItem -> Decl Anno
    generateReadInVar SmartCacheItem {..} =
      Decl nullAnno nullSrcSpan [(var readInName, nullExpr, Nothing)] readInType
      where
        readInName = arrayName ++ "_read_in"
        readInType =
          BaseType
            nullAnno
            (getFortranTypeForStream streamValueType)
            []
            nullExpr
            nullExpr
        Stream _ arrayName streamValueType _ = inputStream

generateOutVarDecls :: SmartCacheItem -> [Decl Anno]
generateOutVarDecls SmartCacheItem {..} =
  map
    (\name ->
       Decl nullAnno nullSrcSpan [(var name, nullExpr, Nothing)] streamType)
    outStreamNames
  where
    outStreamNames = map fst outputStreamNamesAndBufferIndex
    streamType =
      BaseType
        nullAnno
        (getFortranTypeForStream streamValueType)
        []
        nullExpr
        nullExpr
    Stream _ _ streamValueType _ = inputStream

generateBufferDecls :: PipelineItem SharedPipelineData -> [Decl Anno]
generateBufferDecls SmartCache {..} = map generateBufferDecl cacheLines
  where
    generateBufferDecl :: SmartCacheItem -> Decl Anno
    generateBufferDecl SmartCacheItem {..} =
      Decl nullAnno nullSrcSpan [(var bufName, nullExpr, Nothing)] bufType
      where
        bufName = arrayName ++ "_buffer"
        bufType =
          BaseType
            nullAnno
            (getFortranTypeForStream streamValueType)
            [Dimension nullAnno [(con 1, con smartCacheSize)]]
            nullExpr
            nullExpr
        Stream _ arrayName streamValueType _ = inputStream

comment text = TextStmt nullAnno nullSrcSpan ("! F4 Comment: " ++ text)

var name = arrayVar name []

arrayVar name indices = Var nullAnno nullSrcSpan [(varName name, indices)]

con val = Con nullAnno nullSrcSpan (show val)

nullExpr = NullExpr nullAnno nullSrcSpan

getFortranTypeForStream streamValueType =
  case streamValueType of
    Float -> Real nullAnno
    _     -> Integer nullAnno
