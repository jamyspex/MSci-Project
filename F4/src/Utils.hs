{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Utils where

import           Data.Generics
import           Data.List
import qualified Data.Map             as DMap
import           Data.Maybe
import           Debug.Trace
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Safe

synthIdxPrefix = "synthIdx"

scratchDirName = "scratch"

type SubNameStr = String

type SrcName = String

data SubRec = MkSubRec
  { subAst          :: ProgUnit Anno
  , subSrcFile      :: String
  , subSrcLines     :: [String]
  , subName         :: String
  , argTranslations :: ArgumentTranslationTable
  , parallelise     :: Bool
  }

data ArgumentTranslation = ArgTrans
  { parameter :: ArgName Anno
  , argument  :: VarName Anno
  } deriving (Show)

type ArgumentTranslationTable
   = DMap.Map SubNameStr (Fortran Anno, [ArgumentTranslation])

type SubroutineTable = DMap.Map SubNameStr SubRec

data SmartCacheDetailsForStream
  = SmartCacheDetailsForStream { requiredBufferSize    :: Int
                               , startIndex            :: [Int]
                               , endIndex              :: [Int]
                               , startToPointDistances :: [([Int], Int)]
                               , maxPosOffset          :: Int
                               , maxNegOffset          :: Int }
  | DummySmartCacheDetailsForStream { stream :: Stream Anno }

buildDummyStreamFromReductionVar :: String -> Stream Anno
buildDummyStreamFromReductionVar outputVarName =
  Stream outputVarName "" Float []

instance Show SmartCacheDetailsForStream where
  show smartCacheDetails =
    "Start index: " ++
    show startIndex ++
    "\n" ++
    "End index: " ++
    show endIndex ++
    "\n" ++
    "Max pos. offset = " ++
    show maxPosOffset ++
    " Max neg. offset = " ++
    show maxNegOffset ++
    "\n" ++
    "Buffer size: " ++
    show requiredBufferSize ++
    "\n" ++
    concatMap
      (\(point, index) ->
         "Stencil point: " ++
         show point ++ " buffer index = " ++ show index ++ "\n")
      startToPointDistances
    where
      SmartCacheDetailsForStream {..} = smartCacheDetails

data SmartCacheItem
  = SmartCacheItem { size                            :: Int
                   , inputStream                     :: Stream Anno
                   , maxPositiveOffset               :: Int
                   , maxNegativeOffset               :: Int
                   , outputStreamNamesAndBufferIndex :: [(String, Int)] }
  | SmartCacheTransitItem { inputStream :: Stream Anno
                          , size        :: Int }
  | DummySmartCacheItem { inputStream       :: Stream Anno
                        , outputStreamNames :: [String]
                        , size              :: Int }

instance Show SmartCacheItem where
  show DummySmartCacheItem {..} =
    "-------------------------------\n" ++
    "WARNING: This is a dummy smart cache item\n" ++
    "The compiler could not generate a smart\n" ++
    "cache item for this stream because it is\n" ++
    "accesed using a stencil with constant values!\n" ++
    "Input stream: " ++
    getStreamName inputStream ++
    "\nOutput streams: " ++
    concatMap (\name -> "\t" ++ name ++ "\n") outputStreamNames ++
    "-------------------------\n"
  show SmartCacheItem {..} =
    "-------------------------------\n" ++
    "Smart cache item\n" ++
    "Input stream: " ++
    name ++
    "\n" ++
    "Buffer size: " ++
    show size ++
    "\n" ++
    "Max positive offset = " ++
    show maxPositiveOffset ++
    "\n" ++
    "Max negative offset = " ++
    show maxNegativeOffset ++
    "\n" ++
    "Stream Dimensions: " ++
    show dims ++
    "\n" ++
    "Output Streams:\n" ++
    concatMap
      (\(name, bufIdx) ->
         "\t" ++ name ++ " from buffer index = " ++ show bufIdx ++ "\n")
      outputStreamNamesAndBufferIndex ++
    "-------------------------------\n"
    where
      (name, dims) =
        case inputStream of
          (Stream name _ _ dims)        -> (name, dims)
          (TransitStream name _ _ dims) -> (name, dims)

data Kernel = Kernel
  { inputs                 :: [Stream Anno]
  , outputs                :: [Stream Anno]
  , kernelName             :: String
  , driverLoopVariableName :: String
  , outputReductionVars    :: [String]
  , inputReductionVars     :: [String]
  , originalSubroutineName :: String
  , body                   :: ProgUnit Anno
  , order                  :: Int
  , loopVars               :: [String]
  } deriving (Data, Typeable)

instance Show Kernel where
  show kernel =
    " ! ==============================================\n" ++
    " ! Name: " ++
    name ++
    " Order: " ++
    show o ++
    "\n" ++
    " ! Input streams:\n" ++
    concatMap (\s -> " !\t" ++ printStream s ++ "\n") inS ++
    " ! Output streams:\n" ++
    concatMap (\s -> " !\t" ++ printStream s ++ "\n") outS ++
    " ! Input Reduction Variables:\n" ++
    concatMap (\r -> "! \t" ++ show r ++ "\n") inR ++
    " ! Output Reduction Variables:\n" ++
    concatMap (\r -> "! \t" ++ show r ++ "\n") outR ++
    " ! --------------------------------------------\n" ++
    miniPPProgUnit b ++ " ! ==============================================\n\n"
    where
      inS = inputs kernel
      outS = outputs kernel
      name = kernelName kernel
      outR = outputReductionVars kernel
      inR = inputReductionVars kernel
      b = body kernel
      o = order kernel

data Index
  = LoopVar String
  | Const Int
  deriving (Show, Eq, Ord)

data ArrayAccess = AA
  { arrName            :: String
  , indices            :: [Index]
  , declaredDimensions :: [(Int, Int)]
  , isLHS              :: Bool
  } deriving (Eq, Ord)

data NestingDirection
  = Normal
  | Reverse
  | Undefined
  | Either
  deriving (Show, Eq)

instance Show ArrayAccess where
  show AA {..} =
    "ArrayAccess: " ++ arrName ++ " indices = " ++ show indices ++ "\n"

detectNestingDirection :: [String] -> [ArrayAccess] -> NestingDirection
detectNestingDirection loopVarsInNestOrder arrayAccesses =
  if valid
    then firstNotEither
    else error "Index usage ordering not consistent"
  where
    allNestingDirections = map (checkOne loopVarsInNestOrder) arrayAccesses
    firstNotEither =
      head $
      filter
        (\case
           Either -> False
           _ -> True)
        allNestingDirections
    valid = all (\v -> v == firstNotEither || v == Either) allNestingDirections

checkOne :: [String] -> ArrayAccess -> NestingDirection
checkOne loopVars arrayAccess =
  case (forward, backward) of
    (True, True) -> Either
    (True, _) -> Normal
    (_, True) -> Reverse
    (False, False) ->
      error ("Can not detect loop nesting direction" ++ show arrayAccess)
  where
    forward = go 0 loopVars accessLoopVarsOnly
    backward = go 0 (reverse loopVars) accessLoopVarsOnly
    accessLoopVarsOnly =
      (concatMap
         (\case
            LoopVar name -> [name]
            Const _ -> []) .
       indices)
        arrayAccess
    go :: Int -> [String] -> [String] -> Bool
    go misMatchCount (lv:lvs) (ai:ais)
      | misMatchCount == 2 = False
      | lv == ai = go misMatchCount lvs ais
      | lv /= ai = go (misMatchCount + 1) lvs ais
      | otherwise = go misMatchCount lvs ais
    go misMatchCount _ _
      | misMatchCount == 2 = False
      | otherwise = True

getLoopNests :: Fortran Anno -> [Fortran Anno]
getLoopNests = go "" []
  where
    go :: String -> [Fortran Anno] -> Fortran Anno -> [Fortran Anno]
    go _ _ (OriginalSubContainer _ name body) = go name [] body
    go name preamble (FSeq _ _ f1 f2)
      | isLoop f1 = osc name (block (preamble ++ [f1])) : go name [] f2
      | isLoop f2 = [osc name (block $ preamble ++ [f1, f2])]
      | isOSC f1 = go "" [] f1 ++ go "" [] f2
      | isOSC f2 = go "" [] f2
      | isFSeq f2 = go name (preamble ++ [f1]) f2
      | otherwise = error "can't find main kernel body after preamble"
    go _ _ (NullStmt _ _) = []
    go name preamble fortran
      | isLoop fortran = [osc name (block $ preamble ++ [fortran])]
      | otherwise = error "encountered fortran that is not main body"

isOSC fortran =
  case fortran of
    OriginalSubContainer {} -> True
    _                       -> False

isLoop fortran =
  case fortran of
    For {} -> True
    _      -> False

osc = OriginalSubContainer nullAnno

isFSeq f =
  case f of
    FSeq {} -> True
    _       -> False

data Stream p
  = Stream String -- name
           String -- array name
           StreamValueType -- value type
           [(Int, Int)] -- dimensions
  | StencilStream String -- name
                  String -- array name
                  StreamValueType -- value type
                  [(Int, Int)] -- dimensions
                  (Stencil p) -- stencil offsets
  | TransitStream String -- name
                  String -- array name
                  StreamValueType -- value type
                  [(Int, Int)] -- dimensions
  deriving (Show, Typeable, Data)

instance Eq (Stream p) where
  (==) s1 s2 = getStreamName s1 == getStreamName s2

instance Ord (Stream p) where
  compare s1 s2 = getStreamName s1 `compare` getStreamName s2

convertStencilStream (StencilStream name arrayName valueType dims _) =
  Stream name arrayName valueType dims

isStencil stream =
  case stream of
    StencilStream {} -> True
    _                -> False

isTransit stream =
  case stream of
    TransitStream {} -> True
    _                -> False

-- Data type used to represent a processing pipeline
-- for output to a Fortran kernel file
data PipelineItem a
  = Map { inputStreams         :: [Stream Anno]
        , outputStreams        :: [Stream Anno]
        , name                 :: String
        , inputReduceVariables :: [String]
        , fortran              :: ProgUnit Anno
        , originalSubName      :: String
        , nextStage            :: PipelineItem a
        , stageNumber          :: Int
        , readPipes            :: [Pipe]
        , writtenPipes         :: [Pipe]
        , sharedData           :: a }
  | Reduce { inputStreams          :: [Stream Anno]
           , outputStreams         :: [Stream Anno]
           , name                  :: String
           , outputReduceVariables :: [String]
           , inputReduceVariables  :: [String]
           , fortran               :: ProgUnit Anno
           , originalSubName       :: String
           , nextStage             :: PipelineItem a
           , stageNumber           :: Int
           , readPipes             :: [Pipe]
           , writtenPipes          :: [Pipe]
           , sharedData            :: a }
  | SmartCache { inputStreams   :: [Stream Anno]
               , outputStreams  :: [Stream Anno]
               , name           :: String
               , smartCacheSize :: Int
               , cacheLines     :: [SmartCacheItem]
               , nextStage      :: PipelineItem a
               , readPipes      :: [Pipe]
               , writtenPipes   :: [Pipe]
               , sharedData     :: a }
  | MemoryReader { memToOutputStreams :: [(FPGAMemArray, Stream Anno)]
                 , nextStage          :: PipelineItem a
                 , name               :: String
                 , readPipes          :: [Pipe]
                 , writtenPipes       :: [Pipe]
                 , sharedData         :: a }
  | MemoryWriter { inputStreamsToMem :: [(Stream Anno, FPGAMemArray)]
                 , name              :: String
                 , readPipes         :: [Pipe]
                 , writtenPipes      :: [Pipe]
                 , sharedData        :: a }
  | NullItem

-- These two instances are simply here to set a preci
-- instance Eq (PipelineItem SharedPipelineData) where
--   (==) _ _ = True
--
-- instance Ord (PipelineItem SharedPipelineData) where
--   compare SmartCache{} _ = Prelude.GT
--   compare _ SmartCache{} = Prelude.LT
--   compare _ _ = Prelude.EQ
type PipelineStage
   = ( PipelineItem SharedPipelineData
     , Maybe (PipelineItem SharedPipelineData)
     , [PipelineItem SharedPipelineData])

data Pipe =
  Pipe String
       String
       String
       StreamValueType
       (Stream Anno)
  deriving (Show, Data, Typeable)

getPipeName :: Pipe -> String
getPipeName (Pipe _ _ name _ _) = name

getPipeSource :: Pipe -> String
getPipeSource (Pipe source _ _ _ _) = source

getPipeDest :: Pipe -> String
getPipeDest (Pipe _ dest _ _ _) = dest

getPipeStream :: Pipe -> Stream Anno
getPipeStream (Pipe _ _ _ _ stream) = stream

newtype DeviceModule = DeviceModule
  { kernels :: [PipelineItem SharedPipelineData]
  -- , pipes   :: [Pipe]
  }

printPipes :: [Pipe] -> String
printPipes = concatMap (\p -> getPipeName p ++ "\n")

instance Show (PipelineItem SharedPipelineData) where
  show Map {..} =
    rule '~' ++
    "This is a map kernel.\nName:" ++
    name ++
    hl ++
    "Input Streams:\n" ++
    printAllStreams inputStreams ++
    "OutputStreams:\n" ++
    printAllStreams outputStreams ++
    "Input reduction vars: \n" ++
    concatMap (\r -> "\t" ++ r ++ "\n") inputReduceVariables ++
    "readPipes:\n" ++
    printPipes readPipes ++
    "writtenPipes:\n" ++
    printPipes writtenPipes ++
    hl ++ miniPPProgUnit fortran ++ hl ++ show sharedData ++ rule '~'
  show Reduce {..} =
    rule '~' ++
    "This is a reduce kernel.\nName:" ++
    name ++
    hl ++
    "Input Streams:\n" ++
    printAllStreams inputStreams ++
    "Output Streams:\n" ++
    printAllStreams outputStreams ++
    "Input reduction vars: \n" ++
    concatMap (\r -> "\t" ++ r ++ "\n") inputReduceVariables ++
    "Output reduction vars: \n" ++
    concatMap (\r -> "\t" ++ r ++ "\n") outputReduceVariables ++
    "readPipes:\n" ++
    printPipes readPipes ++
    "writtenPipes:\n" ++
    printPipes writtenPipes ++
    hl ++ miniPPProgUnit fortran ++ hl ++ show sharedData ++ rule '~'
  show SmartCache {..} =
    rule '~' ++
    "This is a smart cache kernel.\nName: " ++
    name ++
    "\nSize: " ++
    show smartCacheSize ++
    "\nCache Lines:\n" ++
    concatMap
      (\cl -> (unlines . map ("\t" ++) . lines . show) cl ++ "\n")
      cacheLines ++
    "\n" ++
    "Input Streams:\n" ++
    printAllStreams inputStreams ++
    "Output Streams:\n" ++ printAllStreams outputStreams ++ rule '~'
  show MemoryReader {..} =
    rule '~' ++
    "This is a memory reader kernel.\nName: " ++
    name ++
    hl ++
    "Memory to streams:\n" ++
    concatMap
      (\(mem, stream) -> "\t" ++ arrayName mem ++ " --> " ++ show stream ++ "\n")
      memToOutputStreams ++
    rule '~'
  show MemoryWriter {..} =
    rule '~' ++
    "This is a memory writer kernel.\nName: " ++
    name ++
    hl ++
    "Streams to memory:\n" ++
    concatMap
      (\(stream, mem) -> "\t" ++ show stream ++ " --> " ++ arrayName mem ++ "\n")
      inputStreamsToMem ++
    rule '~'

printAllStreams = concatMap (\s -> "\t" ++ printStream s ++ "\n")

-- -- Data type used to represent a stream flowing between kernels
-- -- will likely directly map to a pipe in OpenCL
-- data Stream = Stream String StreamValueType deriving Show
-- data StreamValueType = Float deriving Show
data FPGAMemArray = FPGAMemArray
  { arrayName  :: String
  , dimensions :: [(Int, Int)]
  } deriving (Show)

data SharedPipelineData
  = SPD { driverLoopLowerBound    :: Int
        , driverLoopUpperBound    :: Int
        , driverLoopIndexName     :: String
        , largestStreamDimensions :: [(Int, Int)]
        , largestStreamName       :: String }
  | NullPipeLineData
  deriving (Show)

printStream (Stream name arrayName valueType dims) =
  "Stream: " ++
  name ++
  " array name: " ++
  arrayName ++ " type: " ++ show valueType ++ " dimensions: " ++ show dims
printStream (StencilStream name arrayName valueType dims stencil) =
  "StencilStream: " ++
  name ++
  " array name: " ++
  arrayName ++
  " type: " ++
  show valueType ++ " dimensions: " ++ show dims ++ showStencils "\t" [stencil]
printStream (TransitStream name arrayName valueType dims) =
  "TransitStream: " ++
  name ++
  " array name: " ++
  arrayName ++ " type: " ++ show valueType ++ " dimensions: " ++ show dims

data StreamValueType =
  Float
  deriving (Show, Data, Typeable)

getReductionVarNameQuery :: Fortran Anno -> [String]
getReductionVarNameQuery fortran =
  case fortran of
    OpenCLReduce _ _ _ _ _ _ redVar _ -> map getVarName redVar
    _                                 -> []
  where
    getVarName (varname, _) = getNameFromVarName varname

-- TODO if you find you need stuff later on that you had in Kernel
-- this is probably where you need to change
convertKernelToPipelineItem :: Kernel -> PipelineItem SharedPipelineData
convertKernelToPipelineItem k@Kernel {..} =
  case kernelType k of
    MapKernel ->
      Map
        { inputStreams = inputs
        , outputStreams = outputs
        , name = kernelName
        , fortran = body
        , inputReduceVariables = inputReductionVars
        , originalSubName = originalSubroutineName
        , nextStage = NullItem
        , stageNumber = order
        , readPipes = []
        , writtenPipes = []
        , sharedData =
            SPD
              { driverLoopLowerBound = 0
              , driverLoopUpperBound = 0
              , driverLoopIndexName = driverLoopVariableName
              , largestStreamDimensions = []
              , largestStreamName = ""
              }
        }
    ReduceKernel ->
      Reduce
        { inputStreams = inputs
        , outputStreams = outputs
        , outputReduceVariables = outputReductionVars
        , inputReduceVariables = inputReductionVars
        , name = kernelName
        , fortran = body
        , originalSubName = originalSubroutineName
        , nextStage = NullItem
        , stageNumber = order
        , readPipes = []
        , writtenPipes = []
        , sharedData =
            SPD
              { driverLoopLowerBound = 0
              , driverLoopUpperBound = 0
              , driverLoopIndexName = driverLoopVariableName
              , largestStreamDimensions = []
              , largestStreamName = ""
              }
        }

data KernelType
  = MapKernel
  | ReduceKernel

kernelType :: Kernel -> KernelType
kernelType kernel =
  if valid
    then case (openCLMapCount, openCLReduceCount) of
           (1, 0) -> MapKernel
           (0, 1) -> ReduceKernel
    else error "more than one map or fold in kernel"
  where
    valid =
      (openCLMapCount + openCLReduceCount == 1) &&
      openCLMapCount >= 0 && openCLReduceCount >= 0
    openCLMapCount = length $ everything (++) (mkQ [] mapQuery) (body kernel)
    openCLReduceCount =
      length $ everything (++) (mkQ [] reduceQuery) (body kernel)
    reduceQuery :: Fortran Anno -> [Fortran Anno]
    reduceQuery fortran =
      case fortran of
        r@OpenCLReduce {} -> [r]
        _                 -> []
    mapQuery :: Fortran Anno -> [Fortran Anno]
    mapQuery fortran =
      case fortran of
        m@OpenCLMap {} -> [m]
        _              -> []

removeDuplicates :: Ord a => (b -> a) -> [b] -> [b]
removeDuplicates getKey input = DMap.elems uniqueMap
  where
    pairsForMap = map (\item -> (getKey item, item)) input
    uniqueMap = foldr addToMap DMap.empty pairsForMap
    addToMap :: Ord a => (a, b) -> DMap.Map a b -> DMap.Map a b
    addToMap (key, val) = DMap.insert key val

-- validate that a large expression only contains certain allowed types of sub expression
validateExprListContents :: (Expr Anno -> [Bool]) -> Expr Anno -> Bool
validateExprListContents subExprQuery expr =
  and $ everything (++) (mkQ [] subExprQuery) expr

-- get index expr from array accesses
idxVarQuery :: Expr Anno -> [Expr Anno]
idxVarQuery (Var _ _ [(_, indices)]) = indices

getSubroutineBody :: SubRec -> Fortran Anno
getSubroutineBody subrec = getSubBody ast
  where
    ast = subAst subrec

getArgName (ArgName _ name) = name

getArgs :: ProgUnit Anno -> [ArgName Anno]
getArgs (Sub _ _ return _ args _) =
  if isJust return
    then error
           "Only subroutines with Nothing as their return type can be merged"
    else everything (++) (mkQ [] argNameQuery) args
  where
    argNameQuery :: ArgName Anno -> [ArgName Anno]
    argNameQuery input =
      case input of
        argname@(ArgName _ name) -> [argname]
        _                        -> []
getArgs _ = error "Passed something other than a Sub to getArgs"

getArgsAsString :: ProgUnit Anno -> [String]
getArgsAsString sub = map getArgName $ getArgs sub

gerArgsAsString = error "Passed something other than a Sub to getArgsAsString"

hl = rule '-'

rule char = "\n" ++ replicate 80 char ++ "\n"

getStreamDimensions (Stream _ _ _ dims)          = dims
getStreamDimensions (StencilStream _ _ _ dims _) = dims
getStreamDimensions (TransitStream _ _ _ dims)   = dims

getStreamName (Stream name _ _ _)          = name
getStreamName (StencilStream name _ _ _ _) = name
getStreamName (TransitStream name _ _ _)   = name

getStreamType (Stream _ _ valueType _)          = valueType
getStreamType (StencilStream _ _ valueType _ _) = valueType
getStreamType (TransitStream _ _ valueType _)   = valueType

getArrayNameFromStream (Stream _ arrayName _ _)          = arrayName
getArrayNameFromStream (StencilStream _ arrayName _ _ _) = arrayName
getArrayNameFromStream (TransitStream _ arrayName _ _)   = arrayName

getAttrs typeDecl =
  case typeDecl of
    (BaseType _ _ attrs _ _) -> attrs
    (ArrayT _ _ _ attrs _ _) -> attrs

getSubBody :: ProgUnit Anno -> Fortran Anno
getSubBody (Sub _ _ _ _ _ (Block _ _ _ _ _ fortran)) = fortran
getSubBody (Module _ _ _ _ _ _ progUnits) = head $ map getSubBody progUnits

getSubName (Sub _ _ _ (SubName _ name) _ _) = name

getDecls :: ProgUnit Anno -> [Decl Anno]
getDecls (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) =
  everything (++) (mkQ [] getDeclsQuery) decls
getDecls (Module _ _ _ _ _ _ progUnits) = concatMap getDecls progUnits

getDeclsQuery :: Decl Anno -> [Decl Anno]
getDeclsQuery decl =
  case decl of
    Decl {} -> [decl]
    _       -> []

getDeclNames :: ProgUnit Anno -> [String]
getDeclNames (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) =
  map (getNameFromVarName . getVarName) declStatements
  where
    declStatements = everything (++) (mkQ [] declNameQuery) decls

getAllVarNames expr = everything (++) (mkQ [] extractVarNamesFromExpr) expr

buildIndex loopVarName offset
  | offset > 0 = var loopVarName `plus` con (abs offset)
  | offset < 0 = var loopVarName `minus` con (abs offset)
  | offset == 0 = var loopVarName
  | otherwise = error "Cannot build array index"

getVarName (Var _ _ ((varname, _):_)) = varname

getVarNameG expr =
  headNote "getVarNameG: head of empty list" $
  everything (++) (mkQ [] extractVarNamesFromExpr) expr

getNameFromVarName (VarName _ name) = name

declNameAsString = getNameFromVarName . getVarName . head . declNameQuery

declNameQuery :: Decl Anno -> [Expr Anno]
declNameQuery decl =
  case decl of
    (Decl _ _ ((expr, _, _):_) _) -> [expr] --everything (++) (mkQ [] extractVarNamesFromExpr) expr
    _                             -> []

extractVarNamesFromExpr :: Expr Anno -> [VarName Anno]
extractVarNamesFromExpr expr =
  case expr of
    Var _ _ varnameList -> map fst varnameList
    _                   -> []

readIndex :: String -> Int
readIndex = round . read

data Array = Array
  { arrayVarName    :: VarName Anno
  , arrDimensions   :: Int
  , dimensionRanges :: [(Int, Int)]
  } deriving (Show)

arrayExprQuery :: [Array] -> Expr Anno -> [(Expr Anno, Array)]
arrayExprQuery arrays var@(Var _ _ ((VarName _ name, idx:_):_)) =
  case foundAt of
    Just idx -> [(var, arrays !! idx)] -- var | name `elem` arrayNames]
    _        -> []
  where
    foundAt =
      findIndex (\Array {..} -> getNameFromVarName arrayVarName == name) arrays
arrayExprQuery _ _ = []

getAllArrayAccessesWithMatchingArray ::
     [Array] -> Fortran Anno -> [(Expr Anno, Array)]
getAllArrayAccessesWithMatchingArray arrays fortran =
  concatMap (arrayExprQuery arrays) allVars
  where
    allVars = everything (++) (mkQ [] allVarsQuery) fortran

getAllArrayAccesses :: [Array] -> Fortran Anno -> [Expr Anno]
getAllArrayAccesses arrays fortran =
  map fst $ getAllArrayAccessesWithMatchingArray arrays fortran

allVarsQuery expr =
  case expr of
    v@Var {} -> [v]
    _        -> []

data ArrayAccessType
  = ArrayRead
  | ArrayWrite
  deriving (Show)

getArrayAccesses :: ArrayAccessType -> [Array] -> Fortran Anno -> [Expr Anno]
getArrayAccesses readOrWrite arrays fortran =
  map fst $ getArrayAccessesWithMatchedArray readOrWrite arrays fortran

getArrayAccessesWithMatchedArray ::
     ArrayAccessType -> [Array] -> Fortran Anno -> [(Expr Anno, Array)]
getArrayAccessesWithMatchedArray readOrWrite arrays fortran = allArrayExprs
  where
    allArrayExprs =
      everything (++) (mkQ [] (arrayExprQuery arrays)) exprsFromFortran
    exprsFromFortran =
      case fortran of
        (FSeq _ _ fst snd) -> recursiveCall fst ++ recursiveCall snd
        (OpenCLMap _ _ _ _ _ _ body) -> recursiveCall body
        (OpenCLReduce _ _ _ _ _ _ _ body) -> recursiveCall body
        (OpenCLStencil _ _ _ body) -> recursiveCall body
        _ ->
          case readOrWrite of
            ArrayRead ->
              case fortran of
                Assg _ _ _ rhs -> [rhs]
                For _ _ _ start bound incre body ->
                  start : bound : incre : recursiveCall body
                DoWhile _ _ bound body -> bound : recursiveCall body
                If _ _ cond branch elseIfs elseBranch ->
                  [cond] ++
                  recursiveCall branch ++
                  elseBranchResult ++
                  branchConds ++ concatMap recursiveCall branchBodys
                  where (branchConds, branchBodys) = unzip elseIfs
                        elseBranchResult =
                          case elseBranch of
                            (Just body) -> recursiveCall body
                            _           -> []
                _ -> []
            ArrayWrite ->
              case fortran of
                (Assg _ _ lhs _) -> [lhs]
                (For _ _ _ _ _ _ body) -> recursiveCall body
                (DoWhile _ _ _ body) -> recursiveCall body
                (If _ _ _ branch elseIfs elseBranch) ->
                  recursiveCall branch ++
                  elseBranchResult ++ concatMap recursiveCall branchBodys
                  where (_, branchBodys) = unzip elseIfs
                        elseBranchResult =
                          case elseBranch of
                            (Just body) -> recursiveCall body
                            _           -> []
                _ -> []
    recursiveCall = getArrayAccesses readOrWrite arrays

-- Array reads are basically anywhere but the left hand side of a assignment
-- so include all expression nodes from the fortran nodes EXCEPT the lhs of an
-- assigment
-- Array writes are only on the left hand side of an assignment
-- so only include the lhs of assignment nodes. Using the recursive call
-- to inspection all the Fortran node sub children
getArrayReads :: [Array] -> Fortran Anno -> [Expr Anno]
getArrayReads = getArrayAccesses ArrayRead

arrayFromDecl :: Decl Anno -> Array
arrayFromDecl = arrayFromDeclWithRanges False

getArrayDeclDimensions :: Decl Anno -> [(Int, Int)]
getArrayDeclDimensions (Decl _ _ _ typeDecl) =
  map getArrayDimensionConstants $ getArrayDimensions typeDecl

arrayFromDeclWithRanges :: Bool -> Decl Anno -> Array
arrayFromDeclWithRanges withRanges decl@(Decl _ _ _ typeDecl) =
  Array
    { arrayVarName = name
    , arrDimensions = numberOfDimensions
    , dimensionRanges =
        if withRanges
          then dimInts
          else []
    }
  where
    dimExprs = getArrayDimensions typeDecl
    dimInts = map getArrayDimensionConstants dimExprs
    numberOfDimensions = length $ getArrayDimensions typeDecl
    name = (getVarName . head . declNameQuery) decl

isTopLevelLoop :: Fortran Anno -> Bool
isTopLevelLoop fortran
  | loopBodyOnlyContainsLoop fortran = isTopLevelLoop (stripLoopNest fortran)
  | loopBodyStatementsOnly fortran = True
  | otherwise = False

getArrayDimensionConstants :: (Expr Anno, Expr Anno) -> (Int, Int)
getArrayDimensionConstants (expr1, expr2) =
  (getSingleConstant expr1, getSingleConstant expr2)

-- get an int from a an expr defining array dimensions
getSingleConstant :: Expr Anno -> Int
getSingleConstant expr =
  case expr of
    (Con _ _ val) -> read val :: Int
    (Unary _ _ _ (Con _ _ val)) -> negate $ read val :: Int
    (NullExpr _ _) -> 1 -- when array declared with starting index omitted, fortran defaults to 1
    expr ->
      error ("Expr other than constant in array dimensions. \n" ++ miniPP expr)

getArrayDimensions :: Type Anno -> [(Expr Anno, Expr Anno)]
getArrayDimensions declType =
  case declType of
    (ArrayT _ dimensions _ _ _ _) -> dimensions
    (BaseType _ _ attrs _ _) -> concat $ concatMap getDimensionAttrs attrs
  where
    getDimensionAttrs attr =
      case attr of
        (Dimension _ dimensions) -> [dimensions]
        _                        -> []

getArrayDecls :: ProgUnit Anno -> [Decl Anno]
getArrayDecls progUnit = arrayDecls
  where
    arrayDecls = filter isArrayDecl $ getDecls progUnit

getDeclType :: Decl Anno -> Type Anno
getDeclType (Decl _ _ _ typeDecl) = typeDecl

isArrayDecl :: Decl Anno -> Bool
isArrayDecl = not . null . getArrayDimensions . getDeclType

loopBodyStatementsOnly :: Fortran Anno -> Bool
loopBodyStatementsOnly fortran =
  case fortran of
    For {} -> False
    FSeq _ _ f1 f2 -> loopBodyStatementsOnly f1 && loopBodyStatementsOnly f2
    OriginalSubContainer _ _ body -> loopBodyStatementsOnly body
    _ -> True

loopBodyOnlyContainsLoop :: Fortran Anno -> Bool
loopBodyOnlyContainsLoop fortran =
  case fortran of
    For {}                      -> True
    FSeq _ _ For {} NullStmt {} -> True
    _                           -> False

stripLoopNest :: Fortran Anno -> Fortran Anno
stripLoopNest (OriginalSubContainer _ _ body) = stripLoopNest body
stripLoopNest (FSeq _ _ f1 NullStmt {}) = stripLoopNest f1
stripLoopNest (For _ _ _ _ _ _ body) = body
stripLoopNest body
  | loopBodyStatementsOnly body = body
  | otherwise = error ("Can't strip loop nest from: \n" ++ miniPPF body)

getBaseType (BaseType _ baseType _ _ _) = baseType
getBaseType (ArrayT _ _ baseType _ _ _) = baseType

getKind (BaseType _ _ _ kind _) = kind
getKind (ArrayT _ _ _ _ kind _) = kind

getLen (BaseType _ _ _ _ len) = len
getLen (ArrayT _ _ _ _ _ len) = len

debug_displaySubRoutineTable :: SubroutineTable -> Bool -> IO ()
debug_displaySubRoutineTable srt withAst =
  case withAst of
    False -> mapM_ (debug_displaySubTableEntry withAst) asList
    True  -> mapM_ (debug_displaySubTableEntry withAst) asList
  where
    asList = map (\(_, value) -> value) $ DMap.toList srt

debug_displaySubTableEntry :: Bool -> SubRec -> IO ()
debug_displaySubTableEntry showAst sr = do
  putStrLn $ hl
  putStrLn $ "Subroutine name: " ++ (subName sr)
  putStrLn $ "Filename: " ++ (subSrcFile sr)
  putStrLn $ "Source:"
  putStrLn $ miniPPProgUnit (subAst sr)
  if showAst
    then do
      putStrLn $ "AST: "
      putStrLn $ show (subAst sr)
    else putStrLn $ "AST not shown."
  putStrLn $ "Argument translations:"
  putStrLn $
    concatMap
      (\(subname, (callStatement, argTransList)) ->
         "\t" ++
         subname ++
         "->\n" ++
         "\t" ++
         miniPPF callStatement ++
         "\n" ++
         (concatMap (\argTrans -> "\t" ++ show argTrans ++ "\n") argTransList)) $
    DMap.toList (argTranslations sr)
  putStrLn
    (if (parallelise sr)
       then "This subroutine will be offloaded to the FPGA"
       else "This subroutine will not be offloaded to the FPGA")
  putStrLn $ hl ++ "\n"
  where
    hl = (take 80 $ repeat '=')

-- function takes a list of loop variables and an array access expr and then returns a list of tuples of
-- the form (array name, loop variable name, maybe (position loop var is used in), nothing if it is not used)
getLoopIndexPosition :: [String] -> Expr Anno -> [(String, String, Maybe Int)]
getLoopIndexPosition loopVarNames (Var _ _ ((VarName _ arrayName, indexList):_)) =
  map (\name -> (arrayName, name, getIndexPos name indexList 0)) loopVarNames
  where
    getIndexPos :: String -> [Expr Anno] -> Int -> Maybe Int
    getIndexPos loopVariableName [] _ = Nothing
    getIndexPos loopVariableName (idx:idxs) position
      | null $ getAllVarNames idx =
        getIndexPos loopVariableName idxs (position + 1)
      | (getNameFromVarName . getVarNameG) idx == loopVariableName =
        Just position
      | otherwise = getIndexPos loopVariableName idxs (position + 1)
