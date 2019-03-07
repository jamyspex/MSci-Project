{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}

module Utils where

import           Data.Generics
import qualified Data.Map                      as DMap
import           Data.Maybe
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP

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

data SmartCacheDetailsForStream = SmartCacheDetailsForStream
  { requiredBufferSize    :: Int
  , startIndex            :: [Int]
  , endIndex              :: [Int]
  , startToPointDistances :: [([Int], Int)]
  }

instance Show SmartCacheDetailsForStream where
  show smartCacheDetails =
    "Start index: " ++
    show startIndex ++
    "\n" ++
    "End index: " ++
    show endIndex ++
    "\n" ++
    "Buffer size: " ++
    show requiredBufferSize ++
    "\n" ++
    concatMap
      (\(index, point) ->
         "Stencil point: " ++
         show point ++ " buffer index = " ++ show index ++ "\n")
      startToPointDistances
    where
      SmartCacheDetailsForStream {..} = smartCacheDetails

data SmartCacheItem
  = SmartCacheItem { size                            :: Int
                   , inputStream                     :: Stream Anno
                   , outputStreamNamesAndBufferIndex :: [(String, Int)] }
  | SmartCacheTransitItem { inputStream :: Stream Anno
                          , size        :: Int }

instance Show SmartCacheItem where
  show SmartCacheItem {..} =
    "-------------------------------\n" ++
    "Smart cache item\n" ++
    "Input stream: " ++
    name ++
    "\n" ++
    "Buffer size: " ++
    show size ++
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
          (Stream name _ dims)        -> (name, dims)
          (TransitStream name _ dims) -> (name, dims)

data Kernel = Kernel
  { inputs              :: [Stream Anno]
  , outputs             :: [Stream Anno]
  , kernelName          :: String
  , outputReductionVars :: [String]
  , body                :: ProgUnit Anno
  , order               :: Int
  , loopVars            :: [String]
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
    " ! Output Reduction Variables:\n" ++
    concatMap (\r -> "! \t" ++ show r ++ "\n") outR ++
    " ! --------------------------------------------\n" ++
    miniPPProgUnit b ++ " ! ==============================================\n\n"
    where
      inS = inputs kernel
      outS = outputs kernel
      name = kernelName kernel
      outR = outputReductionVars kernel
      b = body kernel
      o = order kernel

data Stream p
  = Stream String -- name
           StreamValueType -- value type
           [(Int, Int)] -- dimensions
  | StencilStream String -- name
                  StreamValueType -- value type
                  [(Int, Int)] -- dimensions
                  (Stencil p) -- stencil offsets
  | TransitStream String -- name
                  StreamValueType -- value type
                  [(Int, Int)] -- dimensions
  deriving (Show, Typeable, Data)

instance Eq (Stream p) where
  (==) (Stream name1 _ _) (Stream name2 _ _) = name1 == name2

instance Ord (Stream p) where
  compare (Stream name1 _ _) (Stream name2 _ _) = name1 `compare` name2
 -- compare (TransitStream name1 _ _) (TransitStream name2 _ _) =
 --   name1 `compare` name2
 -- compare (Stream name1 _ _) (TransitStream name2 _ _) = name1 `compare` name2
 -- compare (TransitStream name1 _ _) (Stream name2 _ _) = name2 `compare` name2

convertStencilStream (StencilStream name valueType dims _) =
  Stream name valueType dims

isStencil stream = case stream of
  StencilStream{} -> True
  _               -> False

isTransit stream = case stream of
  TransitStream{} -> True
  _               -> False

-- Data type used to represent a processing pipeline
-- for output to a Fortran kernel file
data PipelineItem a
  = Map { inputStreams  :: [Stream Anno]
        , outputStreams :: [Stream Anno]
        , name          :: String
        , fortran       :: ProgUnit Anno
        , nextStage     :: PipelineItem a
        , stageNumber   :: Int
        , sharedData    :: a }
  | Reduce { inputStreams  :: [Stream Anno]
           , outputStreams :: [Stream Anno]
           , name          :: String
           , reductionVars :: [String]
           , fortran       :: ProgUnit Anno
           , nextStage     :: PipelineItem a
           , stageNumber   :: Int
           , sharedData    :: a }
  | SmartCache { inputStreams   :: [Stream Anno]
               , outputStreams  :: [Stream Anno]
               , name           :: String
               , smartCacheSize :: Int
               , cacheLines     :: [SmartCacheItem]
               , nextStage      :: PipelineItem a
               , sharedData     :: a }
  | MemoryReader { memToOutputStreams :: [(FPGAMemArray, Stream Anno)]
                 , nextStage          :: PipelineItem a
                 , name               :: String
                 , sharedData         :: a }
  | MemoryWriter { inputStreamsToMem :: [(Stream Anno, FPGAMemArray)]
                 , name              :: String
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
       StreamValueType
  deriving (Show, Data, Typeable)

data DeviceModule = DeviceModule
  { kernels :: [PipelineItem SharedPipelineData]
  , pipes   :: [Pipe]
  }

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
newtype FPGAMemArray = FPGAMemArray
  { arrayName :: String
  } deriving (Show)

data SharedPipelineData
  = SPD { driverLoopSize :: Int }
  | NullPipeLineData
  deriving (Show)

printStream (Stream name valueType dims) =
  "Stream: "
    ++ name
    ++ " type: "
    ++ show valueType
    ++ " dimensions: "
    ++ show dims
printStream (StencilStream name valueType dims stencil) =
  "StencilStream: "
    ++ name
    ++ " type: "
    ++ show valueType
    ++ " dimensions: "
    ++ show dims
    ++ showStencils "\t" [stencil]
printStream (TransitStream name valueType dims) =
  "TransitStream: "
    ++ name
    ++ " type: "
    ++ show valueType
    ++ " dimensions: "
    ++ show dims

data StreamValueType =
  Float
  deriving (Show, Data, Typeable)

-- TODO if you find you need stuff later on that you had in Kernel
-- this is probably where you need to change
convertKernelToPipelineItem :: Kernel -> PipelineItem SharedPipelineData
convertKernelToPipelineItem k@Kernel {..} = case kernelType k of
  MapKernel -> Map
    { inputStreams  = inputs
    , outputStreams = outputs
    , name          = kernelName
    , fortran       = body
    , nextStage     = NullItem
    , stageNumber   = order
    , sharedData    = NullPipeLineData
    }
  ReduceKernel -> Reduce
    { inputStreams  = inputs
    , outputStreams = outputs
    , reductionVars = outputReductionVars
    , name          = kernelName
    , fortran       = body
    , nextStage     = NullItem
    , stageNumber   = order
    , sharedData    = NullPipeLineData
    }

data KernelType
  = MapKernel
  | ReduceKernel

kernelType :: Kernel -> KernelType
kernelType kernel = if valid
  then case (openCLMapCount, openCLReduceCount) of
    (1, 0) -> MapKernel
    (0, 1) -> ReduceKernel
  else error "more than one map or fold in kernel"
 where
  valid =
    (openCLMapCount + openCLReduceCount == 1)
      && openCLMapCount
      >= 0
      && openCLReduceCount
      >= 0
  openCLMapCount = length $ everything (++) (mkQ [] mapQuery) (body kernel)
  openCLReduceCount =
    length $ everything (++) (mkQ [] reduceQuery) (body kernel)
  reduceQuery :: Fortran Anno -> [Fortran Anno]
  reduceQuery fortran = case fortran of
    r@OpenCLReduce{} -> [r]
    _                -> []
  mapQuery :: Fortran Anno -> [Fortran Anno]
  mapQuery fortran = case fortran of
    m@OpenCLMap{} -> [m]
    _             -> []

removeDuplicates :: Ord a => (b -> a) -> [b] -> [b]
removeDuplicates getKey input = DMap.elems uniqueMap
 where
  pairsForMap = map (\item -> (getKey item, item)) input
  uniqueMap   = foldr addToMap DMap.empty pairsForMap
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
getSubroutineBody subrec = getSubBody ast where ast = subAst subrec

getArgName (ArgName _ name) = name

getArgs :: ProgUnit Anno -> [ArgName Anno]
getArgs (Sub _ _ return _ args _) = if isJust return
  then error "Only subroutines with Nothing as their return type can be merged"
  else everything (++) (mkQ [] argNameQuery) args
 where
  argNameQuery :: ArgName Anno -> [ArgName Anno]
  argNameQuery input = case input of
    argname@(ArgName _ name) -> [argname]
    _                        -> []
getArgs _ = error "Passed something other than a Sub to getArgs"

getArgsAsString :: ProgUnit Anno -> [String]
getArgsAsString sub = map getArgName $ getArgs sub

gerArgsAsString = error "Passed something other than a Sub to getArgsAsString"

hl = rule '-'

rule char = "\n" ++ replicate 80 char ++ "\n"

getStreamDimensions (Stream _ _ dims         ) = dims
getStreamDimensions (StencilStream _ _ dims _) = dims
getStreamDimensions (TransitStream _ _ dims  ) = dims

getStreamName (Stream name _ _         ) = name
getStreamName (StencilStream name _ _ _) = name
getStreamName (TransitStream name _ _  ) = name

getAttrs typeDecl = case typeDecl of
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
getDeclsQuery decl = case decl of
  Decl{} -> [decl]
  _      -> []

getDeclNames :: ProgUnit Anno -> [String]
getDeclNames (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) = map
  (getNameFromVarName . getVarName)
  declStatements
  where declStatements = everything (++) (mkQ [] declNameQuery) decls

getAllVarNames expr = everything (++) (mkQ [] extractVarNamesFromExpr) expr

getVarName (Var _ _ ((varname, _) : _)) = varname

getVarNameG expr = head $ everything (++) (mkQ [] extractVarNamesFromExpr) expr

getNameFromVarName (VarName _ name) = name

declNameAsString = getNameFromVarName . getVarName . head . declNameQuery

declNameQuery :: Decl Anno -> [Expr Anno]
declNameQuery decl = case decl of
  (Decl _ _ ((expr, _, _) : _) _) -> [expr] --everything (++) (mkQ [] extractVarNamesFromExpr) expr
  _                               -> []

extractVarNamesFromExpr :: Expr Anno -> [VarName Anno]
extractVarNamesFromExpr expr = case expr of
  Var _ _ varnameList -> map fst varnameList
  _                   -> []

buildAstSeq :: (a -> a -> a) -> a -> [a] -> a
buildAstSeq _ nullNode []          = nullNode
buildAstSeq _ _        [statement] = statement
buildAstSeq constructor nullNode (statement : statements) =
  constructor statement (buildAstSeq constructor nullNode statements)

readIndex :: String -> Int
readIndex = round . read

data Array = Array
  { varName         :: VarName Anno
  , arrDimensions   :: Int
  , dimensionRanges :: [(Int, Int)]
  } deriving (Show)

arrayExprQuery :: [Array] -> Expr Anno -> [Expr Anno]
arrayExprQuery arrays expr = case expr of
  var@(Var _ _ ((VarName _ name, idx : _) : _)) ->
    [ var | name `elem` arrayNames ]
  _ -> []
 where
  arrayNames =
    map (\array -> let (VarName _ name) = varName array in name) arrays

getAllArrayAccesses :: [Array] -> Fortran Anno -> [Expr Anno]
getAllArrayAccesses arrays fortran = concatMap (arrayExprQuery arrays) allVars
  where allVars = everything (++) (mkQ [] allVarsQuery) fortran

allVarsQuery expr = case expr of
  v@Var{} -> [v]
  _       -> []

data ArrayAccess
  = ArrayRead
  | ArrayWrite
  deriving (Show)

getArrayAccesses :: ArrayAccess -> [Array] -> Fortran Anno -> [Expr Anno]
getArrayAccesses readOrWrite arrays fortran = allArrayExprs -- trace ("arrays length = " ++ ((show . length) arrays) ++ "\tallArrayExprs length = " ++ ((show . length) allArrayExprs) ++ "\n" ++ miniPPF fortran ++ "\n--------------------------") allArrayExprs
 where
  allArrayExprs =
    everything (++) (mkQ [] (arrayExprQuery arrays)) exprsFromFortran
  exprsFromFortran = case fortran of
    (FSeq _ _ fst snd) -> recursiveCall fst ++ recursiveCall snd
    (OpenCLMap _ _ _ _ _ _ body) -> recursiveCall body
    (OpenCLReduce _ _ _ _ _ _ _ body) -> recursiveCall body
    (OpenCLStencil _ _ _ body) -> recursiveCall body
    _ -> case readOrWrite of
      ArrayRead -> case fortran of
        Assg _ _ _ rhs -> [rhs]
        For _ _ _ start bound incre body ->
          start : bound : incre : recursiveCall body
        DoWhile _ _ bound body -> bound : recursiveCall body
        If _ _ cond branch elseIfs elseBranch ->
          [cond]
            ++ recursiveCall branch
            ++ elseBranchResult
            ++ branchConds
            ++ concatMap recursiveCall branchBodys
         where
          (branchConds, branchBodys) = unzip elseIfs
          elseBranchResult           = case elseBranch of
            (Just body) -> recursiveCall body
            _           -> []
        _ -> []
      ArrayWrite -> case fortran of
        (Assg _ _ lhs _      ) -> [lhs]
        (For _ _ _ _ _ _ body) -> recursiveCall body
        (DoWhile _ _ _ body  ) -> recursiveCall body
        (If _ _ _ branch elseIfs elseBranch) ->
          recursiveCall branch
            ++ elseBranchResult
            ++ concatMap recursiveCall branchBodys
         where
          (_, branchBodys) = unzip elseIfs
          elseBranchResult = case elseBranch of
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

arrayFromDeclWithRanges :: Bool -> Decl Anno -> Array
arrayFromDeclWithRanges withRanges decl@(Decl _ _ _ typeDecl) = Array
  { varName         = name
  , arrDimensions   = numberOfDimensions
  , dimensionRanges = if withRanges then dimInts else []
  }
 where
  dimExprs           = getArrayDimensions typeDecl
  dimInts            = map getArrayDimensionConstants dimExprs
  numberOfDimensions = length $ getArrayDimensions typeDecl
  name               = (getVarName . head . declNameQuery) decl

getArrayDimensionConstants :: (Expr Anno, Expr Anno) -> (Int, Int)
getArrayDimensionConstants (expr1, expr2) =
  (getSingleConstant expr1, getSingleConstant expr2)

-- get an int from a an expr defining array dimensions
getSingleConstant :: Expr Anno -> Int
getSingleConstant expr = case expr of
  (Con _ _ val              ) -> read val :: Int
  (Unary _ _ _ (Con _ _ val)) -> negate $ read val :: Int
  (NullExpr _ _             ) -> 1 -- when array declared with starting index omitted, fortran defaults to 1
  _ -> error "Expr other than constant in array dimensions. \n"

getArrayDimensions :: Type Anno -> [(Expr Anno, Expr Anno)]
getArrayDimensions declType = case declType of
  (ArrayT _ dimensions _ _ _ _) -> dimensions
  (BaseType _ _ attrs _ _     ) -> concat $ concatMap getDimensionAttrs attrs
 where
  getDimensionAttrs attr = case attr of
    (Dimension _ dimensions) -> [dimensions]
    _                        -> []

getArrayDecls :: ProgUnit Anno -> [Decl Anno]
getArrayDecls progUnit = arrayDecls
  where arrayDecls = filter isArrayDecl $ getDecls progUnit

getDeclType :: Decl Anno -> Type Anno
getDeclType (Decl _ _ _ typeDecl) = typeDecl

isArrayDecl :: Decl Anno -> Bool
isArrayDecl = not . null . getArrayDimensions . getDeclType

debug_displaySubRoutineTable :: SubroutineTable -> Bool -> IO ()
debug_displaySubRoutineTable srt withAst = case withAst of
  False -> mapM_ (debug_displaySubTableEntry withAst) asList
  True  -> mapM_ (debug_displaySubTableEntry withAst) asList
  where asList = map (\(_, value) -> value) $ DMap.toList srt

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
  putStrLn
    $ concatMap
        (\(subname, (callStatement, argTransList)) ->
          "\t"
            ++ subname
            ++ "->\n"
            ++ "\t"
            ++ miniPPF callStatement
            ++ "\n"
            ++ (concatMap (\argTrans -> "\t" ++ show argTrans ++ "\n")
                          argTransList
               )
        )
    $ DMap.toList (argTranslations sr)
  putStrLn
    (if (parallelise sr)
      then "This subroutine will be offloaded to the FPGA"
      else "This subroutine will not be offloaded to the FPGA"
    )
  putStrLn $ hl ++ "\n"
  where hl = (take 80 $ repeat '=')

-- null nodes useful AST construction
nullUseBlock = UseBlock (UseNil nullAnno) NoSrcLoc

-- function takes a list of loop variables and an array access expr and then returns a list of tuples of
-- the form (array name, loop variable name, maybe (position loop var is used in), nothing if it is not used)
getLoopIndexPosition :: [String] -> Expr Anno -> [(String, String, Maybe Int)]
getLoopIndexPosition varNames (Var _ _ ((VarName _ arrayName, indexList) : _))
  = map (\name -> (arrayName, name, getIndexPos name indexList 0)) varNames
 where
  getIndexPos :: String -> [Expr Anno] -> Int -> Maybe Int
  getIndexPos loopVariableName [] _ = Nothing
  getIndexPos loopVariableName (idx : idxs) position
    | null $ getAllVarNames idx = getIndexPos loopVariableName
                                              idxs
                                              (position + 1)
    | (getNameFromVarName . getVarNameG) idx == loopVariableName = Just position
    | otherwise = getIndexPos loopVariableName idxs (position + 1)
