{-# Language RecordWildCards #-}
module KernelExtraction where

import           Data.Generics
import           Data.List
import           Data.List.Index
import qualified Data.Map                      as DMap
import           Data.Ord
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils

-- Function goes through the merged subroutine and extracts kernel subroutines
-- for each map/fold returns a module containing all the appropriate subroutines
getKernels :: SubRec -> IO [Kernel]
getKernels subrec = do
  mapM_
    (\(_, originalName, b) -> putStrLn
      (  "\n--------------------\n"
      ++ originalName
      ++ "\n--------------------\n"
      ++ miniPPProgUnit b
      ++ "\n======================\n"
      )
    )
    kernelSubsAndOrder
  putStrLn $ "no. of kernels: " ++ (show . length) kernelSubsAndOrder
  putStrLn $ concatMap show kernels
  return kernels
 where
  allDecls               = getDecls $ subAst subrec
  allArgs                = getArgs $ subAst subrec
  subbody                = getSubroutineBody subrec
  allOldSubNameAndBodies = everything (++) (mkQ [] getOldSubsQuery) subbody
  kernelBodies           = map (getKernelsQuery False) allOldSubNameAndBodies
  globablyOrdered = indexed $ concatMap prepareForKernelBuilder kernelBodies
  curriedKernelBuilder   = makeKernelSub allDecls allArgs
  kernelSubsAndOrder     = map
    (\(globalOrder, builderInput) ->
      curriedKernelBuilder builderInput globalOrder
    )
    globablyOrdered
  kernels = map buildKernel kernelSubsAndOrder

-- strip of the OriginalSubContainer wrapper from the kernels and produce
-- tuples of (body, (name, maybe splitNum)) splitNum = what part of the
-- original subroutine the new kernel is made up off. If the original subroutine
-- wasn't split up then it is Nothing
prepareForKernelBuilder
  :: [Fortran Anno] -> [(Fortran Anno, (String, Maybe Int))]
prepareForKernelBuilder kernels
  | length kernels > 1 = imap (\i b -> prepareOne (Just i) b) kernels
  | otherwise          = [prepareOne Nothing $ head kernels]
 where
  prepareOne :: Maybe Int -> Fortran Anno -> (Fortran Anno, (String, Maybe Int))
  prepareOne splitNum (OriginalSubContainer _ name body) =
    (body, (name, splitNum))
  prepareOne _ _ = error "Kernel body not wrapped by OriginalSubContainer!"

-- Pick out at old subroutine boundaries so we can attach the original
-- name to the new kernels for nicer naming
getOldSubsQuery :: Fortran Anno -> [(String, Fortran Anno)]
getOldSubsQuery fortran = case fortran of
  (OriginalSubContainer _ name body) -> [(name, body)]
  _ -> []

-- form new valid subroutines based of the kernels
-- e.g. insert all the appropriate decls and arguments
-- and give them a name. Arguments:
-- (function body, (Original name, Maybe sub split number)) ->
-- globalOrderNumber -> all decls -> all args -> (global order, original subname, kernel sub)
makeKernelSub
  :: [Decl Anno]
  -> [ArgName Anno]
  -> (Fortran Anno, (String, Maybe Int))
  -> Int
  -> (Int, String, ProgUnit Anno)
makeKernelSub decls args (body, (originalName, splitNum)) globalOrder =
  ( globalOrder
  , originalName
  , Sub nullAnno nullSrcSpan Nothing name argList block
  )
 where
  argsComparable     = getComparableItems args getArgName
  declComparable     = getComparableItems decls declNameAsString
  allVarsNamesInBody = map getNameFromVarName $ getAllVarNames body
  subRequiredArgs    = map snd $ filter
    (\(argName, _) -> argName `elem` allVarsNamesInBody)
    argsComparable
  subRequiredDecls = map snd $ filter
    (\(declName, _) -> declName `elem` allVarsNamesInBody)
    declComparable
  argList = Arg
    nullAnno
    (buildAstSeq (ASeq nullAnno) (NullArg nullAnno) subRequiredArgs)
    nullSrcSpan
  declList =
    buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) subRequiredDecls
  nameSuffix = case splitNum of
    Nothing  -> ""
    Just val -> "_" ++ show val
  name  = SubName nullAnno (originalName ++ nameSuffix)
  impl  = ImplicitNull nullAnno
  block = Block nullAnno nullUseBlock impl nullSrcSpan declList body

-- Get list of tuples of (comparable key, the orignal item) to then
-- be used with filter and elem. Useful for arg and decl items
-- when deciding when to include them kernel subroutine or not.
getComparableItems :: Eq b => [a] -> (a -> b) -> [(b, a)]
getComparableItems items getKey = map (\i -> (getKey i, i)) items

-- custom traversal to make sure we don't get duplicate kernels in output
-- e.g. make sure if we take an OpenCLStencil node don't also include its
-- OpenCLMap/Reduce body for conversion to a kernel subroutine
getKernelsQuery :: Bool -> (String, Fortran Anno) -> [Fortran Anno]
getKernelsQuery False (name, body) = case body of
  (FSeq _ _ f1 f2) ->
    getKernelBody name f1 ++ getKernelBody name f2 ++ getKernelsQuery
      True
      (name, f2)
  _ -> getKernelBody name body
getKernelsQuery True (name, body) = case body of
  (FSeq _ _ f1 f2) ->
    getKernelBody name f1 ++ getKernelBody name f2 ++ getKernelsQuery
      True
      (name, f2)
  _ -> []

-- A kernel is defined as a OpenCLMap/Reduce/Stencil at the top level of a block
-- OriginalSubContainer is used to encapslate the newly split kernels so the original
-- sub name can be used when naming the kernel.
getKernelBody :: String -> Fortran Anno -> [Fortran Anno]
getKernelBody name fortran = case fortran of
  map@OpenCLMap{}         -> [OriginalSubContainer nullAnno name map]
  reduce@OpenCLReduce{}   -> [OriginalSubContainer nullAnno name reduce]
  stencil@OpenCLStencil{} -> [OriginalSubContainer nullAnno name stencil]
  _                       -> []

data ParsedArrayExpr = PAE { arrName :: String , loopVarsOrdered :: [String] }

parseArrayExpr :: Expr Anno -> ParsedArrayExpr
parseArrayExpr (Var _ _ ((VarName _ arrayName, indexList) : _)) = PAE
  { arrName         = arrayName
  , loopVarsOrdered = loopVars
  }
 where
  loopVars =
    map (\expr -> trace ("expr = " ++ miniPP expr) (getLoopVarName expr))
      $ filter (not . isConstant) indexList

getLoopVarName :: Expr Anno -> String
getLoopVarName t@(Bin _ _ (Plus _) loopVar (Con _ _ offset)) =
  (getNameFromVarName . getVarName) loopVar
getLoopVarName t@(Bin _ _ (Minus _) loopVar (Con _ _ offset)) =
  (getNameFromVarName . getVarName) loopVar
getLoopVarName loopVar = (getNameFromVarName . getVarName) loopVar

isConstant expr = case expr of
  Con{} -> True
  _     -> False

joinPAEAndArray :: [ParsedArrayExpr] -> [Array] -> [(Array, ParsedArrayExpr)]
joinPAEAndArray paes arrays = DMap.elems
  $ DMap.intersectionWith (\array pae -> (array, pae)) arraysMap paeMap
 where
  paeMap    = DMap.fromList $ map (\pae -> (arrName pae, pae)) paes
  arraysMap = DMap.fromList $ map
    (\array -> ((getNameFromVarName . arrayVarName) array, array))
    arrays

-- Analyse the kernel and deteremine what streams it needs as input
-- and what streams it produces as output. This is based on array accesses
-- and stencil accesses.
-- At this point we know two things for sure:
-- 1) usesIndexVariablesAndConstantOffset in StencilDetection.hs ensures
-- that the stencils only use the loop variables then +/- an offset
-- 2) validateIndexingAndMakeUnique in AddKernelLoopGuards.hs ensures that
-- the loop variables are used in a consistent order across a given array
-- These things combined give us some faith that any streams we generate will be
-- "coherent" don't quite know what that means at this point but definitely is important
-- The only thing left to check is that all the array writes use only loop vars the
-- mechanics of usesIndexVariablesAndConstantOffset can be used to do this. The function
-- validateExprListContents in Utils.hs can be used. Along with a check that the variables
-- used to index the array are the loop variables.
buildKernel :: (Int, String, ProgUnit Anno) -> Kernel
buildKernel (order, originalName, sub) = if arrayWritesValid
  then kernel
  else error "Array write invalid"
 where
  subBody           = getSubBody sub
  allDecls          = getArrayDecls sub
  reductionVars     = everything (++) (mkQ [] getReductionVarNameQuery) subBody
  allArrays         = map (arrayFromDeclWithRanges True) allDecls
  arrayReadExprs    = getArrayAccesses ArrayRead allArrays subBody
  readArrayNames    = map (getNameFromVarName . getVarName) arrayReadExprs
  arrayReads        = filter (filterAllArrays readArrayNames) allArrays
  arrayWriteExprs   = getArrayAccesses ArrayWrite allArrays subBody
  writtenArrayNames = map (getNameFromVarName . getVarName) arrayWriteExprs
  arrayWrites       = filter (filterAllArrays writtenArrayNames) allArrays
  stencils          = everything (++) (mkQ [] getStencilsQuery) subBody
  loopVariables     = everything (++) (mkQ [] getLoopVarNames) subBody
  arrayWritesValid  = validateArrayWrites loopVariables arrayWriteExprs
  (stencilArrays, arraysNoStencils) =
    matchArraysToStencils parsedReadAndArray stencils
  parsedArrayReads     = map parseArrayExpr arrayReadExprs
  parsedReadAndArray   = joinPAEAndArray parsedArrayReads arrayReads
  inputStreams = getInputStreams stencilArrays arraysNoStencils loopVariables
  parsedArrayWrites    = map parseArrayExpr arrayWriteExprs
  parsedWriteAndArrays = joinPAEAndArray parsedArrayWrites arrayWrites
  outputStreams        = map
    (\(array, PAE {..}) -> arrayToStream loopVarsOrdered array)
    parsedWriteAndArrays
  kernel = Kernel
    { inputs                 = inputStreams
    , outputs                = outputStreams
    , outputReductionVars    = reductionVars
    , body                   = sub
    , order                  = order
    , originalSubroutineName = originalName
    , driverLoopVariableName = ""
    , loopVars               = loopVariables
    , kernelName             = getSubName sub
    }

-- used for filtering all array declarations to writes/reads
filterAllArrays :: [String] -> Array -> Bool
filterAllArrays wanted candidate = name `elem` wanted
  where (VarName _ name) = arrayVarName candidate

arrayToStream :: [String] -> Array -> Stream Anno
arrayToStream loopVars array = Stream (buildStreamName array loopVars)
                                      (getArrayName array)
                                      Float
                                      (dims array)

getArrayName = getNameFromVarName . arrayVarName

dims = dimensionRanges

buildStreamName array loopVars =
  getArrayName array ++ concatMap ("_" ++) loopVars

getInputStreams
  :: [(Array, ParsedArrayExpr, [Stencil Anno])]
  -> [(Array, ParsedArrayExpr)]
  -> [String]
  -> [Stream Anno]
getInputStreams requiredStencils requiredArrays loopVars =
  stencilStreams ++ arrayStreams
 where
  arrayStreams = map
    (\(array, PAE {..}) -> arrayToStream loopVarsOrdered array)
    requiredArrays
  stencilStreams = concatMap getStencilStreams requiredStencils
  getStencilStreams (array, PAE {..}, stencils) = map curriedCon stencils
   where
    curriedCon = StencilStream (buildStreamName array loopVarsOrdered)
                               (getArrayName array)
                               Float
                               (dims array)

stencilArrayName (Stencil _ _ _ _ (VarName _ name)) = name

-- match stencils to arrays and then return two lists
-- ([(array, [stencils])], [arrays not accessed with stencils])
-- build a two maps one of name -> array and another of name -> [stencil]
-- then take the union of these to produce a name -> (array, [stencil]) map
-- then return the elems of that combined map
matchArraysToStencils
  :: [(Array, ParsedArrayExpr)]
  -> [Stencil Anno]
  -> ( [(Array, ParsedArrayExpr, [Stencil Anno])]
     , [(Array, ParsedArrayExpr)]
     )
matchArraysToStencils arraysAndParsedExprs stencils =
  (DMap.elems stencilArrayMap, DMap.elems arrayOnlyMap)
 where
  stencilsGrouped = groupBy
    (\s1 s2 -> stencilArrayName s1 == stencilArrayName s2)
    stencilsSorted
  stencilsSorted = sortOn stencilArrayName stencils
  arrayMap       = DMap.fromList $ map
    (\a -> ((getNameFromVarName . arrayVarName . fst) a, a))
    arraysAndParsedExprs
  stencilMap = DMap.fromList
    $ map (\grp -> ((stencilArrayName . head) grp, grp)) stencilsGrouped
  stencilArrayMap = DMap.intersectionWith
    (\(array, parsedExpr) stencil -> (array, parsedExpr, stencil))
    arrayMap
    stencilMap
  arrayOnlyMap = DMap.difference arrayMap stencilMap

-- check the array writes index expr are only Con/loop vars
validateArrayWrites :: [String] -> [Expr Anno] -> Bool
validateArrayWrites loopVars arrayWrites = simpleExprOnly && onlyUsesLoopVars
 where
  allIndexExprs = concatMap idxVarQuery arrayWrites
  simpleExprResults =
    map (validateExprListContents arrayWritesUseLoopVarsOrConOnly) allIndexExprs
  simpleExprOnly = and simpleExprResults
  allUsedIndexVarNames =
    map getNameFromVarName $ concatMap extractVarNamesFromExpr allIndexExprs
  onlyUsesLoopVars = all (`elem` loopVars) allUsedIndexVarNames

arrayWritesUseLoopVarsOrConOnly :: Expr Anno -> [Bool]
arrayWritesUseLoopVarsOrConOnly expr = case expr of
  Con{} -> [True]
  Var{} -> [True]
  _     -> [False]

getStencilsQuery :: Fortran Anno -> [Stencil Anno]
getStencilsQuery fortran = case fortran of
  (OpenCLStencil _ _ stencils _) -> stencils
  _                              -> []

getLoopVarNames :: Fortran Anno -> [String]
getLoopVarNames subBody = case subBody of
  OpenCLMap _ _ _ _ loopVars _ _ -> map getVarName loopVars
  OpenCLReduce _ _ _ _ loopVars _ _ _ -> map getVarName loopVars
  _                              -> []
  where getVarName (varname, _, _, _) = getNameFromVarName varname

getReductionVarNameQuery :: Fortran Anno -> [String]
getReductionVarNameQuery fortran = case fortran of
  OpenCLReduce _ _ _ _ _ _ redVar _ -> map getVarName redVar
  _ -> []
  where getVarName (varname, _) = getNameFromVarName varname
