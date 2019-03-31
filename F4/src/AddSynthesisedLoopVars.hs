{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module AddSynthesisedLoopVars where

import           Control.Monad.Extra
import           Data.Generics
import           Data.List
import           Data.List.Index
import           Data.List.Utils
import qualified Data.Map             as DMap
import           Data.Maybe
import           Data.Ord
import           Data.Tuple.Utils
import           Debug.Trace
import           DetectDriverLoopSize
import           FortranDSL
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Utils

-- This module synthesises and adds the statements needed to calculate
-- the original loopVariables from the driver loop. This allows them
-- to be used in the previously added loop guards.
synthesiseLoopVars :: [(Int, Int)] -> [Kernel] -> IO [Kernel]
synthesiseLoopVars largestStreamDims kernels =
  mapM (addToOneKernel largestOperatingRangeForEachLoopVar) $
  zip kernels nestingDirs
  where
    largestOperatingRangeForEachLoopVar =
      map
        (maximumBy
           (\(_, _, (lwb1, upb1)) (_, _, (lwb2, upb2)) ->
              (upb1 - lwb1) `compare` (upb2 - lwb2)))
        varDimListGrouped
    varDimListGrouped =
      groupBy (\(_, lv1, _) (_, lv2, _) -> lv1 == lv2) $
      sortBy (\(_, lv1, _) (_, lv2, _) -> lv1 `compare` lv2) $
      concat loopVarsAndOperatingRanges
    (nestingDirs, loopVarsAndOperatingRanges) =
      unzip $ map (getAllLoopVarToRange largestStreamDims . body) kernels

loopVarName = "count"

addToOneKernel ::
     [(Int, String, (Int, Int))] -> (Kernel, NestingDirection) -> IO Kernel
addToOneKernel largestOperatingRangeForEachLoopVar (kernel, nestingDirection) = do
  putStrLn $ show kernelWithLoopVarsUpdated
  return kernelWithLoopVarsUpdated
  where
    kernelCode = body kernel
    loopVarNamesOnly =
      map (\(_, name, _) -> name) largestOperatingRangeForEachLoopVar
    derivationCode =
      emitLoopVarDerivationCode usedLoopVarsAndRanges nestingDirection
    kernelWithLoopDerviation =
      addDerivationCodeToKernelBody derivationCode kernel
    allDeclNames = getDeclNames kernelCode
    usedLoopVarsAndRanges =
      filter
        (\(_, loopVarName, _) -> loopVarName `elem` allDeclNames)
        largestOperatingRangeForEachLoopVar
    kernelWithLoopVarsUpdated =
      removeIntentFromLoopVars loopVarNamesOnly kernelWithLoopDerviation

removeIntentFromLoopVars :: [String] -> Kernel -> Kernel
removeIntentFromLoopVars loopVars kernel = kernel {body = newKernelCode}
  where
    kernelCode@(Sub anno srcSpan returnType name args block) = body kernel
    (Block blockAnno uses implicit blockSrcSpan _ fortran) = block
    allDecls = getDecls kernelCode
    originalSubName = getSubName kernelCode
    newKernelCode = Sub anno srcSpan returnType name args updatedBlock
    updatedBlock =
      Block
        blockAnno
        uses
        implicit
        blockSrcSpan
        (declNode withIntentRemovedFromLoopVarDecls)
        fortran
    withIntentRemovedFromLoopVarDecls =
      map
        (\d ->
           if (getNameFromVarName . getVarNameG) d `elem` loopVars
             then removeIntentAttr d
             else d)
        allDecls
    removeIntentAttr :: Decl Anno -> Decl Anno
    removeIntentAttr d@(Decl an srcSpn expr fortranType) =
      Decl an srcSpn expr updatedType
      where
        updatedType =
          BaseType nullAnno baseType attrsWithIntentsRemoved kind len
        (baseType, attrs, kind, len) =
          ( getBaseType fortranType
          , getAttrs fortranType
          , getKind fortranType
          , getLen fortranType)
        attrsWithIntentsRemoved =
          filter
            (\case
               (Intent _ _) -> False
               _ -> True)
            attrs

addDerivationCodeToKernelBody :: Fortran Anno -> Kernel -> Kernel
addDerivationCodeToKernelBody dervCode kernel =
  kernel {driverLoopVariableName = loopVarName, body = withDervCodeInserted}
  where
    kernelCode = body kernel
    withDervCodeInserted = everywhere (mkT addDervCodeQuery) kernelCode
    addDervCodeQuery fortran =
      case fortran of
        (OpenCLMap _ _ read written nestVars enclosedVars body) ->
          OpenCLMap
            nullAnno
            nullSrcSpan
            read
            written
            nestVars
            enclosedVars
            (block [dervCode, body])
        (OpenCLReduce _ _ read written nestVars enclosedVars reductionVars body) ->
          OpenCLReduce
            nullAnno
            nullSrcSpan
            read
            written
            nestVars
            enclosedVars
            reductionVars
            (block [dervCode, body])
        _ -> fortran

emitLoopVarDerivationCode ::
     [(Int, String, (Int, Int))] -> NestingDirection -> Fortran Anno
emitLoopVarDerivationCode allLoopVarInfo nestingDirection =
  block $
  map (emitOneLoopVarDerivation loopVarName sortedByNestOrder) sortedByNestOrder
  where
    sortedByNestOrder =
      case nestingDirection of
        _ -> sortOn fst3 allLoopVarInfo
        -- Reverse -> sortOn (Down . fst3) allLoopVarInfo
        -- _       -> error "Invalid nesting direction!"

emitOneLoopVarDerivation ::
     String
  -> [(Int, String, (Int, Int))]
  -> (Int, String, (Int, Int))
  -> Fortran Anno
emitOneLoopVarDerivation driverLoopIdxName allLoopVarInfo (0, loopVarName, _) =
  assign
    (var loopVarName)
    (divide (var driverLoopIdxName) (con $ multiplyBounds 0 allLoopVarInfo))
emitOneLoopVarDerivation driverLoopIdxName allLoopVarInfo (nestLevel, loopVarName, (lwb, upb)) =
  assign
    (var loopVarName)
    (modulo
       (if moduloDivisor == 1
          then var driverLoopIdxName
          else divide (var driverLoopIdxName) (con moduloDivisor))
       (con (upb - lwb + 1)))
  where
    moduloDivisor = multiplyBounds nestLevel allLoopVarInfo

multiplyBounds nestLevel allLoopVarInfo =
  product $
  map (\(_, _, (lwb, upb)) -> upb - lwb + 1) $
  drop (nestLevel + 1) allLoopVarInfo

-- Get a list of all the loop variables and the dimensions of the arrays
-- they are used to access then validate these are all the same for each variable
-- and if they are return one tuple for each variable.
-- The returned tuples have the following meaning:
--      (loop nesting level in original code 0 = least nested
--      loop variable name,
--      range of array dimension the variable is used to iteration over)
getAllLoopVarToRange ::
     [(Int, Int)]
  -> ProgUnit Anno
  -> (NestingDirection, [(Int, String, (Int, Int))])
getAllLoopVarToRange largestStreamDims kernelCode =
  case getNestingDirection loopVarUsageOrder loopVarsFromFortran of
    Normal ->
      ( Normal
      , imap
          (\idx item -> (idx, item, largestStreamDims !! idx))
          loopVarUsageOrder)
    Reverse ->
      ( Reverse
      , imap
          (\idx item -> (idx, item, largestStreamDims !! idx))
          (reverse loopVarUsageOrder))
    _ -> error "Loop variable usage is not simple can't calculate dummy indices"
  where
    kernelBody = getSubBody kernelCode
    allArrayDecls = getArrayDecls kernelCode
    allArrays = map arrayFromDecl allArrayDecls
    allArrayAccesses = getAllArrayAccesses allArrays kernelBody
    loopVarsFromFortran =
      map getNameFromVarName (getLoopVars $ getSubBody kernelCode)
    loopVarUsageOrder =
      getLoopVarUsageOrder loopVarsFromFortran allArrayAccesses

getNestingDirection :: [String] -> [String] -> NestingDirection
getNestingDirection usageOrder nestOrder =
  case nestDirectionTuple of
    (True, False) -> Normal
    (False, True) -> Reverse
    _             -> Undefined
  where
    nestDirectionTuple =
      (usageOrder == nestOrder, reverse usageOrder == nestOrder)

getLoopVarUsageOrder :: [String] -> [Expr Anno] -> [String]
getLoopVarUsageOrder loopVars arrayAccesses =
  uniq $ map fst $ sortOn snd loopVarPositions
  where
    loopIndexPos = concatMap (getLoopIndexPosition loopVars) arrayAccesses
    loopVarPositions = checkLoopVarUsageConsistentAndGetPositions loopIndexPos

checkLoopVarUsageConsistentAndGetPositions ::
     [(String, String, Maybe Int)] -> [(String, Int)]
checkLoopVarUsageConsistentAndGetPositions allLoopIndexPos =
  if valid
    then map (\(_, lv, Just pos) -> (lv, pos)) usingAllIndexes
    else error "loop var usage is not consistent"
  where
    usingAllIndexes =
      head $ filter (all isJust . map (\(_, _, pos) -> pos)) grpdByArray
    valid = all (validateIndex usingAllIndexes) allLoopIndexPos
    grpdByArray =
      groupBy (\(an1, _, _) (an2, _, _) -> an1 == an2) allLoopIndexPos

validateIndex ::
     [(String, String, Maybe Int)] -> (String, String, Maybe Int) -> Bool
validateIndex usesAllLoopVars tv@(_, loopVar, Just pos) =
  pos <= shouldBeLoopVarPos
  where
    (_, _, Just shouldBeLoopVarPos) =
      head $ filter (\(_, lv, _) -> lv == loopVar) usesAllLoopVars
-- <= so array loop vars can be used to index arrays with lower dimensions
-- Doubt that this is robust - FIXME
validateIndex _ (_, _, Nothing) = True

allStreamsSameSize :: [Kernel] -> IO Bool
allStreamsSameSize kernels = do
  mapM_ printSize allStreams
  return valid
  where
    printSize s =
      putStrLn
        ("stream = " ++
         getStreamName s ++ " dims = " ++ show (getStreamDimensions s))
    allStreams = concatMap inputs kernels ++ concatMap outputs kernels
    allSizes = map getStreamDimensions allStreams
    valid = all (== head allSizes) allSizes
