module StencilDetection where

import           Data.Data
import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Data.List
import qualified Data.Map              as DMap
import           Debug.Trace
import           F95IntrinsicFunctions
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Text.Read
import           Utils

data Array = Array {
    varName       :: VarName Anno,
    arrDimensions :: Int
} deriving Show

detectStencilsInSubsToBeParallelise :: SubroutineTable -> SubroutineTable
detectStencilsInSubsToBeParallelise srt = updatedSrt
    where
        forOffloadSubs = filter (\sub -> parallelise sub) $ DMap.elems srt
        withStencilsDetect = map (\sub -> (detectStencils sub)) forOffloadSubs
        updatedSrt = foldr (\cur acc -> DMap.insert (subName cur) cur acc) srt withStencilsDetect

detectStencils :: SubRec -> SubRec
detectStencils subrec = removeRedundantStencilNodes $ addStencilNodes arraysInSub subrec
    where
        arraysInSub = map arrayFromDecl $ getArrayDecls subrec

removeRedundantStencilNodes :: SubRec -> SubRec
removeRedundantStencilNodes subRec = subRec { subAst = everywhere (mkT removeStencilQuery) $ subAst subRec}
    where
        removeStencilQuery :: Fortran Anno -> Fortran Anno
        removeStencilQuery (OpenCLStencil anno srcspan stencils body) = OpenCLStencil anno srcspan stencils $ everywhere (mkT removeInnerStenQuery) body
        removeStencilQuery curVal = curVal
        removeInnerStenQuery :: Fortran Anno -> Fortran Anno
        removeInnerStenQuery (OpenCLStencil _ _ _ body) = body
        removeInnerStenQuery curVal                     = curVal

addStencilNodes :: [Array] -> SubRec -> SubRec
addStencilNodes arrays subRec = subRec { subAst = everywhere (mkT addNodeQuery) $ subAst subRec }
    where
        addNodeQuery curVal@(OpenCLMap _ _ _ _ _ _ body)
            | length stencils > 0 = OpenCLStencil nullAnno nullSrcSpan stencils curVal
            | otherwise = curVal
            where
                stencils = createStencilRecords arrays $ findStencils arrays curVal
        addNodeQuery curVal@(OpenCLReduce _ _ _ _ _ _ _ body)
            | length stencils > 0 = OpenCLStencil nullAnno nullSrcSpan stencils curVal
            | otherwise = curVal
            where
                stencils = createStencilRecords arrays $ findStencils arrays curVal
        addNodeQuery curVal = curVal

createStencilRecords :: [Array] -> [[Expr Anno]] -> [Stencil Anno]
createStencilRecords arrays stencils = map (\(arrayDecl, stenUsages) -> createOneStencilRecord arrayDecl stenUsages) arrayDeclUsagePairs
    where
        arrayDeclMapItems = map (\array -> (getNameFromVarName $ varName array, array)) arrays
        arrayDeclMap = DMap.fromList arrayDeclMapItems
        stencilMapItems = map (\stencils -> (arrayName $ head stencils, stencils)) stencils
        stenMap = DMap.fromList stencilMapItems
        requiredKeys = DMap.keys stenMap
        arrayDeclUsagePairs = map (\key -> (arrayDeclMap DMap.! key, stenMap DMap.! key)) requiredKeys

createOneStencilRecord :: Array -> [Expr Anno] -> Stencil Anno
createOneStencilRecord arrayDecl stencilArrayAccesses = Stencil nullAnno dimensions (length offsets) offsets arrayVarName
    where
        arrayVarName = getVarName $ head stencilArrayAccesses
        offsets =  map (getOffsets . idxVarQuery) stencilArrayAccesses
        dimensions = arrDimensions arrayDecl

getOffsets :: [Expr Anno] -> [StencilIndex]
getOffsets indices = map getOffset indices

getOffset :: Expr Anno -> StencilIndex
getOffset t@(Bin _ _ (Plus _) loopVar (Con _ _ offset))  = Offset $ readIndex offset
getOffset t@(Bin _ _ (Minus _) loopVar (Con _ _ offset)) = Offset (negate $ readIndex offset)
getOffset t@(Var _ _ loopVar) = Offset 0
getOffset t@(Con _ _ val) = Constant $ readIndex val
getOffset missing@_ = error ("getOffset pattern miss for: " ++ show missing)

arrayFromDecl :: Decl Anno -> Array
arrayFromDecl decl@(Decl _ _ _ typeDecl) = Array { varName = name, arrDimensions = numberOfDimensions}
    where
        numberOfDimensions = length $ getArrayDimensions typeDecl
        name = getVarName decl

getArrayDimensions :: Type Anno -> [(Expr Anno, Expr Anno)]
getArrayDimensions declType = case declType of
                                (ArrayT _ dimensions _ _ _ _) -> dimensions
                                (BaseType _ _ attrs _ _) -> concatMap id $ concatMap getDimensionAttrs attrs
    where
        getDimensionAttrs attr = case attr of
            (Dimension _ dimensions) -> [dimensions]
            _                        -> []

getArrayDecls :: SubRec -> [Decl Anno]
getArrayDecls subrec = arrayDecls
    where
        arrayDecls = filter (isArrayDecl) $ getDecls (subAst subrec)

getDeclType :: Decl Anno -> Type Anno
getDeclType (Decl _ _ _ typeDecl) = typeDecl

isArrayDecl :: Decl Anno -> Bool
isArrayDecl decl = (not . null . getArrayDimensions . getDeclType) decl

findStencilAccesses :: [Array] -> Fortran Anno -> [Expr Anno]
findStencilAccesses arrays (OpenCLMap _ _ _ _ _ _ body) = findStencilAccesses arrays body -- trace ("trace:: \n" ++ miniPPF body ++ "\n::trace") $ findStencilAccesses arrays body
findStencilAccesses arrays (OpenCLReduce _ _ _ _ _ _ _ body) = findStencilAccesses arrays body -- trace ("trace:: \n" ++  miniPPF body ++ "\n::trace") $ findStencilAccesses arrays body
-- findStencilAccesses arrays (OpenCLStencil _ _ _ body) = findStencilAccesses arrays body
findStencilAccesses arrays body = arrayAccesses
    where
        arrayAccesses = everything (++) (mkQ [] (getArrayReadsQuery arrays)) body


arrayName = (getNameFromVarName . getVarName)

sortVarNames :: Expr Anno -> Expr Anno -> Ordering
sortVarNames one two = name1 `compare` name2
    where
        name1 = arrayName one
        name2 = arrayName two

getMapsAndFolds :: Fortran Anno -> [Fortran Anno]
getMapsAndFolds fortran = everything (++) (mkQ [] mapFoldQuery) fortran
    where
        mapFoldQuery :: Fortran Anno -> [Fortran Anno]
        mapFoldQuery fortran = case fortran of
            map@(OpenCLMap _ _ _ _ _ _ _)       -> [map]
            fold@(OpenCLReduce _ _ _ _ _ _ _ _) -> [fold]
            a@_                                 -> []

usesIndexVariablesAndConstantOffset :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)] -> [Expr Anno] -> [Expr Anno]
usesIndexVariablesAndConstantOffset idxVars stencilAccesses = getStencilsOnly simpleOpsOnly
    where
        getStencilsOnly = map fst
        loopVarNames = map (\(varName, _, _, _) -> getNameFromVarName varName) idxVars
        stencilsAndIdxNames = map stencilToStencilIdxNames stencilAccesses
        stencilsUsingLoopVars
            = getStencilsOnly $ filter (\(_, indices) -> foldr (\cur acc -> acc && cur `elem` loopVarNames) True indices) stencilsAndIdxNames
        stencilsUsingLoopVarsAndIdxExprs = map stencilToIndexExprs stencilsUsingLoopVars
        simpleOpsOnly
            = filter (\(_, indices) -> foldr (&&) True (map stencilOnlyContainsValidOps indices)) stencilsUsingLoopVarsAndIdxExprs

-- this might have to do constant folding at some point
stencilOnlyContainsValidOps :: Expr Anno -> Bool
stencilOnlyContainsValidOps index = foldr (&&) True $ map convertToBools getAllExpr
    where
        getAllExpr = everything (++) (mkQ [] getSimpleExprsQuery) index
        getSimpleExprsQuery :: Expr Anno -> [Expr Anno]
        getSimpleExprsQuery expr = case expr of
                                    e@(Bin _ _ _ _ _) -> [e]
                                    e@(Con _ _ _)     -> [e]
                                    e@(Var _ _ _)     -> [e]
                                    _                 -> []
        convertToBools expr = case expr of
                                (Bin _ _ _ _ _) -> True
                                (Con _ _ _)     -> True
                                (Var _ _ _)     -> True
                                _               -> False


idxVarQuery :: Expr Anno -> [Expr Anno]
idxVarQuery (Var _ _ [(_,indices)]) = indices

stencilToIndexExprs sten = (sten, idxVarQuery sten)
stencilToStencilIdxNames sten = (sten, map getNameFromVarName $ everything (++) (mkQ [] extractVarNamesFromExpr) $ idxVarQuery sten)

getLoopVariables :: [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)]
                 -> Fortran Anno
                 -> [(VarName Anno, Expr Anno, Expr Anno, Expr Anno)]
getLoopVariables vars (OpenCLMap _ _ _ _ loopVariables _ body) = getLoopVariables (vars ++ loopVariables) body
getLoopVariables vars (OpenCLReduce _ _ _ _ loopVariables _ _ body) = getLoopVariables (vars ++ loopVariables) body
getLoopVariables vars _ = removeDuplicates (getNameFromVarName . varNameFromLoopIdx) vars

varNameFromLoopIdx (varname, _, _, _) = varname

findStencils :: [Array] -> Fortran Anno -> [[Expr Anno]]
findStencils arrays fortran = moreThanOneAccess
    where
        loopVariables = removeDuplicates (getNameFromVarName . varNameFromLoopIdx) $ getLoopVariables [] fortran
        allAccesses = findStencilAccesses arrays fortran
        uniqueAccesses = removeDuplicates miniPP allAccesses
        usesLoopVarsOnly = usesIndexVariablesAndConstantOffset loopVariables uniqueAccesses
        sortedByVarName = sortBy sortVarNames usesLoopVarsOnly
        groupedByArray = groupBy groupByArray sortedByVarName
        groupByArray var1 var2 = arrayName var1 == arrayName var2
        moreThanOneAccess = filter (\grp -> length grp > 1) groupedByArray

getArrayReadsQuery :: [Array] -> Fortran Anno -> [Expr Anno]
getArrayReadsQuery arrays fortran = allReadExprs
    where
        arrayNames = map (\array -> let (VarName _ name) = (varName array) in name) arrays
        arrayReadQuery expr = case expr of
                                var@(Var _ _ ((VarName _ name, (idx:_)):_)) ->
                                    if (name `elem` arrayNames) then [var] else []
                                _                            -> []
        allReadExprs = everything (++) (mkQ [] arrayReadQuery) readExprsFromFortran
        readExprsFromFortran = case fortran of
                        (Assg _ _ _ rhs) -> [rhs]
                        (For _ _ _ start bound incre body) -> (start:bound:incre:(recursiveCall body))
                        (DoWhile _ _ bound body) -> [bound] ++ recursiveCall body
                        (FSeq _ _ fst snd) -> recursiveCall fst ++ recursiveCall snd
                        (If _ _ cond branch elseIfs elseBranch) ->
                            [cond] ++ recursiveCall branch ++ elseBranchResult
                            ++ branchConds ++ concatMap recursiveCall branchBodys
                            where
                                (branchConds, branchBodys) = unzip elseIfs
                                elseBranchResult = case elseBranch of
                                    (Just body) -> recursiveCall body
                                    _           -> []
                        (NullStmt _ _) -> []
                        -- (OpenCLStencil _ _ _ body) -> recursiveCall body
                        -- (OpenCLMap _ _ _ _ _ _ body) -> recursiveCall body
                        -- (OpenCLReduce _ _ _ _ _ _ _ body) -> recursiveCall body
                        missing@_ -> [] --error ("Unimplemented Fortran Statement " ++ miniPPF missing)
        recursiveCall = getArrayReadsQuery arrays


