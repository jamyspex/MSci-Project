module StencilDetection where

import           Data.Data
import           Data.Generics         (Data, Typeable, everything, everywhere,
                                        gmapQ, gmapT, mkQ, mkT)
import           Data.List
import           F95IntrinsicFunctions
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils

data Array = Array {
    varName       :: VarName Anno,
    arrDimensions :: Int
} deriving Show

detectStencils :: SubRec -> IO ()
detectStencils subrec = do
    putStrLn (concatMap (\d -> show d ++ "\n") arraysInSub)
    putStrLn (concatMap (\(block, exprs) -> ("\n----------------------------------\n" ++  miniPPF block ++ "\n" ++ (concatMap (\d -> miniPP d ++ "\n") exprs))) results)
    -- putStrLn (concatMap (\d -> miniPP d ++ "\n") $ findArrayAccesses arraysInSub subBody)
    where
        arraysInSub = map arrayFromDecl $ getArrayDecls subrec
        subBody = getSubBody $ subAst subrec
        results = map (\body -> (body, findStencils arraysInSub body)) $ getMapsAndFolds subBody

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
findStencilAccesses arrays (OpenCLMap _ _ _ _ _ _ body) = findStencilAccesses arrays body
findStencilAccesses arrays (OpenCLReduce _ _ _ _ _ _ _ body) = findStencilAccesses arrays body
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
getLoopVariables vars _ = vars

varNameFromLoopIdx (varname, _, _, _) = varname

findStencils :: [Array] -> Fortran Anno -> [Expr Anno]
findStencils arrays fortran = usesLoopVarsOnly
    where
        loopVariables = removeDuplicates (getNameFromVarName . varNameFromLoopIdx) $ getLoopVariables [] fortran
        allAccesses = findStencilAccesses arrays fortran
        uniqueAccesses = removeDuplicates miniPP allAccesses
        sortedByVarName = sortBy sortVarNames uniqueAccesses
        groupedByArray = groupBy groupByArray sortedByVarName
        groupByArray var1 var2 = arrayName var1 == arrayName var2
        moreThanOneAccess = concatMap id $ filter (\grp -> length grp > 1) groupedByArray
        usesLoopVarsOnly = usesIndexVariablesAndConstantOffset loopVariables moreThanOneAccess

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
                        missing@_ -> error ("Unimplemented Fortran Statement " ++ show missing)
        recursiveCall = getArrayReadsQuery arrays


