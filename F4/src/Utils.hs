module Utils where

import           Data.Generics
import qualified Data.Map             as DMap
import           Debug.Trace
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP


type SubNameStr = String
type SrcName = String
data SubRec = MkSubRec {
       subAst          :: ProgUnit Anno,
       subSrcFile      :: String,
       subSrcLines     :: [String],
       subName         :: String,
       argTranslations :: ArgumentTranslationTable,
       parallelise     :: Bool
}

data ArgumentTranslation = ArgTrans {
    parameter :: ArgName Anno,
    argument  :: VarName Anno
} deriving Show


type ArgumentTranslationTable = DMap.Map SubNameStr ((Fortran Anno), [ArgumentTranslation])


type SubroutineTable = DMap.Map SubNameStr SubRec

removeDuplicates :: Ord a => (b -> a) -> [b] -> [b]
removeDuplicates getKey input = DMap.elems uniqueMap
    where
        pairsForMap = map (\item -> (getKey item, item)) input
        uniqueMap = foldr addToMap DMap.empty pairsForMap
        addToMap :: Ord a => (a, b) -> DMap.Map a b -> DMap.Map a b
        addToMap (key, val) map = DMap.insert key val map

-- validate that a large expression only contains certain allowed types of sub expression
validateExprListContents :: (Expr Anno -> [Bool]) -> Expr Anno -> Bool
validateExprListContents subExprQuery expr = foldr (&&) True $ everything (++) (mkQ [] subExprQuery) expr

-- get index expr from array accesses
idxVarQuery :: Expr Anno -> [Expr Anno]
idxVarQuery (Var _ _ [(_,indices)]) = indices

getSubroutineBody :: SubRec -> Fortran Anno
getSubroutineBody subrec = getSubBody ast
    where
        ast = subAst subrec

getArgName (ArgName _ name) = name

getArgs :: ProgUnit Anno -> [ArgName Anno]
getArgs (Sub _ _ return _ args _) =
    if return /= Nothing then
        error "Only subroutines with Nothing as their return type can be merged"
    else
        everything (++) (mkQ [] argNameQuery) args
    where
        argNameQuery :: ArgName Anno -> [ArgName Anno]
        argNameQuery input = case input of
                        argname@(ArgName _ name) -> [argname]
                        _                        -> []
getArgs _ = error "Passed something other than a Sub to getArgs"

getArgsAsString :: ProgUnit Anno -> [String]
getArgsAsString sub = map getArgName $ getArgs sub
gerArgsAsString = error "Passed something other than a Sub to getArgsAsString"


getAttrs typeDecl = case typeDecl of
    (BaseType _ _ attrs _ _) -> attrs
    (ArrayT _ _ _ attrs _ _) -> attrs

getSubBody :: ProgUnit Anno -> Fortran Anno
getSubBody (Sub _ _ _ _ _ (Block _ _ _ _ _ fortran)) = fortran
getSubBody (Module _ _ _ _ _ _ progUnits) = head $ map getSubBody progUnits

getSubName (Sub _ _ _ (SubName _ name) _ _) = name

getDecls :: ProgUnit Anno -> [Decl Anno]
getDecls (Sub _ _ _ _ _ (Block _ _ _ _ decls _))  = everything (++) (mkQ [] getDeclsQuery) decls
getDecls (Module _ _ _ _ _ _ progUnits) = concatMap getDecls progUnits

getDeclsQuery :: Decl Anno -> [Decl Anno]
getDeclsQuery decl = case decl of
                        (Decl _ _ _ _) -> [decl]
                        _              -> []

getDeclNames :: ProgUnit Anno -> [String]
getDeclNames  (Sub _ _ _ _ _ (Block _ _ _ _ decls _)) = map (getNameFromVarName . getVarName) declStatements
    where
        declStatements = everything (++) (mkQ [] declNameQuery) $ decls

getAllVarNames expr = everything (++) (mkQ [] extractVarNamesFromExpr) expr


getVarName (Var _ _ ((varname, _):_)) = varname

getVarNameG expr = head $ everything (++) (mkQ [] extractVarNamesFromExpr) expr

getNameFromVarName (VarName _ name) = name

declNameAsString = getNameFromVarName . getVarName . head . declNameQuery

declNameQuery :: Decl Anno -> [Expr Anno]
declNameQuery decl = case decl of
                                (Decl _ _ ((expr, _, _):_) _) -> [expr] --everything (++) (mkQ [] extractVarNamesFromExpr) expr
                                _                             -> []

extractVarNamesFromExpr :: Expr Anno -> [VarName Anno]
extractVarNamesFromExpr expr = case expr of
                            Var _ _ varnameList -> map (\(varname, _) -> varname) varnameList
                            _ -> []

buildAstSeq :: (a -> a -> a) -> a -> [a] -> a
buildAstSeq _ nullNode [] = nullNode
buildAstSeq _ _ (statement:[]) = statement
buildAstSeq constructor nullNode (statement:statements) = constructor statement (buildAstSeq constructor nullNode statements)

readIndex :: String -> Int
readIndex = (round . read)

data Array = Array {
    varName         :: VarName Anno,
    arrDimensions   :: Int,
    dimensionRanges :: [(Int, Int)]
} deriving Show

arrayReadQuery :: [Array] -> Expr Anno -> [Expr Anno]
arrayReadQuery arrays expr = case expr of
    var@(Var _ _ ((VarName _ name, (idx:_)):_)) -> if (name `elem` arrayNames) then [var] else []
    _                            -> []
    where
        arrayNames = map (\array -> let (VarName _ name) = (varName array) in name) arrays

getAllArrayAccesses :: [Array] -> Fortran Anno -> [Expr Anno]
getAllArrayAccesses arrays fortran = concatMap (arrayReadQuery arrays) allVars
    where
        allVars = everything (++) (mkQ [] (allVarsQuery)) fortran

allVarsQuery expr = case expr of
                    v@(Var _ _ _) -> [v]
                    _             -> []

data ArrayAccess = ArrayRead | ArrayWrite

getArrayAccesses :: ArrayAccess -> [Array] -> Fortran Anno -> [Expr Anno]
getArrayAccesses readOrWrite arrays fortran = allReadExprs
    where
        allReadExprs = everything (++) (mkQ [] (arrayReadQuery arrays)) exprsFromFortran
        exprsFromFortran = case readOrWrite of
            ArrayRead ->
                case fortran of
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
                    _ -> []
            ArrayWrite ->
                case fortran of
                    (Assg _ _ lhs _) -> [lhs]
                    _                -> []
        recursiveCall = getArrayAccesses readOrWrite arrays


getArrayReads :: [Array] -> Fortran Anno -> [Expr Anno]
getArrayReads arrays fortran = getArrayAccesses ArrayRead arrays fortran

arrayFromDecl :: Decl Anno -> Array
arrayFromDecl decl = arrayFromDeclWithRanges False decl

arrayFromDeclWithRanges :: Bool -> Decl Anno -> Array
arrayFromDeclWithRanges withRanges decl@(Decl _ _ _ typeDecl) = Array {
        varName = name,
        arrDimensions = numberOfDimensions,
        dimensionRanges = if withRanges then dimInts else []
    }
    where
        dimExprs = getArrayDimensions typeDecl
        dimInts = map getArrayDimensionConstants dimExprs
        numberOfDimensions = length $ getArrayDimensions typeDecl
        name = (getVarName . head . declNameQuery) decl

getArrayDimensionConstants :: (Expr Anno, Expr Anno) -> (Int, Int)
getArrayDimensionConstants (expr1, expr2) = (getSingleConstant expr1, getSingleConstant expr2)

-- get an int from a an expr defining array dimensions
getSingleConstant :: Expr Anno -> Int
getSingleConstant expr = case expr of
    (Con _ _ val) -> read val :: Int
    (Unary _ _ _ (Con _ _ val)) -> negate $ read val :: Int
    (NullExpr _ _) -> 1 -- when array declared with starting index omitted, fortran defaults to 1
    _ -> error ("Expr other than constant in array dimensions. \n")

getArrayDimensions :: Type Anno -> [(Expr Anno, Expr Anno)]
getArrayDimensions declType = case declType of
                                (ArrayT _ dimensions _ _ _ _) -> dimensions
                                (BaseType _ _ attrs _ _) -> concatMap id $ concatMap getDimensionAttrs attrs
    where
        getDimensionAttrs attr = case attr of
            (Dimension _ dimensions) -> [dimensions]
            _                        -> []

getArrayDecls :: ProgUnit Anno -> [Decl Anno]
getArrayDecls progUnit = arrayDecls
    where
        arrayDecls = filter (isArrayDecl) $ getDecls progUnit

getDeclType :: Decl Anno -> Type Anno
getDeclType (Decl _ _ _ typeDecl) = typeDecl

isArrayDecl :: Decl Anno -> Bool
isArrayDecl decl = (not . null . getArrayDimensions . getDeclType) decl

debug_displaySubRoutineTable :: SubroutineTable -> Bool -> IO ()
debug_displaySubRoutineTable srt withAst = case withAst of
    False -> mapM_ debug_displaySubTableEntry asList
    True  -> mapM_ debug_displaySubTableEntryWithAst asList
    where
        asList = map (\(_, value) -> value) $ DMap.toList srt

debug_displaySubTableEntry :: SubRec -> IO ()
debug_displaySubTableEntry sr = do
    putStrLn $ hl
    putStrLn $ "Subroutine name: " ++ (subName sr)
    putStrLn $ "Filename: " ++ (subSrcFile sr)
    putStrLn $ "Source:"
    putStrLn $ miniPPProgUnit (subAst sr)
    putStrLn $ "Argument translations:"
    putStrLn $ concatMap (\(subname, (callStatement, argTransList)) -> "\t" ++ subname ++ "->\n" ++
        "\t" ++ miniPPF callStatement ++ "\n" ++
        (concatMap (\argTrans -> "\t" ++ show argTrans ++ "\n") argTransList)) $ DMap.toList (argTranslations sr)
    putStrLn (if (parallelise sr) then "This subroutine will be offloaded to the FPGA" else "This subroutine will not be offloaded to the FPGA")
    putStrLn $ hl ++ "\n"
    where
        hl = (take 80 $ repeat '=')

debug_displaySubTableEntryWithAst :: SubRec -> IO ()
debug_displaySubTableEntryWithAst sr = do
    putStrLn $ hl
    putStrLn $ "Subroutine name: " ++ (subName sr)
    putStrLn $ "Filename: " ++ (subSrcFile sr)
    putStrLn $ "Source:"
    putStrLn $ miniPPProgUnit (subAst sr)
    putStrLn $ "AST: "
    putStrLn $ show (subAst sr)
    putStrLn $ "Argument translations:"
    putStrLn $ concatMap (\(subname, (callStatement, argTransList)) -> "\t" ++ subname ++ "->\n" ++
        "\t" ++ miniPPF callStatement ++ "\n" ++
        (concatMap (\argTrans -> "\t" ++ show argTrans ++ "\n") argTransList)) $ DMap.toList (argTranslations sr)
    putStrLn (if (parallelise sr) then "This subroutine will be offloaded to the FPGA" else "This subroutine will not be offloaded to the FPGA")
    putStrLn $ hl ++ "\n"
    where
        hl = (take 80 $ repeat '=')



-- null nodes useful AST construction
nullUseBlock = UseBlock (UseNil nullAnno) NoSrcLoc
