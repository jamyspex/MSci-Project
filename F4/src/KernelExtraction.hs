module KernelExtraction where

import           Data.Generics
import           Data.List.Index
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils


-- Function goes through the merged subroutine and extracts kernel subroutines
-- for each map/fold returns a module containing all the appropriate subroutines
getKernels :: SubRec -> IO ()
getKernels subrec = do
    mapM_ (\(_, b) -> putStrLn ("\n--------------------\n" ++ miniPPProgUnit b ++ "\n======================\n")) kernelSubs
    putStrLn $ "no. of kernels: " ++ (show . length) kernelSubs
    return ()
    where
        allDecls = getDecls $ subAst subrec
        allArgs = getArgs $ subAst subrec
        subbody = getSubroutineBody subrec
        allOldSubNameAndBodies = everything (++) (mkQ [] getOldSubsQuery) subbody
        kernelBodies = map (getKernelsQuery False) allOldSubNameAndBodies
        globablyOrdered = indexed $ concatMap prepareForKernelBuilder kernelBodies
        curriedKernelBuilder = makeKernelSub allDecls allArgs
        kernelSubs = map (\(globalOrder, builderInput) -> curriedKernelBuilder builderInput globalOrder) globablyOrdered

-- strip of the OriginalSubContainer wrapper from the kernels and produce
-- tuples of (body, (name, maybe splitNum)) splitNum = what part of the
-- original subroutine the new kernel is made up off. If the original subroutine
-- wasn't split up then it is Nothing
prepareForKernelBuilder :: [Fortran Anno] -> [(Fortran Anno, (String, Maybe Int))]
prepareForKernelBuilder kernels
    | length kernels > 1 = imap (\i b -> prepareOne (Just i) b) kernels
    | otherwise = [prepareOne Nothing $ head kernels]
    where
        prepareOne :: Maybe Int -> Fortran Anno -> (Fortran Anno, (String, Maybe Int))
        prepareOne splitNum (OriginalSubContainer _ name body) = (body, (name, splitNum))
        prepareOne _ _ = error "Kernel body not wrapped by OriginalSubContainer!"

-- Pick out at old subroutine boundaries so we can attach the original
-- name to the new kernels for nicer naming
getOldSubsQuery :: Fortran Anno -> [(String, Fortran Anno)]
getOldSubsQuery fortran = case fortran of
    (OriginalSubContainer _ name body) -> [(name, body)]
    _                                  -> []


-- form new valid subroutines based of the kernels
-- e.g. insert all the appropriate decls and arguments
-- and give them a name. Arguments:
-- (function body, (Original name, Maybe sub split number)) ->
-- globalOrderNumber -> all decls -> all args -> (global order, kernel sub)
makeKernelSub :: [Decl Anno] -> [ArgName Anno] -> (Fortran Anno, (String, Maybe Int)) -> Int -> (Int, ProgUnit Anno)
makeKernelSub decls args (body, (originalName, splitNum)) globalOrder =
    (globalOrder, Sub nullAnno nullSrcSpan Nothing name argList block)
    where
        argsComparable = getComparableItems args getArgName
        declComparable = getComparableItems decls (getNameFromVarName . getVarName . declNameQuery)
        allVarsNamesInBody = map getNameFromVarName $ getAllVarNames body
        subRequiredArgs = map snd $ filter (\(argName, _) -> argName `elem` allVarsNamesInBody) argsComparable
        subRequiredDecls = map snd $ filter (\(declName, _) -> declName `elem` allVarsNamesInBody) declComparable
        argList = Arg nullAnno (buildAstSeq (ASeq nullAnno) (NullArg nullAnno) subRequiredArgs) nullSrcSpan
        declList = buildAstSeq (DSeq nullAnno) (NullDecl nullAnno nullSrcSpan) subRequiredDecls
        nameSuffix = case splitNum of
            Nothing  -> ""
            Just val -> "_" ++ show val
        name = SubName nullAnno (originalName ++ nameSuffix)
        impl = ImplicitNull nullAnno
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
    (FSeq _ _ f1 f2) -> getKernelBody name f1 ++ getKernelBody name f2 ++ getKernelsQuery True (name, f2)
    _ -> getKernelBody name body
getKernelsQuery True (name, body) = case body of
    (FSeq _ _ f1 f2) -> getKernelBody name f1 ++ getKernelBody name f2 ++ getKernelsQuery True (name, f2)
    _ -> []

-- A kernel is defined as a OpenCLMap/Reduce/Stencil at the top level of a block
-- OriginalSubContainer is used to encapslate the newly split kernels so the original
-- sub name can be used when naming the kernel.
getKernelBody :: String -> Fortran Anno -> [Fortran Anno]
getKernelBody name fortran = case fortran of
    map@(OpenCLMap _ _ _ _ _ _ _)         -> [(OriginalSubContainer nullAnno name map)]
    reduce@(OpenCLReduce _ _ _ _ _ _ _ _) -> [(OriginalSubContainer nullAnno name reduce)]
    stencil@(OpenCLStencil _ _ _ _)       -> [(OriginalSubContainer nullAnno name stencil)]
    _                                     -> []
