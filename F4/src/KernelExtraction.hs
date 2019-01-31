module KernelExtraction where

import           Data.Generics
import           Language.Fortran
import           LanguageFortranTools
import           MiniPP
import           Parser
import           Utils


-- Function goes through the merged subroutine and extracts kernel subroutines
-- for each map/fold returns a module containing all the appropriate subroutines
getKernels :: SubRec -> IO ()
getKernels subrec = do
    mapM_ (\b -> putStrLn ("\n--------------------\n" ++ miniPPF b ++ "\n======================\n")) kernelBodies
    putStrLn $ "no. of kernels: " ++ (show . length) kernelBodies
    return ()
    where
        subbody = getSubroutineBody subrec
        kernelBodies = concatMap (getKernelsQuery False) allOldSubNameAndBodies
        allOldSubNameAndBodies = everything (++) (mkQ [] getOldSubsQuery) subbody

getOldSubsQuery :: Fortran Anno -> [(String, Fortran Anno)]
getOldSubsQuery fortran = case fortran of
    (MergedSubContainer _ name body) -> [(name, body)]
    _                                -> []



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

getKernelBody :: String -> Fortran Anno -> [Fortran Anno]
getKernelBody name fortran = case fortran of
    map@(OpenCLMap _ _ _ _ _ _ _)         -> [(MergedSubContainer nullAnno name map)]
    reduce@(OpenCLReduce _ _ _ _ _ _ _ _) -> [(MergedSubContainer nullAnno name reduce)]
    stencil@(OpenCLStencil _ _ _ _)       -> [(MergedSubContainer nullAnno name stencil)]
    _                                     -> []
