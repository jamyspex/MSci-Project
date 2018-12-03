{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_language_fortran (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/james/Documents/Uni/MSci-Project/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/bin"
libdir     = "/home/james/Documents/Uni/MSci-Project/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/lib/x86_64-linux-ghc-8.4.3/language-fortran-0.3-DnTmLOXLSgMntUzwJ6Kf9"
dynlibdir  = "/home/james/Documents/Uni/MSci-Project/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/james/Documents/Uni/MSci-Project/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/share/x86_64-linux-ghc-8.4.3/language-fortran-0.3"
libexecdir = "/home/james/Documents/Uni/MSci-Project/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/libexec/x86_64-linux-ghc-8.4.3/language-fortran-0.3"
sysconfdir = "/home/james/Documents/Uni/MSci-Project/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "language_fortran_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "language_fortran_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "language_fortran_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "language_fortran_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "language_fortran_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "language_fortran_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
