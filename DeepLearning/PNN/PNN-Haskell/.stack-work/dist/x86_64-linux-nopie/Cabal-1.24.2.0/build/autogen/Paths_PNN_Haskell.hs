{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_PNN_Haskell (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ruben/Documents/PNN-Haskell/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/bin"
libdir     = "/home/ruben/Documents/PNN-Haskell/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2/PNN-Haskell-0.1.0.0-1Rk6QNiCN7LFQy8c5gZ6at"
dynlibdir  = "/home/ruben/Documents/PNN-Haskell/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/ruben/Documents/PNN-Haskell/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/share/x86_64-linux-ghc-8.0.2/PNN-Haskell-0.1.0.0"
libexecdir = "/home/ruben/Documents/PNN-Haskell/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/libexec"
sysconfdir = "/home/ruben/Documents/PNN-Haskell/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PNN_Haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PNN_Haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PNN_Haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PNN_Haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PNN_Haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PNN_Haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
