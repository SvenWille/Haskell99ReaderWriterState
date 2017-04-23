{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Haskell99ReaderWriterState (
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

bindir     = "/home/johan/Haskell99ReaderWriterState/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/bin"
libdir     = "/home/johan/Haskell99ReaderWriterState/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/lib/x86_64-linux-ghc-8.0.2/Haskell99ReaderWriterState-0.1.0.0-2rzng8ObeTiCXjwW2WVPQJ"
dynlibdir  = "/home/johan/Haskell99ReaderWriterState/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/johan/Haskell99ReaderWriterState/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/share/x86_64-linux-ghc-8.0.2/Haskell99ReaderWriterState-0.1.0.0"
libexecdir = "/home/johan/Haskell99ReaderWriterState/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/libexec"
sysconfdir = "/home/johan/Haskell99ReaderWriterState/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskell99ReaderWriterState_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskell99ReaderWriterState_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Haskell99ReaderWriterState_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Haskell99ReaderWriterState_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskell99ReaderWriterState_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskell99ReaderWriterState_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
