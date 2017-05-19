{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_functional_parsing (
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

bindir     = "/home/wlmr/.cabal/bin"
libdir     = "/home/wlmr/.cabal/lib/x86_64-linux-ghc-8.0.2/functional-parsing-0.1.0.0-DymQ2eXu6gi5m5xdhSJYLr"
dynlibdir  = "/home/wlmr/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/wlmr/.cabal/share/x86_64-linux-ghc-8.0.2/functional-parsing-0.1.0.0"
libexecdir = "/home/wlmr/.cabal/libexec"
sysconfdir = "/home/wlmr/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "functional_parsing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "functional_parsing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "functional_parsing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "functional_parsing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "functional_parsing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "functional_parsing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
