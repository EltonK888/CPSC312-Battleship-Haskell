{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cpsc312_battleship_project (
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

bindir     = "/mnt/c/Users/EltonKok/OneDrive - The University Of British Columbia/BCS Winter 2020/CPSC312/cpsc312 battleship project/.cabal-sandbox/bin"
libdir     = "/mnt/c/Users/EltonKok/OneDrive - The University Of British Columbia/BCS Winter 2020/CPSC312/cpsc312 battleship project/.cabal-sandbox/lib/x86_64-linux-ghc-8.6.5/cpsc312-battleship-project-0.1.0.0-B8J9PV6NjkjGTwqw7E7xqf-cpsc312-battleship-project"
dynlibdir  = "/mnt/c/Users/EltonKok/OneDrive - The University Of British Columbia/BCS Winter 2020/CPSC312/cpsc312 battleship project/.cabal-sandbox/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/mnt/c/Users/EltonKok/OneDrive - The University Of British Columbia/BCS Winter 2020/CPSC312/cpsc312 battleship project/.cabal-sandbox/share/x86_64-linux-ghc-8.6.5/cpsc312-battleship-project-0.1.0.0"
libexecdir = "/mnt/c/Users/EltonKok/OneDrive - The University Of British Columbia/BCS Winter 2020/CPSC312/cpsc312 battleship project/.cabal-sandbox/libexec/x86_64-linux-ghc-8.6.5/cpsc312-battleship-project-0.1.0.0"
sysconfdir = "/mnt/c/Users/EltonKok/OneDrive - The University Of British Columbia/BCS Winter 2020/CPSC312/cpsc312 battleship project/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cpsc312_battleship_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cpsc312_battleship_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cpsc312_battleship_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cpsc312_battleship_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cpsc312_battleship_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cpsc312_battleship_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
