{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_src (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\admin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.4.1\\src-0.0.1-inplace-src"
dynlibdir  = "C:\\Users\\admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.4.1"
datadir    = "C:\\Users\\admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.4.1\\src-0.0.1"
libexecdir = "C:\\Users\\admin\\AppData\\Roaming\\cabal\\src-0.0.1-inplace-src\\x86_64-windows-ghc-9.4.1\\src-0.0.1"
sysconfdir = "C:\\Users\\admin\\AppData\\Roaming\\cabal\\etc"

getBinDir     = catchIO (getEnv "src_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "src_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "src_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "src_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "src_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "src_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
