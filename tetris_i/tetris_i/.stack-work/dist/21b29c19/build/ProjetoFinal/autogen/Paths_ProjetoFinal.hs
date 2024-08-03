{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ProjetoFinal (
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

bindir     = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\tetris_i\\tetris_i\\.stack-work\\install\\43f37ca4\\bin"
libdir     = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\tetris_i\\tetris_i\\.stack-work\\install\\43f37ca4\\lib\\x86_64-windows-ghc-8.8.4\\ProjetoFinal-0.1.0.0-9ar4e4nGWbU9T6ky0Rvk3J-ProjetoFinal"
dynlibdir  = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\tetris_i\\tetris_i\\.stack-work\\install\\43f37ca4\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\tetris_i\\tetris_i\\.stack-work\\install\\43f37ca4\\share\\x86_64-windows-ghc-8.8.4\\ProjetoFinal-0.1.0.0"
libexecdir = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\tetris_i\\tetris_i\\.stack-work\\install\\43f37ca4\\libexec\\x86_64-windows-ghc-8.8.4\\ProjetoFinal-0.1.0.0"
sysconfdir = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\tetris_i\\tetris_i\\.stack-work\\install\\43f37ca4\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProjetoFinal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProjetoFinal_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ProjetoFinal_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ProjetoFinal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProjetoFinal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProjetoFinal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
