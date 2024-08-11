{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_sudoku (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\Sudoku\\sudoku\\.stack-work\\install\\51f1adf6\\bin"
libdir     = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\Sudoku\\sudoku\\.stack-work\\install\\51f1adf6\\lib\\x86_64-windows-ghc-9.6.6\\sudoku-0.1.0.0-399xrDApZHxLgoDbfbxMi6-sudoku"
dynlibdir  = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\Sudoku\\sudoku\\.stack-work\\install\\51f1adf6\\lib\\x86_64-windows-ghc-9.6.6"
datadir    = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\Sudoku\\sudoku\\.stack-work\\install\\51f1adf6\\share\\x86_64-windows-ghc-9.6.6\\sudoku-0.1.0.0"
libexecdir = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\Sudoku\\sudoku\\.stack-work\\install\\51f1adf6\\libexec\\x86_64-windows-ghc-9.6.6\\sudoku-0.1.0.0"
sysconfdir = "C:\\Users\\arthu\\OneDrive\\Documentos\\Dev\\Haskel\\Haskell\\Sudoku\\sudoku\\.stack-work\\install\\51f1adf6\\etc"

getBinDir     = catchIO (getEnv "sudoku_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "sudoku_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "sudoku_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "sudoku_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_sysconfdir") (\_ -> return sysconfdir)



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
