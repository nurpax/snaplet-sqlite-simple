module SafeCWD
  ( inDir
  , removeDirectoryRecursiveSafe
  ) where

import Control.Concurrent.SSem
import Control.Exception
import Control.Monad
import System.Directory
import System.IO.Unsafe

sem :: SSem
sem = unsafePerformIO $ new 1

inDir :: Bool -> FilePath -> IO a -> IO a
inDir startClean dir action = bracket before after (const action)
  where
    before = do
        wait sem
        cwd <- getCurrentDirectory
        when startClean $ removeDirectoryRecursiveSafe dir
        createDirectoryIfMissing True dir
        setCurrentDirectory dir
        return cwd
    after cwd = do
        setCurrentDirectory cwd
        signal sem

removeDirectoryRecursiveSafe :: String -> IO ()
removeDirectoryRecursiveSafe p =
    doesDirectoryExist p >>= flip when (removeDirectoryRecursive p)
