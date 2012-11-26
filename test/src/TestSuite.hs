{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception hiding (Handler)
import           Control.Monad
import           Prelude hiding (catch)
import           Test.Framework
------------------------------------------------------------------------------

import           Snap

import           App
import           SafeCWD
import qualified Tests as Tests


------------------------------------------------------------------------------
main :: IO ()
main =
  inDir True "non-cabal-appdir" runTests
  where
    runTests = do
      (_, _handler, _doCleanup) <- runSnaplet Nothing appInit
      defaultMain [tests] `finally` (return ())

    tests = mutuallyExclusive $ testGroup "snaplet-sqlite-simple" [Tests.tests]
