{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception hiding (Handler)
import           Control.Monad
import           Prelude hiding (catch)
import           Test.Framework
------------------------------------------------------------------------------

import           SafeCWD
import qualified Tests as Tests


------------------------------------------------------------------------------
main :: IO ()
main =
  inDir True "non-cabal-appdir" runTests
  where
    runTests = do
      defaultMain tests `finally` (return ())

    tests =
      [ mutuallyExclusive $ testGroup "with db migration" [Tests.testsDbInit]
      , mutuallyExclusive $ testGroup "from empty db" [Tests.tests]
      ]
