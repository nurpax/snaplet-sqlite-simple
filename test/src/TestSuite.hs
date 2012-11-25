{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception hiding (Handler)
import           Control.Monad
import           Prelude hiding (catch)
import           Snap.Http.Server.Config
import           System.IO
import           Test.Framework
------------------------------------------------------------------------------

import           Snap

import           App
import           SafeCWD
import qualified Tests as Tests


------------------------------------------------------------------------------
main :: IO ()
main = do
    (tid, mvar) <- inDir False "non-cabal-appdir" startServer
    defaultMain [tests] `finally` killThread tid

    putStrLn "waiting for termination mvar"
    takeMVar mvar

  where tests = mutuallyExclusive $
                  testGroup "snaplet-sqlite-simple" [Tests.tests]


------------------------------------------------------------------------------
startServer :: IO (ThreadId, MVar ())
startServer = do
    mvar <- newEmptyMVar
    t    <- forkIO $ serve mvar (setPort 9753 defaultConfig) appInit
    threadDelay $ 2*10^(6::Int)
    return (t, mvar)

  where
    serve mvar config initializer =
        flip finally (putMVar mvar ()) $
        handle handleErr $ do
            hPutStrLn stderr "initializing snaplet"
            (_, handler, doCleanup) <- runSnaplet Nothing initializer

            flip finally doCleanup $ do
                (conf, site) <- combineConfig config handler
                hPutStrLn stderr "bringing up server"
                simpleHttpServe conf site
                hPutStrLn stderr "server killed"

    handleErr :: SomeException -> IO ()
    handleErr e = hPutStrLn stderr $ "startServer exception: " ++ show e

