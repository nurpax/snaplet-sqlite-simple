{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Network.HTTP.Conduit    as HTTP
import           Prelude hiding (catch)
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           System.IO
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
------------------------------------------------------------------------------
--import qualified Snap.Snaplet.Auth.Tests

import           Snap.Http.Server (simpleHttpServe)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession

import           SafeCWD


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App


------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth authInit

    return $ App s a


------------------------------------------------------------------------------
authInit :: SnapletInit App (AuthManager App)
authInit = initJsonFileAuthManager defAuthSettings sess "users.json"

------------------------------------------------------------------------------
main :: IO ()
main = do
    (tid, mvar) <- inDir False "non-cabal-appdir" startServer
    defaultMain [tests] `finally` killThread tid

    putStrLn "waiting for termination mvar"
    takeMVar mvar

  where tests = mutuallyExclusive $
                testGroup "snap" [ ]
--                                 , Snap.Snaplet.Auth.Tests.tests


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

