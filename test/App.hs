{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module App where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Prelude hiding (catch)
------------------------------------------------------------------------------

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------
data App = App
    {
      _sess :: Snaplet SessionManager
    , _db   :: Snaplet Sqlite
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasSqlite (Handler b App) where
    getSqliteState = with db get

------------------------------------------------------------------------------
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test application" Nothing $ do
  s <- nestSnaplet "sess" sess $
          initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

  -- Initialize auth that's backed by an sqlite database
  d <- nestSnaplet "db" db sqliteInit
  a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
  return $ App s d a

