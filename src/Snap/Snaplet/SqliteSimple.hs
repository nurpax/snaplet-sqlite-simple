{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

{-|

This snaplet makes it simple to use a SQLite database from your Snap
application and is based on the sqlite-simple library
(<http://hackage.haskell.org/package/sqlite-simple>).  Now, adding a
database to your web app takes just two simple steps.

First, include this snaplet in your application's state.

> data App = App
>     { ... -- Other state needed in your app
>     , _db :: Snaplet Sqlite
>     }

Next, call the sqliteInit from your application's initializer.

> appInit = makeSnaplet ... $ do
>     ...
>     d <- nestSnaplet "db" db sqliteInit
>     return $ App ... d

Now you can use any of the sqlite-simple wrapper functions defined in
this module anywhere in your application handlers.  For instance:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- with db $ query_ "select * from blog_post"
>     ...

Optionally, if you find yourself doing many database queries, you can
eliminate some of the boilerplate by defining a HasSqlite instance for
your application.

> instance HasSqlite (Handler b App) where
>   getSqliteState = with db get

With this code, our postHandler example no longer requires the 'with'
function:

> postHandler :: Handler App App ()
> postHandler = do
>     posts <- query_ "select * from blog_post"
>     ...

The first time you run an application with the sqlite-simple snaplet,
a configuration file @devel.cfg@ is created in the
@snaplets/sqlite-simple@ directory underneath your project root.  It
specifies how to connect to your Sqlite database.  Edit this file and
modify the values appropriately and you'll be off and running.

If you want to have out-of-the-box authentication, look at the
documentation for the "Snap.Snaplet.Auth.Backends.Sqlite" module.

-}

module Snap.Snaplet.SqliteSimple (
  -- * The Snaplet
    Sqlite(..)
  , HasSqlite(..)
  , sqliteInit
  , withSqlite

  -- * Wrappers and re-exports
  , query
  , query_
  , execute
  , execute_

  -- Re-exported from sqlite-simple
  , S.Connection
  , S.Query
  , S.Only(..)
  , S.FormatError(..)
  , S.ResultError(..)
  , (S.:.)(..)
  , ToRow(..)
  , FromRow(..)

  , field

  ) where

import           Prelude hiding (catch)

import           Control.Monad.CatchIO hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import qualified Data.Configurator as C
import           Data.List
import           Data.Maybe
import           Data.Pool
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromRow
import qualified Database.SQLite.Simple as S
import           Snap
import           Paths_snaplet_sqlite_simple



------------------------------------------------------------------------------
-- | The state for the sqlite-simple snaplet. To use it in your app
-- include this in your application state and use 'sqliteInit' to initialize it.
data Sqlite = Sqlite
    { sqlitePool :: Pool S.Connection
    -- ^ Function for retrieving the connection pool
    }


------------------------------------------------------------------------------
-- | Instantiate this typeclass on 'Handler b YourAppState' so this snaplet
-- can find the connection source.  If you need to have multiple instances of
-- the sqlite snaplet in your application, then don't provide this instance
-- and leverage the default instance by using \"@with dbLens@\" in front of calls
-- to snaplet-sqlite-simple functions.
class (MonadCatchIO m) => HasSqlite m where
    getSqliteState :: m Sqlite


------------------------------------------------------------------------------
-- | Default instance
instance HasSqlite (Handler b Sqlite) where
    getSqliteState = get


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use this snaplet in the
-- Initializer monad like this:
--
-- > d <- nestSnaplet "db" db pgsInit
-- > count <- liftIO $ runReaderT (execute "INSERT ..." params) d
instance (MonadCatchIO m) => HasSqlite (ReaderT (Snaplet Sqlite) m) where
    getSqliteState = asks (\sqlsnaplet -> sqlsnaplet ^# snapletValue)


------------------------------------------------------------------------------
-- | A convenience instance to make it easier to use functions written for
-- this snaplet in non-snaplet contexts.
instance (MonadCatchIO m) => HasSqlite (ReaderT Sqlite m) where
    getSqliteState = ask


------------------------------------------------------------------------------
-- | Convenience function allowing easy collection of config file errors.
logErr :: MonadIO m
       => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
logErr err m = do
    res <- liftIO m
    when (isNothing res) (tell [err])
    return res


------------------------------------------------------------------------------
-- | Initialize the snaplet
sqliteInit :: SnapletInit b Sqlite
sqliteInit = makeSnaplet "sqlite-simple" description datadir $ do
    config <- getSnapletUserConfig
    (mci,errs) <- runWriterT $ do
        db <- logErr "Must specify db filename" $ C.lookup config "db"
        return $ db
    let ci = fromMaybe (error $ intercalate "\n" errs) mci

    stripes <- liftIO $ C.lookupDefault 1 config "numStripes"
    idle <- liftIO $ C.lookupDefault 5 config "idleTime"
    resources <- liftIO $ C.lookupDefault 20 config "maxResourcesPerStripe"
    pool <- liftIO $ createPool (S.open ci) S.close stripes
                                (realToFrac (idle :: Double)) resources
    return $ Sqlite pool
  where
    description = "Sqlite abstraction"
    datadir = Just $ liftM (++"/resources/db") getDataDir


------------------------------------------------------------------------------
-- | Convenience function for executing a function that needs a database
-- connection.
withSqlite :: (HasSqlite m)
       => (S.Connection -> IO b) -> m b
withSqlite f = do
    s <- getSqliteState
    let pool = sqlitePool s
    liftIO $ withResource pool f


------------------------------------------------------------------------------
-- | See 'P.query'
query :: (HasSqlite m, ToRow q, FromRow r)
      => S.Query -> q -> m [r]
query q params = withSqlite (\c -> S.query c q params)


------------------------------------------------------------------------------
-- | See 'P.query_'
query_ :: (HasSqlite m, FromRow r) => S.Query -> m [r]
query_ q = withSqlite (\c -> S.query_ c q)


------------------------------------------------------------------------------
-- |
execute :: (HasSqlite m, ToRow q, MonadCatchIO m)
        => S.Query -> q -> m ()
execute template qs = withSqlite (\c -> S.execute c template qs)


------------------------------------------------------------------------------
-- |
execute_ :: (HasSqlite m, MonadCatchIO m)
         => S.Query -> m ()
execute_ template = withSqlite (\c -> S.execute_ c template)
