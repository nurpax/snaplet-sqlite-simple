{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|

This module allows you to use the auth snaplet with your user database
stored in a SQLite database.  When you run your application with this
snaplet, a config file will be copied into the the
@snaplets/sqlite-auth@ directory.  This file contains all of the
configurable options for the snaplet and allows you to change them
without recompiling your application.

To use this snaplet in your application enable the session, sqlite,
and auth snaplets as follows:

> data App = App
>     { ... -- your own application state here
>     , _sess :: Snaplet SessionManager
>     , _db   :: Snaplet Sqlite
>     , _auth :: Snaplet (AuthManager App)
>     }

Then in your initializer you'll have something like this:

> d <- nestSnaplet "db" db sqliteInit
> a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

If you have not already created the database table for users, it will
automatically be created for you the first time you run your
application.

-}

module Snap.Snaplet.Auth.Backends.SqliteSimple
  ( initSqliteAuth
  ) where

------------------------------------------------------------------------------
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe
import           Data.Pool
import           Database.SQLite3 (SQLData(..))
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.ToField as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Types
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Session
import           Web.ClientSession
import           Paths_snaplet_sqlite_simple


data SqliteAuthManager = SqliteAuthManager
    { pamTable    :: AuthTable
    , pamConnPool :: Pool S.Connection
    }


------------------------------------------------------------------------------
-- | Initializer for the sqlite backend to the auth snaplet.
--
initSqliteAuth
  :: Lens b (Snaplet SessionManager)  -- ^ Lens to the session snaplet
  -> Snaplet Sqlite  -- ^ The sqlite snaplet
  -> SnapletInit b (AuthManager b)
initSqliteAuth sess db = makeSnaplet "sqliteql-auth" desc datadir $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- authSettingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let tableDesc = defAuthTable { tblName = authTable }
    let manager = SqliteAuthManager tableDesc $
                                      sqlitePool $ getL snapletValue db
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return $ AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "An Sqlite backend for user authentication"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


------------------------------------------------------------------------------
-- | Create the user table if it doesn't exist.
createTableIfMissing :: SqliteAuthManager -> IO ()
createTableIfMissing SqliteAuthManager{..} = do
    withResource pamConnPool $ \conn -> do
        res <- S.query_ conn $ Query $
          "select relname from pg_class where relname='"
          `T.append` tblName pamTable `T.append` "'"
        when (null (res :: [Only T.Text])) $
          S.execute_ conn (Query q) >> return ()
    return ()
  where
    q = T.concat
          [ "CREATE TABLE "
          , tblName pamTable
          , " ("
          , T.intercalate "," (map (fDesc . ($pamTable) . (fst)) colDef)
          , ")"
          ]

buildUid :: Int -> UserId
buildUid = UserId . T.pack . show


instance FromField UserId where
    fromField f = buildUid <$> fromField f

instance FromField Password where
    fromField f = Encrypted <$> fromField f

instance FromRow AuthUser where
    fromRow =
        AuthUser
        <$> _userId
        <*> _userLogin
        <*> _userPassword
        <*> _userActivatedAt
        <*> _userSuspendedAt
        <*> _userRememberToken
        <*> _userLoginCount
        <*> _userFailedLoginCount
        <*> _userLockedOutUntil
        <*> _userCurrentLoginAt
        <*> _userLastLoginAt
        <*> _userCurrentLoginIp
        <*> _userLastLoginIp
        <*> _userCreatedAt
        <*> _userUpdatedAt
        <*> _userRoles
        <*> _userMeta
      where
        !_userId               = field
        !_userLogin            = field
        !_userPassword         = field
        !_userActivatedAt      = field
        !_userSuspendedAt      = field
        !_userRememberToken    = field
        !_userLoginCount       = field
        !_userFailedLoginCount = field
        !_userLockedOutUntil   = field
        !_userCurrentLoginAt   = field
        !_userLastLoginAt      = field
        !_userCurrentLoginIp   = field
        !_userLastLoginIp      = field
        !_userCreatedAt        = field
        !_userUpdatedAt        = field
        !_userRoles            = pure []
        !_userMeta             = pure HM.empty


querySingle :: (ToRow q, FromRow a)
            => Pool S.Connection -> Query -> q -> IO (Maybe a)
querySingle pool q ps = withResource pool $ \conn -> return . listToMaybe =<<
    S.query conn q ps

authExecute :: ToRow q
            => Pool S.Connection -> Query -> q -> IO ()
authExecute pool q ps = do
    withResource pool $ \conn -> S.execute conn q ps
    return ()

instance S.ToField Password where
    toField (ClearText bs) = S.toField bs
    toField (Encrypted bs) = S.toField bs


-- | Datatype containing the names of the columns for the authentication table.
data AuthTable
  =  AuthTable
  {  tblName             :: Text
  ,  colId               :: (Text, Text)
  ,  colLogin            :: (Text, Text)
  ,  colPassword         :: (Text, Text)
  ,  colActivatedAt      :: (Text, Text)
  ,  colSuspendedAt      :: (Text, Text)
  ,  colRememberToken    :: (Text, Text)
  ,  colLoginCount       :: (Text, Text)
  ,  colFailedLoginCount :: (Text, Text)
  ,  colLockedOutUntil   :: (Text, Text)
  ,  colCurrentLoginAt   :: (Text, Text)
  ,  colLastLoginAt      :: (Text, Text)
  ,  colCurrentLoginIp   :: (Text, Text)
  ,  colLastLoginIp      :: (Text, Text)
  ,  colCreatedAt        :: (Text, Text)
  ,  colUpdatedAt        :: (Text, Text)
  ,  rolesTable          :: Text
  }

-- | Default authentication table layout
defAuthTable :: AuthTable
defAuthTable
  =  AuthTable
  {  tblName             = "snap_auth_user"
  ,  colId               = ("uid", "SERIAL PRIMARY KEY")
  ,  colLogin            = ("login", "text UNIQUE NOT NULL")
  ,  colPassword         = ("password", "text")
  ,  colActivatedAt      = ("activated_at", "timestamptz")
  ,  colSuspendedAt      = ("suspended_at", "timestamptz")
  ,  colRememberToken    = ("remember_token", "text")
  ,  colLoginCount       = ("login_count", "integer NOT NULL")
  ,  colFailedLoginCount = ("failed_login_count", "integer NOT NULL")
  ,  colLockedOutUntil   = ("locked_out_until", "timestamptz")
  ,  colCurrentLoginAt   = ("current_login_at", "timestamptz")
  ,  colLastLoginAt      = ("last_login_at", "timestamptz")
  ,  colCurrentLoginIp   = ("current_login_ip", "text")
  ,  colLastLoginIp      = ("last_login_ip", "text")
  ,  colCreatedAt        = ("created_at", "timestamptz")
  ,  colUpdatedAt        = ("updated_at", "timestamptz")
  ,  rolesTable          = "user_roles"
  }

fDesc :: (Text, Text) -> Text
fDesc f = fst f `T.append` " " `T.append` snd f

-- | List of deconstructors so it's easier to extract column names from an
-- 'AuthTable'.
colDef :: [(AuthTable -> (Text, Text), AuthUser -> SQLData)]
colDef =
  [ (colId              , S.toField . fmap unUid . userId)
  , (colLogin           , S.toField . userLogin)
  , (colPassword        , S.toField . userPassword)
  , (colActivatedAt     , S.toField . userActivatedAt)
  , (colSuspendedAt     , S.toField . userSuspendedAt)
  , (colRememberToken   , S.toField . userRememberToken)
  , (colLoginCount      , S.toField . userLoginCount)
  , (colFailedLoginCount, S.toField . userFailedLoginCount)
  , (colLockedOutUntil  , S.toField . userLockedOutUntil)
  , (colCurrentLoginAt  , S.toField . userCurrentLoginAt)
  , (colLastLoginAt     , S.toField . userLastLoginAt)
  , (colCurrentLoginIp  , S.toField . userCurrentLoginIp)
  , (colLastLoginIp     , S.toField . userLastLoginIp)
  , (colCreatedAt       , S.toField . userCreatedAt)
  , (colUpdatedAt       , S.toField . userUpdatedAt)
  ]

saveQuery :: AuthTable -> AuthUser -> (Text, [SQLData])
saveQuery at u@AuthUser{..} = maybe insertQuery updateQuery userId
  where
    insertQuery =  (T.concat [ "INSERT INTO "
                             , tblName at
                             , " ("
                             , T.intercalate "," cols
                             , ") VALUES ("
                             , T.intercalate "," vals
                             , ")"
                             ]
                   , params)
    qval f  = fst (f at) `T.append` " = ?"
    updateQuery uid =
        (T.concat [ "UPDATE "
                  , tblName at
                  , " SET "
                  , T.intercalate "," (map (qval . fst) $ tail colDef)
                  , " WHERE "
                  , fst (colId at)
                  , " = ?"
                  ]
        , params ++ [S.toField $ unUid uid])
    cols = map (fst . ($at) . fst) $ tail colDef
    vals = map (const "?") cols
    params = map (($u) . snd) $ tail colDef


------------------------------------------------------------------------------
-- |
instance IAuthBackend SqliteAuthManager where
    save SqliteAuthManager{..} u@AuthUser{..} = do
        let (qstr, params) = saveQuery pamTable u
        let q = Query qstr
        withResource pamConnPool $ \conn -> do
            S.execute conn q params
            let q2 = Query $ T.concat
                     [ "select * from "
                     , tblName pamTable
                     , " where "
                     , fst (colLogin pamTable)
                     , " = ?"
                     ]
            res <- S.query conn q2 [userLogin]
            return $ fromMaybe u $ listToMaybe res

    lookupByUserId SqliteAuthManager{..} uid = do
        let q = Query $ T.concat
                [ "select * from "
                , tblName pamTable
                , " where "
                , fst (colId pamTable)
                , " = ?"
                ]
        querySingle pamConnPool q [unUid uid]

    lookupByLogin SqliteAuthManager{..} login = do
        let q = Query $ T.concat
                [ "select * from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?"
                ]
        querySingle pamConnPool q [login]

    lookupByRememberToken SqliteAuthManager{..} token = do
        let q = Query $ T.concat
                [ "select * from "
                , tblName pamTable
                , " where "
                , fst (colRememberToken pamTable)
                , " = ?"
                ]
        querySingle pamConnPool q [token]

    destroy SqliteAuthManager{..} AuthUser{..} = do
        let q = Query $ T.concat
                [ "delete from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?"
                ]
        authExecute pamConnPool q [userLogin]

