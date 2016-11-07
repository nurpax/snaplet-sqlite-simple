{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

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
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import qualified Database.SQLite.Simple.ToField as S
import           Database.SQLite.Simple.Types
import           Database.SQLite3 (SQLData(..))
import           Paths_snaplet_sqlite_simple
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.SqliteSimple
import           Web.ClientSession


data SqliteAuthManager = SqliteAuthManager
    { pamTable    :: AuthTable
    , pamConnPool :: MVar S.Connection
    }


------------------------------------------------------------------------------
-- | Initializer for the sqlite backend to the auth snaplet.
--
initSqliteAuth
  :: SnapletLens b SessionManager  -- ^ Lens to the session snaplet
  -> Snaplet Sqlite  -- ^ The sqlite snaplet
  -> SnapletInit b (AuthManager b)
initSqliteAuth sess db = makeSnaplet "sqlite-auth" desc datadir $ do
    config <- getSnapletUserConfig
    authTable <- liftIO $ C.lookupDefault "snap_auth_user" config "authTable"
    authSettings <- authSettingsFromConfig
    key <- liftIO $ getKey (asSiteKey authSettings)
    let tableDesc = defAuthTable { tblName = authTable }
    let manager = SqliteAuthManager tableDesc $
                                      sqliteConn $ db ^# snapletValue
    liftIO $ createTableIfMissing manager
    rng <- liftIO mkRNG
    return AuthManager
      { backend = manager
      , session = sess
      , activeUser = Nothing
      , minPasswdLen = asMinPasswdLen authSettings
      , rememberCookieName = asRememberCookieName authSettings
      , rememberCookieDomain = Nothing
      , rememberPeriod = asRememberPeriod authSettings
      , siteKey = key
      , lockout = asLockout authSettings
      , randomNumberGenerator = rng
      }
  where
    desc = "An Sqlite backend for user authentication"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


tableExists :: S.Connection -> T.Text -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

createInitialSchema :: S.Connection -> AuthTable -> IO ()
createInitialSchema conn pamTable = do
  let q = Query $ T.concat
          [ "CREATE TABLE ", tblName pamTable, " (uid INTEGER PRIMARY KEY,"
          , "login text UNIQUE NOT NULL,"
          , "password text,"
          , "activated_at timestamp,suspended_at timestamp,remember_token text,"
          , "login_count INTEGER NOT NULL,failed_login_count INTEGER NOT NULL,"
          , "locked_out_until timestamp,current_login_at timestamp,"
          , "last_login_at timestamp,current_login_ip text,"
          , "last_login_ip text,created_at timestamp,updated_at timestamp);"
          ]
  S.execute_ conn q

versionTable :: AuthTable -> T.Text
versionTable pamTable = T.concat [tblName pamTable, "_version"]

schemaVersion :: S.Connection -> AuthTable -> IO Int
schemaVersion conn pamTable = do
  let verTbl = versionTable pamTable
  versionExists <- tableExists conn verTbl
  if not versionExists
    then return 0
    else
    do
      let q = T.concat ["SELECT version FROM ", verTbl, " LIMIT 1"]
      [Only v] <- S.query_ conn (Query q) :: IO [Only Int]
      return v

setSchemaVersion :: S.Connection -> AuthTable -> Int -> IO ()
setSchemaVersion conn pamTable v = do
  let q = Query $ T.concat ["UPDATE ", versionTable pamTable, " SET version = ?"]
  S.execute conn q (Only v)

upgradeSchema :: Connection -> AuthTable -> Int -> IO ()
upgradeSchema conn pam fromVersion = do
  ver <- schemaVersion conn pam
  when (ver == fromVersion) (upgrade ver >> setSchemaVersion conn pam (fromVersion+1))
  where
    upgrade 0 = do
      S.execute_ conn (Query $ T.concat ["CREATE TABLE ", versionTable pam, " (version INTEGER)"])
      S.execute_ conn (Query $ T.concat ["INSERT INTO  ", versionTable pam, " VALUES (1)"])

    upgrade 1 = do
      S.execute_ conn (addColumnQ (colEmail pam))
      S.execute_ conn (addColumnQ (colResetToken pam))
      S.execute_ conn (addColumnQ (colResetRequestedAt pam))

    upgrade 2 = do
      S.execute_ conn (addColumnQ (colRoles pam))
      S.execute_ conn (addColumnQ (colMeta pam))

    upgrade _ = error "unknown version"

    addColumnQ (c,t) =
      Query $ T.concat [ "ALTER TABLE ", tblName pam, " ADD COLUMN ", c, " ", t]


------------------------------------------------------------------------------
-- | Create the user table if it doesn't exist.
createTableIfMissing :: SqliteAuthManager -> IO ()
createTableIfMissing SqliteAuthManager{..} =
    withMVar pamConnPool $ \conn -> do
      authTblExists <- tableExists conn $ tblName pamTable
      unless authTblExists $ createInitialSchema conn pamTable
      upgradeSchema conn pamTable 0
      upgradeSchema conn pamTable 1
      upgradeSchema conn pamTable 2


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
        <*> _userEmail
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
        <*> _userResetToken
        <*> _userResetRequestedAt
        <*> decodeRoles
        <*> decodeMeta
      where
        !_userId               = field
        !_userLogin            = field
        !_userEmail            = field
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
        !_userResetToken       = field
        !_userResetRequestedAt = field
        !_userRoles            = field :: RowParser (Maybe LT.Text)
        !_userMeta             = field :: RowParser (Maybe LT.Text)

        decodeRoles :: RowParser [Role]
        decodeRoles = do
          roles <- fmap (fmap (map Role) . textDecodeBS) _userRoles
          return $ fromMaybe [] roles

        decodeMeta = do
          meta <- fmap (fmap (fromMaybe HM.empty . A.decode' . LT.encodeUtf8)) _userMeta
          return $ fromMaybe HM.empty meta

        -- NOTE I think the T/LT.encudeUtf8 will break if the Roles
        -- list contains strings that are not valid UTF-8.  It's
        -- probably never the case, though.  In my defense, this is a
        -- rarely used feature, and somewhat deprecated in Snap.
        textDecodeBS :: Maybe LT.Text -> Maybe [ByteString]
        textDecodeBS Nothing  = Nothing
        textDecodeBS (Just t) = fmap (map T.encodeUtf8) . A.decode' . LT.encodeUtf8 $ t


querySingle :: (ToRow q, FromRow a)
            => MVar S.Connection -> Query -> q -> IO (Maybe a)
querySingle pool q ps = withMVar pool $ \conn -> return . listToMaybe =<<
    S.query conn q ps

authExecute :: ToRow q
            => MVar S.Connection -> Query -> q -> IO ()
authExecute pool q ps = do
    withMVar pool $ \conn -> S.execute conn q ps
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
  ,  colEmail            :: (Text, Text)
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
  ,  colResetToken       :: (Text, Text)
  ,  colResetRequestedAt :: (Text, Text)
  ,  colRoles            :: (Text, Text)
  ,  colMeta             :: (Text, Text)
  }


-- | Default authentication table layout
defAuthTable :: AuthTable
defAuthTable
  =  AuthTable
  {  tblName             = "snap_auth_user"
  ,  colId               = ("uid", "INTEGER PRIMARY KEY")
  ,  colLogin            = ("login", "text UNIQUE NOT NULL")
  ,  colEmail            = ("email", "text")
  ,  colPassword         = ("password", "text")
  ,  colActivatedAt      = ("activated_at", "timestamp")
  ,  colSuspendedAt      = ("suspended_at", "timestamp")
  ,  colRememberToken    = ("remember_token", "text")
  ,  colLoginCount       = ("login_count", "INTEGER NOT NULL")
  ,  colFailedLoginCount = ("failed_login_count", "INTEGER NOT NULL")
  ,  colLockedOutUntil   = ("locked_out_until", "timestamp")
  ,  colCurrentLoginAt   = ("current_login_at", "timestamp")
  ,  colLastLoginAt      = ("last_login_at", "timestamp")
  ,  colCurrentLoginIp   = ("current_login_ip", "text")
  ,  colLastLoginIp      = ("last_login_ip", "text")
  ,  colCreatedAt        = ("created_at", "timestamp")
  ,  colUpdatedAt        = ("updated_at", "timestamp")
  ,  colResetToken       = ("reset_token", "text")
  ,  colResetRequestedAt = ("reset_requested_at", "timestamp")
  ,  colRoles            = ("roles_json", "text")
  ,  colMeta             = ("meta_json", "text")
  }

-- | List of deconstructors so it's easier to extract column names from an
-- 'AuthTable'.
colDef :: [(AuthTable -> (Text, Text), AuthUser -> SQLData)]
colDef =
  [ (colId              , S.toField . fmap unUid . userId)
  , (colLogin           , S.toField . userLogin)
  , (colEmail           , S.toField . userEmail)
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
  , (colResetToken      , S.toField . userResetToken)
  , (colResetRequestedAt, S.toField . userResetRequestedAt)
  , (colRoles           , S.toField . encodeOrNull . userRoles)
  , (colMeta            , S.toField . encodeOrNullHM . userMeta)
  ]
  where
    encodeOrNull [] = Nothing
    encodeOrNull x  = Just . LT.decodeUtf8 . A.encode $ x

    encodeOrNullHM hm | HM.null hm = Nothing
                      | otherwise  = Just . LT.decodeUtf8 . A.encode $ hm


colNames :: AuthTable -> T.Text
colNames pam =
  T.intercalate "," . map (\(f,_) -> fst (f pam)) $ colDef

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
    -- The list of column names
    cols = map (fst . ($at) . fst) $ tail colDef
    vals = map (const "?") cols
    params = map (($u) . snd) $ tail colDef


------------------------------------------------------------------------------
-- |
instance IAuthBackend SqliteAuthManager where
    save SqliteAuthManager{..} u@AuthUser{..} = do
        let (qstr, params) = saveQuery pamTable u
        withMVar pamConnPool $ \conn -> do
            -- Note that the user INSERT here expects that duplicate
            -- login error checking has been done already at the level
            -- that calls here.
            S.execute conn (Query qstr) params
            let q2 = Query $ T.concat
                     [ "select ", colNames pamTable, " from "
                     , tblName pamTable
                     , " where "
                     , fst (colLogin pamTable)
                     , " = ?"
                     ]
            -- TODO S.query may throw an exception, ideally we would
            -- catch it and turn it into an AuthError.
            [savedUser] <- S.query conn q2 [userLogin]
            return $ Right savedUser

    lookupByUserId SqliteAuthManager{..} uid = do
        let q = Query $ T.concat
                [ "select ", colNames pamTable, " from "
                , tblName pamTable
                , " where "
                , fst (colId pamTable)
                , " = ?"
                ]
        querySingle pamConnPool q [unUid uid]

    lookupByLogin SqliteAuthManager{..} login = do
        let q = Query $ T.concat
                [ "select ", colNames pamTable, " from "
                , tblName pamTable
                , " where "
                , fst (colLogin pamTable)
                , " = ?"
                ]
        querySingle pamConnPool q [login]

    lookupByRememberToken SqliteAuthManager{..} token = do
        let q = Query $ T.concat
                [ "select ", colNames pamTable, " from "
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
