{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Tests
  ( tests
  , testsDbInit) where


------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad.State as S
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BL
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

------------------------------------------------------------------------------
import           App
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import qualified Snap.Snaplet.SqliteSimple as SQ
import qualified Snap.Test as ST
import           Snap.Snaplet.Test

------------------------------------------------------------------------------
tests :: Test
tests = mutuallyExclusive $ testGroup "Snap.Snaplet.SqliteSimple"
    [ testInitDbEmpty
    , testCreateUserGood
    , testUpdateUser
    ]

------------------------------------------------------------------------------
testsDbInit :: Test
testsDbInit = mutuallyExclusive $ testGroup "Snap.Snaplet.SqliteSimple"
    [ -- Empty db
      testInitDbEmpty
    , testCreateUserGood
    , testUpdateUser
      -- Create empty db with old schema + one user
    , testInitDbSchema0
    , testCreateUserGood
    , testUpdateUser
      -- Create empty db, add user in old schema, then access it
    , testInitDbSchema0WithUser
    , testUpdateUser
      -- Create empty db, add user in old schema, then access it, and delete it
    , testInitDbSchema0
    , testCreateUserGood
    , testDeleteUser
      -- Create empty db, perform some basic queries
    , testInitDbSchema0
    , testQueries
      -- Login tests, these use some otherwise uncovered DB backend
      -- functions.
    , testInitDbSchema0WithUser
    , testLoginByRememberTokenKO
    , testLoginByRememberTokenOK
    , testLogoutOK
    , testCurrentUserKO
    , testCurrentUserOK
    ]

dropTables :: Connection -> IO ()
dropTables conn = do
  execute_ conn "DROP TABLE IF EXISTS snap_auth_user"
  execute_ conn "DROP TABLE IF EXISTS snap_auth_user_version"

-- Must be the first on the test list for basic database
-- initialization (schema creation for snap_auth_user, etc.)
testInitDbEmpty :: Test
testInitDbEmpty = testCase "snaplet database init" go
  where
    go = do
      conn <- open "test.db"
      dropTables conn
      close conn
      (_, _handler, _doCleanup) <- runSnaplet Nothing appInit
      assertBool "init ok" True

initSchema0 :: Query
initSchema0 = Query $ T.concat
              [ "CREATE TABLE snap_auth_user (uid INTEGER PRIMARY KEY,"
              , "login text UNIQUE NOT NULL,"
              , "password text,"
              , "activated_at timestamp,suspended_at timestamp,remember_token text,"
              , "login_count INTEGER NOT NULL,failed_login_count INTEGER NOT NULL,"
              , "locked_out_until timestamp,current_login_at timestamp,"
              , "last_login_at timestamp,current_login_ip text,"
              , "last_login_ip text,created_at timestamp,updated_at timestamp);"
              ]

addFooUserSchema0 :: Query
addFooUserSchema0 =
  "INSERT INTO snap_auth_user VALUES(1,'foo',X'7368613235367C31327C39426E5255534356444B4E6A3553716345774E756E513D3D7C633534506C69614A42314E483562677143494651616732454C75444B684F37745A78655479456C4F6F356F3D',NULL,NULL,'2cc0caf41bd7387150cc1416ac38bccc36e64c11a8945f72298ea366ffa8fc97',0,0,NULL,'2012-11-28 21:59:15.150153','2012-11-28 21:59:15.109848',NULL, NULL, '2012-11-28 21:59:15.052817','2012-11-28 21:59:15.052817');"

-- Must be the first on the test list for basic database
-- initialization (schema creation for snap_auth_user, etc.)
testInitDbSchema0 :: Test
testInitDbSchema0 = testCase "init db with schema0" $ do
  conn <- open "test.db"
  dropTables conn
  execute_ conn initSchema0
  close conn
  (_, _handler, _doCleanup) <- runSnaplet Nothing appInit
  assertBool "init ok" True

-- Initialize db schema to an empty schema0 and add a user 'foo'.  The
-- expectation is that snaplet initialization needs to do schema
-- migration for the tables and rows.
testInitDbSchema0WithUser :: Test
testInitDbSchema0WithUser = testCase "init + add foo user directly" $ do
  conn <- open "test.db"
  dropTables conn
  execute_ conn initSchema0
  execute_ conn addFooUserSchema0
  close conn
  (_, _handler, _doCleanup) <- runSnaplet Nothing appInit
  assertBool "init ok" True


------------------------------------------------------------------------------
testCreateUserGood :: Test
testCreateUserGood = testCase "createUser good params" assertGoodUser
  where
    assertGoodUser :: Assertion
    assertGoodUser = do
      let hdl = with auth $ createUser "foo" "foo"
      res <- evalHandler Nothing (ST.get "" M.empty) hdl appInit
      either (assertFailure . show) checkUserFields res

    checkUserFields (Left _) =
      assertBool "createUser failed: Couldn't create a new user." False

    checkUserFields (Right u) = do
      assertEqual "login match"  "foo" (userLogin u)
      assertEqual "login count"  0 (userLoginCount u)
      assertEqual "fail count"   0 (userFailedLoginCount u)
      assertEqual "local host ip" Nothing (userCurrentLoginIp u)
      assertEqual "local host ip" Nothing (userLastLoginIp u)
      assertEqual "locked until" Nothing (userLockedOutUntil u)
      assertEqual "empty email" Nothing (userEmail u)
      assertEqual "roles" [] (userRoles u)
      assertEqual "meta" HM.empty (userMeta u)

------------------------------------------------------------------------------
-- Create a user, modify it, persist it and load again, check fields ok.
-- Must run after testCreateUserGood

testUpdateUser :: Test
testUpdateUser = testCase "createUser + update good params" assertGoodUser
  where
    assertGoodUser :: Assertion
    assertGoodUser = do
      let loginHdl = with auth $ loginByUsername "foo" (ClearText "foo") True
      res <- evalHandler Nothing (ST.get "" M.empty) loginHdl appInit
      either (assertFailure . show) checkLoggedInUser res

    checkLoggedInUser (Left _) = assertBool "failed login" False
    checkLoggedInUser (Right u) = do
      assertEqual "login count"  1 (userLoginCount u)
      assertEqual "fail count"   0 (userFailedLoginCount u)
      assertEqual "locked until" Nothing (userLockedOutUntil u)
      assertEqual "local host ip" (Just "127.0.0.1") (userCurrentLoginIp u)
      assertEqual "no previous login" Nothing (userLastLoginIp u)
      let saveHdl = with auth $ saveUser (u { userLogin = "bar"
                                            , userRoles = roles
                                            , userMeta  = meta })
      res <- evalHandler Nothing (ST.get "" M.empty) saveHdl appInit
      either (assertFailure . show) checkUpdatedUser res

    roles = [Role $ BL.pack "Superman", Role $ BL.pack "Journalist"]
    meta  = HM.fromList [ (T.pack "email-verified",
                           A.toJSON $ T.pack "yes")
                        , (T.pack "suppress-products",
                           A.toJSON [T.pack "Kryptonite"]) ]

    checkUpdatedUser (Left _) = assertBool "failed saveUser" False
    checkUpdatedUser (Right u) = do
      assertEqual "login rename ok?"  "bar" (userLogin u)
      assertEqual "login count"  1 (userLoginCount u)
      assertEqual "local host ip" (Just "127.0.0.1") (userCurrentLoginIp u)
      assertEqual "local host ip" Nothing (userLastLoginIp u)
      assertEqual "account roles"  roles (userRoles u)
      assertEqual "account meta data" meta (userMeta u)
      let loginHdl = with auth $ loginByUsername "bar" (ClearText "foo") True
      res <- evalHandler Nothing (ST.get "" M.empty) loginHdl appInit
      either (assertFailure . show) (assertBool "login as 'bar' ok?" . isRight) res

------------------------------------------------------------------------------
-- Test that deleting a user works.

testDeleteUser :: Test
testDeleteUser = testCase "delete a user" assertGoodUser
  where
    loginHdl = with auth $ loginByUsername "foo" (ClearText "foo") True

    assertGoodUser :: Assertion
    assertGoodUser = do
      res <- evalHandler Nothing (ST.get "" M.empty) loginHdl appInit
      either (assertFailure . show) delUser res

    delUser (Left _) = assertBool "failed login" False
    delUser (Right u) = do
      let delHdl = with auth $ destroyUser u
      Right res <- evalHandler Nothing (ST.get "" M.empty) delHdl appInit
      res <- evalHandler Nothing (ST.get "" M.empty) loginHdl appInit
      either (assertFailure . show) (assertBool "login as 'foo' should fail now" . isLeft) res

------------------------------------------------------------------------------
-- Query tests

testQueries :: Test
testQueries = testCase "basic queries" runTest
  where
    queries = do
      SQ.execute_ "CREATE TABLE foo (id INTEGER PRIMARY KEY, t TEXT)"
      SQ.execute "INSERT INTO foo (t) VALUES (?)" (Only ("bar" :: String))
      [(a :: Int,b :: String)] <- SQ.query_ "SELECT id,t FROM foo"
      [Only (s :: String)] <- SQ.query "SELECT t FROM foo WHERE id = ?" (Only (1 :: Int))
      withTop db . SQ.withSqlite $ \conn -> do
        a @=? 1
        b @=? "bar"
        s @=? "bar"
        [Only (v :: Int)] <- query_ conn "SELECT 1+1"
        v @=? 2

    runTest :: Assertion
    runTest = do
      r <- evalHandler Nothing (ST.get "" M.empty) queries appInit
      return ()

------------------------------------------------------------------------------
testLoginByRememberTokenKO :: Test
testLoginByRememberTokenKO = testCase "loginByRememberToken no token" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth loginByRememberToken
        res <- evalHandler Nothing (ST.get "" M.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isLeft) res

    failMsg = "loginByRememberToken: Expected to fail for the " ++
              "absence of a token, but didn't."


------------------------------------------------------------------------------
testLoginByRememberTokenOK :: Test
testLoginByRememberTokenOK = testCase "loginByRememberToken token" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler Nothing (ST.get "" M.empty) hdl appInit
        case res of
          (Left e) -> assertFailure $ show e
          (Right res') -> assertBool failMsg $ isRight res'

    hdl :: Handler App App (Either AuthFailure AuthUser)
    hdl = with auth $ do
        res <- loginByUsername "foo" (ClearText "foo") True
        either (\e -> return (Left e)) (\_ -> loginByRememberToken) res

    failMsg = "loginByRememberToken: Expected to succeed but didn't."

------------------------------------------------------------------------------
assertLogout :: Handler App App (Maybe AuthUser) -> String -> Assertion
assertLogout hdl failMsg = do
    res <- evalHandler Nothing (ST.get "" M.empty) hdl appInit
    either (assertFailure . show) (assertBool failMsg . isNothing) res

testLogoutOK :: Test
testLogoutOK = testCase "logout user logged in." $ assertLogout hdl failMsg
  where
    hdl :: Handler App App (Maybe AuthUser)
    hdl = with auth $ do
        loginByUsername "foo" (ClearText "foo") True
        logout
        mgr <- get
        return (activeUser mgr)

    failMsg = "logout: Expected to get Nothing as the active user, " ++
              " but didn't."

------------------------------------------------------------------------------
testCurrentUserKO :: Test
testCurrentUserKO = testCase "currentUser unsuccesful call" assertion
  where
    assertion :: Assertion
    assertion = do
        let hdl = with auth currentUser
        res <- evalHandler Nothing (ST.get "" M.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isNothing) res

    failMsg = "currentUser: Expected Nothing as the current user, " ++
              " but didn't."

------------------------------------------------------------------------------
testCurrentUserOK :: Test
testCurrentUserOK = testCase "successful currentUser call" assertion
  where
    assertion :: Assertion
    assertion = do
        res <- evalHandler Nothing (ST.get "" M.empty) hdl appInit
        either (assertFailure . show) (assertBool failMsg . isJust) res

    hdl :: Handler App App (Maybe AuthUser)
    hdl = with auth $ do
        res <- loginByUsername "foo" (ClearText "foo") True
        either (\_ -> return Nothing) (\_ -> currentUser) res

    failMsg = "currentUser: Expected to get the current user, " ++
              " but didn't."
