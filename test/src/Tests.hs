{-# LANGUAGE OverloadedStrings #-}

module Tests
  ( tests ) where


------------------------------------------------------------------------------
import qualified Data.Map as M
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

------------------------------------------------------------------------------
import           App
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import qualified Snap.Test as ST
import           Snap.Snaplet.Test


------------------------------------------------------------------------------
tests :: Test
tests = mutuallyExclusive $ testGroup "Snap.Snaplet.SqliteSimple"
    [ testGroup "createUser tests" [testCreateUserGood]
    , testGroup "update user tests" [testUpdateUser]
    ]

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

------------------------------------------------------------------------------
testCreateUserGood :: Test
testCreateUserGood = testCase "createUser good params" assertGoodUser
  where
    assertGoodUser :: Assertion
    assertGoodUser = do
      let hdl = with auth $ createUser "foo" "foo"
      res <- evalHandler (ST.get "" M.empty) hdl appInit
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

------------------------------------------------------------------------------
-- Create a user, modify it, persist it and load again, check fields ok.
-- Must run after testCreateUserGood

testUpdateUser :: Test
testUpdateUser = testCase "createUser + update good params" assertGoodUser
  where
    assertGoodUser :: Assertion
    assertGoodUser = do
      let loginHdl = with auth $ loginByUsername "foo" (ClearText "foo") True
      res <- evalHandler (ST.get "" M.empty) loginHdl appInit
      either (assertFailure . show) checkLoggedInUser res

    checkLoggedInUser (Left _) = assertBool "failed login" False
    checkLoggedInUser (Right u) = do
      assertEqual "login count"  1 (userLoginCount u)
      assertEqual "fail count"   0 (userFailedLoginCount u)
      assertEqual "locked until" Nothing (userLockedOutUntil u)
      assertEqual "local host ip" (Just "127.0.0.1") (userCurrentLoginIp u)
      assertEqual "no previous login" Nothing (userLastLoginIp u)
      let saveHdl = with auth $ saveUser (u { userLogin = "bar" })
      res <- evalHandler (ST.get "" M.empty) saveHdl appInit
      either (assertFailure . show) checkUpdatedUser res

    checkUpdatedUser (Left _) = assertBool "failed saveUser" False
    checkUpdatedUser (Right u) = do
      assertEqual "login rename ok?"  "bar" (userLogin u)
      assertEqual "login count"  1 (userLoginCount u)
      assertEqual "local host ip" (Just "127.0.0.1") (userCurrentLoginIp u)
      assertEqual "local host ip" Nothing (userLastLoginIp u)
      let loginHdl = with auth $ loginByUsername "bar" (ClearText "foo") True
      res <- evalHandler (ST.get "" M.empty) loginHdl appInit
      either (assertFailure . show) (assertBool "login as 'bar' ok?" . isRight) res
