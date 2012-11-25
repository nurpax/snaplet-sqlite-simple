{-# LANGUAGE OverloadedStrings #-}

module Tests
  ( tests ) where


------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
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
tests = testGroup "Snap.Snaplet.SqliteSimple"
    [mutuallyExclusive $
       testGroup "createUser tests" [testCreateUserGood]]

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
      either (assertFailure . show) (assertBool failMsg . isRight) res

    failMsg = "createUser failed: Couldn't create a new user."
