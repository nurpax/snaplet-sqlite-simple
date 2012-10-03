{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model (
    User(..)
  , Comment(..)
  , createTables
  , saveComment
  , listComments) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application

data User = User Int T.Text

data Comment = Comment
  {
    commentId :: Int
  , commentSavedOn :: UTCTime
  , commentText :: T.Text
  } deriving (Show)

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field

tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

createTables :: S.Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "comments"
  unless schemaCreated $ do
    S.execute_ conn
      (S.Query $
       T.concat [ "CREATE TABLE comments ("
                , "id INTEGER PRIMARY KEY, "
                , "user_id INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "comment TEXT)"])

listComments :: User -> Handler App Sqlite [Comment]
listComments (User uid _) =
  query "SELECT id,saved_on,comment FROM comments WHERE user_id = ?" (Only uid)

saveComment :: User -> T.Text -> Handler App Sqlite ()
saveComment (User uid _) c =
  execute "INSERT INTO comments (user_id,comment) VALUES (?,?)" (uid, c)
