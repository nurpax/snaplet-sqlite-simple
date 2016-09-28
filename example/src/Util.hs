{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Util (
    reader
  , logFail
  , logRunExceptT
  , liftEither  
  ) where

import           Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Snap.Core
import           Snap.Snaplet
import           Application

--------------------------------------------------------------------------------

type H = Handler App App

reader :: T.Reader a -> T.Text -> Either String a
reader p s =
  case p s of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left e

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error case anyway.
logFail :: Either String (H ()) -> H ()
logFail = either (logError . T.encodeUtf8 . T.pack) id

logRunExceptT :: ExceptT String H (H ()) -> H ()
logRunExceptT e = runExceptT e >>= logFail

liftEither :: (Monad m, MonadError a (Either a)) => Either a b -> ExceptT a m b
liftEither = either throwError return
