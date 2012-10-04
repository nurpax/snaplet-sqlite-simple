{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import           Data.Lens.Common (getL)
import           Data.Maybe
import           Data.Pool (withResource)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import           Heist()
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import qualified Db

type H = Handler App (AuthManager App)

-- | Render login form
handleLogin :: Maybe T.Text -> H ()
handleLogin authError =
  heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]

-- | Handle login submit
handleLoginSubmit :: H ()
handleLoginSubmit =
  loginUser "login" "password" Nothing
    (\_ -> handleLogin err)
    (redirect "/")
  where
    err = Just "Unknown user or password"

-- | Logs out and redirects the user to the site index.
handleLogout :: H ()
handleLogout = logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: H ()
handleNewUser =
  method GET (renderNewUserForm Nothing) <|> method POST handleFormSubmit
  where
    handleFormSubmit = do
      authUser <- registerUser "login" "password"
      either (renderNewUserForm . Just) (\_ -> redirect "/") authUser

    renderNewUserForm (err :: Maybe AuthFailure) =
      heistLocal (I.bindSplices errs) $ render "new_user"
      where
        errs = [("newUserError", I.textSplice . T.pack . show $ c) | c <- maybeToList err]

-- | Run actions with a logged in user or set error
withLoggedInUser :: (Db.User -> H ()) -> H ()
withLoggedInUser action = do
  currentUser >>= go
  where
    go Nothing = handleLogin (Just "Must be logged in to view the main page")
    go (Just u) =
      maybe
        (return ())
        (\uid -> action (user uid)) (userId u)
        where
          user uid = Db.User (read . T.unpack $ unUid uid) (userLogin u)

renderComment :: Monad m => Db.Comment -> I.Splice m
renderComment (Db.Comment _ saved text) = do
  I.runChildrenWithText [ ("savedOn", T.pack . show $ saved)
                        , ("comment", text)]

handleCommentSubmit :: H ()
handleCommentSubmit = method POST (withLoggedInUser go)
  where
    go user = do
      c <- getParam "comment"
      maybe (return ()) (\t -> withTop db (Db.saveComment user (T.decodeUtf8 t))) c
      redirect "/index"

-- | Render main page
mainPage :: H ()
mainPage = withLoggedInUser go
  where
    go :: Db.User -> H ()
    go user = do
      comments <- withTop db $ Db.listComments user
      heistLocal (splices comments) $ render "/index"
    splices cs =
      I.bindSplices [("comments", I.mapSplices renderComment cs)]

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",        with auth handleLoginSubmit)
         , ("/logout",       with auth handleLogout)
         , ("/new_user",     with auth handleNewUser)
         , ("/save_comment", with auth handleCommentSubmit)
         , ("/",             with auth mainPage)
         , ("/static",       serveDirectory "static")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    -- addRoutes must be called before heistInit - heist wants to
    -- serve "" itself which means our mainPage handler never gets a
    -- chance to get called.
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- Initialize auth that's backed by an sqlite database
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let connPool = sqlitePool $ getL snapletValue d
    liftIO $ withResource connPool $ \conn -> Db.createTables conn

    addAuthSplices auth
    return $ App h s d a

