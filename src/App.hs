{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module App where

import Blaze.ByteString.Builder (toByteString)

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import qualified Crypto.Scrypt as CS

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Encoding as TE

import Database.Persist (insert)
import qualified Database.Persist.Sqlite as Sq

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase, contains, (>->), (<|>))

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Hamlet as TH

import Web.Cookie
import Web.ClientSession (getDefaultKey, encryptIO, decrypt)
import Web.Scotty (middleware, redirect, html, ActionM, param, ScottyM, setHeader, reqHeader)
import qualified Web.Scotty as WS

import Model
import Route

-- Operations about cookies.
makeCookie :: BS.ByteString -> BS.ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v }

renderSetCookie' :: SetCookie -> T.Text
renderSetCookie' = TE.decodeUtf8 . toByteString . renderSetCookie

setCookie :: BS.ByteString -> BS.ByteString -> ActionM ()
setCookie n v = setHeader "Set-Cookie" (fromStrict . renderSetCookie' $ makeCookie n v)

getCookies :: ActionM (Maybe CookiesText)
getCookies =
  fmap (fmap (parseCookiesText . TE.encodeUtf8 . toStrict)) $ reqHeader "Cookie"

getNonce :: String -> IO BS.ByteString
getNonce user = do
  key <- getDefaultKey
  encryptIO key (B8.pack user)

-- Helper functions.
get :: MyRoute -> ActionM () -> ScottyM ()
get route action = WS.get (fromString . T.unpack $ getPath route) action

post :: MyRoute -> ActionM () -> ScottyM ()
post route action = WS.post (fromString . T.unpack $ getPath route) action

-- Receive cookies and returns user_name.
authed :: Maybe CookiesText -> IO (Maybe String)
authed cookies = case cookies of
  Nothing -> return Nothing
  Just cookies' ->
    if null cookies'
    then return Nothing
    else do
      let (_, enc) = head cookies'
      key <- getDefaultKey
      let user' = decrypt key (TE.encodeUtf8 enc)
      case user' of
        Nothing -> return Nothing
        Just user'' -> do
          return $ Just $ T.unpack (TE.decodeUtf8 user'')

-- get user entity
getUser :: String -> ActionM User
getUser name = do
  entries <- liftIO (Sq.runSqlite "dev.sqlite3" (Sq.selectList [UserName Sq.==. name] [Sq.LimitTo 1]))
             :: ActionM [Sq.Entity User]
  return $ Sq.entityVal (head entries)

-- App.
app:: ScottyM ()
app = do
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static" >-> (contains "/js/" <|> contains "/css/")

  get Home $ do
    html $ renderHtml $ $(TH.hamletFile "./template/index.hamlet") render

  get List $ do
    cookies' <- getCookies
    user' <- liftIO $ authed cookies'
    case user' of
      Nothing -> redirect "/" -- not login-ed
      Just user -> do
        services' <- liftIO (Sq.runSqlite "dev.sqlite3" (Sq.selectList [ServiceUser Sq.==. user] []))
                    :: ActionM [Sq.Entity Service]
        let services = map (serviceName . Sq.entityVal) services'
        html $ renderHtml $ $(TH.hamletFile "./template/list.hamlet") render

  post List $ do
    cookies' <- getCookies
    user' <- liftIO $ authed cookies'
    service <- param "service" :: ActionM String
    case user' of
      Nothing -> redirect "/" -- not login-ed
      Just user_name -> do
        user <- getUser user_name
        _ <- liftIO $ Sq.runSqlite "dev.sqlite3" $ do
          insert $ Service service (userName user)
        redirect "/list"

  get Logout $ do
    setCookie "user" ""
    redirect "/"

  get Register $ do
    html $ renderHtml $ $(TH.hamletFile "./template/register.hamlet") render

  -- register new user
  post Register $ do
    user_id <- param "user_id" :: ActionM String
    pass <- liftM (CS.Pass . B8.pack) $ param "password"
    encryptedPass <- liftIO $ CS.encryptPassIO' pass
    _ <- liftIO $ Sq.runSqlite "dev.sqlite3" $ do
      insert $ User user_id (CS.getEncryptedPass encryptedPass)
    redirect "/"

  -- try to login
  post Login $ do
    user_id <- param "user_id" :: ActionM String
    pass <- liftM (CS.Pass . B8.pack) $ param "password"
    user <- liftIO (Sq.runSqlite "dev.sqlite3" (Sq.selectList [UserName Sq.==. user_id] [Sq.LimitTo 1]))
            :: ActionM ([Sq.Entity User])
    if null user
      then redirect "/" -- failed
      else do
      let user' = Sq.entityVal (head user)
      if CS.verifyPass' pass (CS.EncryptedPass $ userEncryptedPass user')
        then do nonce <- liftIO $ getNonce (userName user')
                setCookie "user" nonce
                redirect "/list"
        else redirect "/" -- failed
