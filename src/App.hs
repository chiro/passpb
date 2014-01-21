{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module App where

import Blaze.ByteString.Builder (toByteString)

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import qualified Crypto.Scrypt as CS

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
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
import Web.Scotty

import Model
import Route

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

app:: ScottyM ()
app = do
  middleware logStdoutDev
  middleware $ staticPolicy $ addBase "static" >-> (contains "/js/" <|> contains "/css/")

  get "/" $ do
    html $ renderHtml $ $(TH.hamletFile "./template/index.hamlet") render

  get "/list" $ do
    cookies' <- getCookies
    case cookies' of
      Nothing -> redirect "/" -- not login-ed
      Just cookies ->
        if null cookies
        then redirect "/" -- not login-ed
        else do
          let (_, enc) = head cookies
          key <- liftIO getDefaultKey
          let user' = decrypt key (TE.encodeUtf8 enc)
          case user' of
            Nothing -> redirect "/" -- not login-ed
            Just user'' -> do
              let user = TE.decodeUtf8 user''
              html $ renderHtml $ $(TH.hamletFile "./template/list.hamlet") render

  get "/logout" $ do
    setCookie "user" ""
    redirect "/"

  get "/register" $ do
    html $ renderHtml $ $(TH.hamletFile "./template/register.hamlet") render

  -- register new user
  post "/register" $ do
    user_id <- param "user_id" :: ActionM String
    pass <- liftM (CS.Pass . B8.pack) $ param "password"
    encryptedPass <- liftIO $ CS.encryptPassIO' pass
    liftIO $ Sq.runSqlite "dev.sqlite3" $ do
      insert $ User user_id (CS.getEncryptedPass encryptedPass)
    redirect "/"

  -- try to login
  post "/login" $ do
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
