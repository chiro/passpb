{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module App where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import qualified Crypto.Scrypt as CS

import qualified Data.ByteString.Char8 as B8

import Database.Persist (insert)
import qualified Database.Persist.Sqlite as Sq

import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Hamlet as TH

import Web.Scotty

import Model

app:: ScottyM ()
app = do
  middleware logStdoutDev

  get "/" $ do
    html $ renderHtml $ $(TH.hamletFile "./template/index.hamlet") undefined

  get "/register" $ do
    html $ renderHtml $ $(TH.hamletFile "./template/register.hamlet") undefined

  -- register new user
  post "/register" $ do
    user_id <- param "user_id" :: ActionM String
    pass <- liftM (CS.Pass . B8.pack) $ param "password"
    encryptedPass <- liftIO $ CS.encryptPassIO' pass
    liftIO $ Sq.runSqlite "dev.sqlite3" $ do
      insert $ User user_id (CS.getEncryptedPass encryptedPass)
    redirect "/"

  post "/login" $ do
    user_id <- param "user_id" :: ActionM String
    pass <- liftM (CS.Pass . B8.pack) $ param "password"
    user <- liftIO (Sq.runSqlite "dev.sqlite3" (Sq.selectList [UserName Sq.==. user_id] [Sq.LimitTo 1]))
            :: ActionM ([Sq.Entity User])
    if null user
      then redirect "/"
      else do
      let user' = Sq.entityVal (head user)
      if CS.verifyPass' pass (CS.EncryptedPass $ userEncryptedPass user')
        then text "login succeeded"
        else redirect "/"
