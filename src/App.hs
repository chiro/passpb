{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module App where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Hamlet as TH

import qualified Web.Scotty as WS

app:: WS.ScottyM ()
app = do
  WS.middleware logStdoutDev

  WS.get "/" $ do
    WS.text "Under constructing"

  WS.get "/register" $ do
    WS.html $ renderHtml $ $(TH.hamletFile "./template/register.hamlet") undefined
