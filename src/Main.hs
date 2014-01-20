{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getEnv)

import Control.Monad (liftM)

import qualified Network.Wai.Middleware.RequestLogger as Logger

import qualified Web.Scotty as WS

main :: IO ()
main = do
  port <- liftM read $ getEnv "PORT"
  WS.scotty port $ do
    WS.middleware Logger.logStdoutDev

    WS.get "/" $ do
      WS.text "Under constructing"
