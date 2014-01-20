{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getEnv)

import Control.Monad (liftM)

import qualified Web.Scotty as WS

import App

main :: IO ()
main = do
  port <- liftM read $ getEnv "PORT"
  WS.scotty port app
