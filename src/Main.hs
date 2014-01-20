{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getEnv)

import Control.Monad (liftM)

import Database.Persist.Sqlite

import qualified Web.Scotty as WS

import App
import Model

main :: IO ()
main = do
  runSqlite "dev.sqlite3" $ do
    runMigration migrateAll
  port <- liftM read $ getEnv "PORT"
  WS.scotty port app
