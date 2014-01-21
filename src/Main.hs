{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getEnv)

import Control.Monad (liftM)

import Database.Persist.Sqlite

import qualified Web.Scotty as WS

import Web.Passpb.App
import Web.Passpb.Crypt
import Web.Passpb.Model
import Config

main :: IO ()
main = do
  runSqlite dbFile $ do
    runMigration migrateAll
  port <- liftM read $ getEnv "PORT"
  WS.scotty port (app dbFile)
