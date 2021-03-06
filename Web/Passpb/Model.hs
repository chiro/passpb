{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Web.Passpb.Model where

import qualified Data.ByteString as B

import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    encryptedPass B.ByteString
    deriving Show
Service
    name String
    user String
    deriving Show
|]
