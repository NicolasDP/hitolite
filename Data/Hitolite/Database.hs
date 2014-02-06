{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Hitolite.Database
    ( UsersGeneric(..)
    , insertHitUser
    , deleteHitUser
    , selectHitUser
    , createHitTable
    ) where

import Data.ByteString.Char8 as BS
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateTables"]
      [persistLowerCase|
Users
    login   BS.ByteString
    pubKey  BS.ByteString
    deriving Show
|]

runDB f = runSqlite "/tmp/hitolite.sqlite" f

insertHitUser userName userKey =
    runDB $ do
        deleteWhere $ [UsersLogin ==. userName]
        insertUnique $ Users userName userKey

selectHitUser userName =
    runDB $ selectList [UsersLogin ==. userName] []

deleteHitUser userName =
    runDB $ deleteWhere $ [UsersLogin ==. userName]

createHitTable = runDB $ runMigrationSilent migrateTables
