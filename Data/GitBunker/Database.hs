{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module      : Data.Hitolite.Database
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
-- Bunker's database common interface.
--
-- The idea is to create a database per login. So you can manage different
-- authorized keys for different user.
--
module Data.GitBunker.Database
    ( -- * User's key
      UserGeneric(..)

      -- * Initialization
    , createHitTable

      -- * Usual functions
    , insertHitUser
    , deleteHitUser
    , deleteHitUserKey
    , selectAuthorizedKeys
    ) where

import Data.ByteString.Char8 as BS
import Data.Text             as T (pack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- | The common entity
--
-- This represents a user's key.
--
-- The value 'userPubKey' must be the content of the plublic key (rsa.pub).
--
-- The 'userName' is a user alias you can use to named this key.
--
-- The 'userCommand' is the command to execute if the user try a connection. If
-- Nothing, then the original command will be execute.
share [mkPersist sqlSettings, mkMigrate "migrateTables"]
      [persistLowerCase|
User
    pubKey   BS.ByteString
    name     BS.ByteString
    command  BS.ByteString
    deriving Show
|]

runDB base f =
    runSqlite (T.pack ("/var/lib/bunkerDB/" ++ (BS.unpack base) ++ ".sqlite")) f

-- | create a new bunker database (or update the existing one) with the given
-- name
--
-- > createHitTable "nicolas"
createHitTable base = runDB base $ runMigration migrateTables

-- | Insert a new user in the given base
-- The following command insert a new key for user *vincent* in the base *nicolas* and
-- ask ssh-daemon to execute the command *bunker_checkruserright vincent*
--
-- > insertHitUser "nicolas" <vincent's key> "vincent" "bunker_checkuserright vincent"
insertHitUser base key name command =
    runDB base $ insert $ User key name command

-- | Get the list of authorized keys for the given base
--
-- > selectAuthorizedKeys "nicolas"
selectAuthorizedKeys base =
    runDB base $ selectList [] []

-- | delete all user's keys in the given base
--
-- > deleteHitUser "nicolas" "vincent"
deleteHitUser base name =
    runDB base $ deleteWhere $ [UserName ==. name]

-- | delete a user's key
--
-- > deleteHitUserKey "nicolas" (PersistInt64 19)
deleteHitUserKey base userId =
    runDB base $ deleteWhere $ [UserId ==. userId]
