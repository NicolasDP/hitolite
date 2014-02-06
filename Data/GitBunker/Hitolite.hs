{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.GitBunker.Hitolite
    ( -- * Command line parser
      GitCommand(..)
    , commandLineParser
      -- * Database
      -- ** Data
    , UserGeneric(..)
    , GroupeGeneric(..)
    , ProjectGeneric(..)
    , -- ** Table
      createHitoliteTable
    , -- ** User
      createHitoliteUser
    , listHitoliteUser
    , deleteHitoliteUser
    , -- ** Group
      createHitoliteGroupe
    , -- ** Project
      createHitoliteProject
    , -- ** Linker
      attachUserToGroup
    ) where

import Data.ByteString.Char8 as BS
import Data.List             as L
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

data GitCommand = GitCommand
    { gitCmd     :: BS.ByteString
    , gitCmdArgs :: [BS.ByteString]
    } deriving (Eq, Show)

commandLineParser :: BS.ByteString -> GitCommand
commandLineParser cl =
    let list = L.filter ((/=) BS.empty) $ BS.split ' ' cl
    in  GitCommand (L.head list) (L.map removeUselessQuotes $ L.tail list)
    where
        removeUselessQuotes :: ByteString -> ByteString
        removeUselessQuotes s =
            let s1 = if ((BS.head s) == '\'') then BS.drop 1 s else s
            in if ((BS.last s1) == '\'') then BS.take ((BS.length s1) - 1) s1 else s1


share [mkPersist sqlSettings, mkMigrate "migrateTables"]
      [persistLowerCase|
User
    name       BS.ByteString
    UniqueUser name
    deriving Show
Groupe
    name         BS.ByteString
    owner        BS.ByteString
    UniqueGroupe name
    deriving Show
Project
    name          BS.ByteString
    owner         BS.ByteString
    deriving Show

UsrGrp
    userId   BS.ByteString
    grpId    BS.ByteString
    read     Bool
    write    Bool
    deriving Show
UsrPrj
    userId   BS.ByteString
    prjId    ProjectId
    read     Bool
    write    Bool
    deriving Show
GrpPrj
    grpId    BS.ByteString
    prjId    ProjectId
    read     Bool
    write    Bool
    deriving Show
|]


runDB f = runSqlite "/var/lib/hitoliteDB/hitolite.sqlite" f

createHitoliteTable = runDB $ runMigration migrateTables

-- | Create a new UNIQUE user (return Nothing is not created)
createHitoliteUser name =
    runDB $ insertUnique $ User name

-- | List all user in the database
listHitoliteUser =
    runDB $ selectList [] []

-- | delete auser
-- XXX: should we
--  * delete all his projects and groupes?
deleteHitoliteUser name =
    runDB $ deleteWhere [UserName ==. name]

-- | Create a new UNIQUE Groupe
createHitoliteGroupe grpName ownerName =
    runDB $ do
        ownerDB <- selectFirst [UserName ==. ownerName] []
        case ownerDB of
            Just odb -> insertUnique $ Groupe grpName (userName $ entityVal odb)
            Nothing  -> return Nothing -- Throw an error maybe???

-- | Create a new project
createHitoliteProject prjName ownerName =
    runDB $ do
        ownerDB <- selectFirst [UserName ==. ownerName] []
        case ownerDB of
            Just odb -> insertUnique $ Project prjName (userName $ entityVal odb)
            Nothing  -> return Nothing -- Throw an error maybe???

-- | Attach a user to a group
attachUserToGroup theUserName theGroupeName prvRead prvWrite =
    runDB $ do
        userDB <- selectFirst [UserName ==. theUserName] []
        grpeDB <- selectFirst [GroupeName ==. theGroupeName] []
        case (userDB,grpeDB) of
            (Nothing,_      ) -> return Nothing -- Throw an error maybe???
            (_      ,Nothing) -> return Nothing -- Throw an error maybe???
            (Just _ ,Just _ ) -> insertUnique $ UsrGrp theUserName theGroupeName prvRead prvWrite
