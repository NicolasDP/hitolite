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
    , listHitoliteGroupe
    , deleteHitoliteGroupe
    , -- ** Link between Users and Groupes
      attachUserToGroupe
    , deleteUserFromGroupe
    , listUserInGroupe
    {-
    , -- ** Project
      createHitoliteProject -}
    ) where

import qualified Data.ByteString.Char8   as BS
import qualified Data.List               as L
import qualified Database.Esqueleto      as E
import qualified Database.Persist.Sqlite as DP
import qualified Database.Persist.TH     as DP

data GitCommand = GitCommand
    { gitCmd     :: BS.ByteString
    , gitCmdArgs :: [BS.ByteString]
    } deriving (Eq, Show)

commandLineParser :: BS.ByteString -> GitCommand
commandLineParser cl =
    let list = L.filter ((/=) BS.empty) $ BS.split ' ' cl
    in  GitCommand (L.head list) (L.map removeUselessQuotes $ L.tail list)
    where
        removeUselessQuotes :: BS.ByteString -> BS.ByteString
        removeUselessQuotes s =
            let s1 = if ((BS.head s) == '\'') then BS.drop 1 s else s
            in if ((BS.last s1) == '\'') then BS.take ((BS.length s1) - 1) s1 else s1


DP.share [DP.mkPersist DP.sqlSettings, DP.mkMigrate "migrateTables"]
      [DP.persistLowerCase|
User
    name       BS.ByteString
    UniqueUser name
    deriving Show
Groupe
    name         BS.ByteString
    owner        UserId
    UniqueGroupe name
    deriving Show
Project
    name          BS.ByteString
    owner         UserId
    deriving Show

UsrGrp
    userId   UserId
    grpId    GroupeId
    read     Bool
    write    Bool
    deriving Show
UsrPrj
    userId   UserId
    prjId    ProjectId
    read     Bool
    write    Bool
    deriving Show
GrpPrj
    grpId    GroupeId
    prjId    ProjectId
    read     Bool
    write    Bool
    deriving Show
|]


runDB f = DP.runSqlite "/var/lib/hitoliteDB/hitolite.sqlite" f

createHitoliteTable = runDB $ DP.runMigration migrateTables

-- | Create a new UNIQUE user (return Nothing is not created)
createHitoliteUser name =
    runDB $ DP.insertUnique $ User name

-- | List all user in the database
listHitoliteUser =
    runDB $ E.select $
            E.from $ \person -> return person
-- | delete auser
-- XXX: should we
--  * delete all his projects and groupes?
deleteHitoliteUser name =
    runDB $ do
        userDB <- DP.selectFirst [UserName DP.==. name] []
        case userDB of
            Nothing -> return ()
            Just (DP.Entity uKey _) -> do
               DP.deleteWhere [UserId DP.==. uKey]
               DP.deleteWhere [UsrGrpUserId DP.==. uKey]

-- | Create a new UNIQUE Groupe
createHitoliteGroupe grpName ownerName = do
    runDB $ do
        ownerDB <- DP.selectFirst [UserName DP.==. ownerName] []
        case ownerDB of
            Just (DP.Entity k _) -> DP.insertUnique $ Groupe grpName k
            Nothing  -> return Nothing -- Throw an error maybe???

listHitoliteGroupe =
    runDB $ E.select $
            E.from $ \groupe -> return groupe

deleteHitoliteGroupe name =
    runDB $ do
        grpeDB <- DP.selectFirst [GroupeName DP.==. name] []
        case grpeDB of
            Nothing -> return ()
            Just (DP.Entity ugKey _) -> do
               DP.deleteWhere [GroupeId DP.==. ugKey]
               DP.deleteWhere [UsrGrpGrpId DP.==. ugKey]

-- | Attach a user to a group
attachUserToGroupe theUserName theGroupeName prvRead prvWrite =
    runDB $ do
        userDB <- DP.selectFirst [UserName DP.==. theUserName] []
        grpeDB <- DP.selectFirst [GroupeName DP.==. theGroupeName] []
        case (userDB,grpeDB) of
            (Just (DP.Entity uk _) ,Just (DP.Entity ug _) ) -> DP.insertUnique $ UsrGrp uk ug prvRead prvWrite
	    _ -> return Nothing -- Throw an error maybe???

listUserInGroupe theGroupeName =
    runDB $ E.select $
            E.from   $ \(u, ug, g) -> do
            E.where_ (u E.^. UserId     E.==. ug E.^. UsrGrpUserId E.&&.
                      g E.^. GroupeId   E.==. ug E.^. UsrGrpGrpId E.&&.
                      g E.^. GroupeName E.==. E.val theGroupeName)
            return u

deleteUserFromGroupe theUserName theGroupeName =
    runDB $ do
        userDB <- DP.selectFirst [UserName DP.==. theUserName] []
        grpeDB <- DP.selectFirst [GroupeName DP.==. theGroupeName] []
        case (userDB,grpeDB) of
            (Just (DP.Entity uk _) ,Just (DP.Entity ugk _) ) -> DP.deleteWhere [UsrGrpGrpId DP.==. ugk, UsrGrpUserId DP.==. uk]
            _ -> return () -- Throw an error maybe???

{-
-- | Create a new project
createHitoliteProject prjName ownerName =
    runDB $ do
        ownerDB <- selectFirst [UserName ==. ownerName] []
        case ownerDB of
            Just odb -> insertUnique $ Project prjName (userName $ entityVal odb)
            Nothing  -> return Nothing -- Throw an error maybe???

-}
