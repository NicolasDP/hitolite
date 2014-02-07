{-# LANGUAGE OverloadedStrings #-}
import Data.GitBunker.Hitolite
import Database.Persist

import Data.ByteString.Char8 as BS

import System.Posix.Env.ByteString

showListUser = do
    listUser <- listHitoliteUser
    Prelude.mapM_ showAUser listUser
    where
        showAUser ent =
            let u = entityVal ent
                uk = unKey $ entityKey ent
            in  Prelude.putStrLn $ "id(" ++ (show uk) ++ ") "
                                ++ "user(" ++ (BS.unpack $ userName u) ++ ")"

showListGroupe = do
    listGroupe <- listHitoliteGroupe
    Prelude.mapM_ showAGroupe listGroupe
    where
        showAGroupe ent =
            let u = entityVal ent
                uk = unKey $ entityKey ent
            in  Prelude.putStrLn $ "id(" ++ (show uk) ++ ") "
                                ++ "groupe(" ++ (BS.unpack $ groupeName u) ++ ") "
                                ++ "owner(" ++ (show $ unKey $ groupeOwner u) ++ ")"

showListUserInGroupe name = do
    listUser <- listUserInGroupe name
    Prelude.mapM_ showAUser listUser
    where
        showAUser ent =
            let u = entityVal ent
                uk = unKey $ entityKey ent
            in  Prelude.putStrLn $ "id(" ++ (show uk) ++ ") "
                                ++ "user(" ++ (BS.unpack $ userName u) ++ ")"

main = do
    args <- getArgs
    case args of
        ["create"]           -> do _ <- createHitoliteTable
                                   return ()
        ["user","add",name]  -> do _ <- createHitoliteUser name
                                   return ()
        ["user","del",name]  -> do _ <- deleteHitoliteUser name
                                   return ()
        ["user","list"]      -> showListUser
        ["groupe","add",grpname,name]  -> do _ <- createHitoliteGroupe grpname name
                                             return ()
        ["groupe","del",name]  -> do _ <- deleteHitoliteGroupe name
                                     return ()
        ["groupe",grpname,"add",uname]  -> do _ <- attachUserToGroupe uname grpname True True
                                              return ()
        ["groupe",grpname,"del",uname]  -> do _ <- deleteUserFromGroupe uname grpname
                                              return ()
        ["groupe","list"]      -> showListGroupe
        ["groupe","list",name] -> showListUserInGroupe name
        _            -> Prelude.putStrLn "bad command line"
