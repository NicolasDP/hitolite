{-# LANGUAGE OverloadedStrings #-}
import Data.Hitolite.GitCommand
import Data.Hitolite.Database
import Database.Persist

import Data.ByteString.Char8 as BS

import System.Posix.Env.ByteString
import System.Posix.Process.ByteString

import System.Log.Logger
import System.Log.Handler.Syslog

showKeysFrom name = do
    listRes <- selectHitUser name
    Prelude.mapM_ showKeyOf listRes
    where
        showKeyOf ent =
            let val = entityVal ent
            in  Prelude.putStrLn $ "user(" ++ (BS.unpack $ usersLogin val) ++ ") "
                                 ++ "key(" ++ (BS.unpack $ usersPubKey val) ++ ")"

main = do
    args <- getArgs
    case args of
        ["add",name,key] -> do _ <- insertHitUser name key
                               return ()
        ["get",name] -> showKeysFrom name
        ["del",name] -> do _ <- deleteHitUser name
                           return ()
        _            -> Prelude.putStrLn "bad command line"
