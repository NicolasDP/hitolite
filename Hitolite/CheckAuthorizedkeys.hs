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
            in  do warningM
                       ("user(" ++ (BS.unpack name) ++ ")")
                       ("key(" ++ (BS.unpack $ usersPubKey val) ++ ")")
                   BS.putStrLn $ usersPubKey val

main = do
    s <- openlog "CheckAuthorizedKey" [PID] USER DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)
    args <- getArgs
    case args of
        [name] -> do warningM "check pubkey" (unpack name)
                     showKeysFrom name
        _      -> errorM "Bad command line" (show args)
