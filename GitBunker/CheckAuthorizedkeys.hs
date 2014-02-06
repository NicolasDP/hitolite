{-# LANGUAGE OverloadedStrings #-}
import Data.GitBunker.Database
import Database.Persist

import Data.ByteString.Char8 as BS

import System.Posix.Env.ByteString

import System.Log.Logger
import System.Log.Handler.Syslog

showKeysFrom dbname = do
    listRes <- selectAuthorizedKeys dbname
    Prelude.mapM_ showKeyOf listRes
    where
        showKeyOf ent =
            let val = entityVal ent
                cmd = if (BS.null $ userCommand val) then BS.empty
                                                     else BS.concat [" command=\"",userCommand val,"\""]
            in  do warningM
                       ("database(" ++ (BS.unpack dbname) ++ ")")
                       ("#name(" ++ (BS.unpack $ userName val) ++ ")")
                   BS.putStrLn $ BS.concat [userPubKey val,cmd]

main = do
    s <- openlog "CheckAuthorizedKey" [PID] USER WARNING
    updateGlobalLogger rootLoggerName (addHandler s)
    args <- getArgs
    case args of
        [dbname] -> do warningM "check pubkey" ("#" ++ (unpack dbname))
                       showKeysFrom dbname
        _        -> errorM "Bad command line" (show args)
