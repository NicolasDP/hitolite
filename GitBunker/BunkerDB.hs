{-# LANGUAGE OverloadedStrings #-}
import Data.GitBunker.Database
import Database.Persist

import Data.ByteString.Char8 as BS

import System.Posix.Env.ByteString

showKeysFrom dbname = do
    listRes <- selectAuthorizedKeys dbname
    Prelude.mapM_ showKeyOf listRes
    where
        showKeyOf ent =
            let val = entityVal ent
                keyId  = unKey $ entityKey ent
            in  Prelude.putStrLn $ "id(" ++ (show keyId) ++ ") "
                                ++ "user(" ++ (BS.unpack $ userName val) ++ ") "
                                 ++ "key(" ++ (BS.unpack $ userPubKey val) ++ ") "
                                 ++ "cmd(" ++ (BS.unpack $ userCommand val) ++ ")"

main = do
    args <- getArgs
    case args of
        ["create",dbname]           -> do _ <- createHitTable dbname
                                          return ()
        ["add",dbname,key,name,cmd] -> do _ <- insertHitUser dbname key name cmd
                                          return ()
        ["add",dbname,key,name]     -> do _ <- insertHitUser dbname key name BS.empty
                                          return ()
        ["get",name]                -> showKeysFrom name
        ["del",dbname,name]         -> do _ <- deleteHitUser dbname name
                                          return ()
        _            -> Prelude.putStrLn "bad command line"
