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
        _            -> Prelude.putStrLn "bad command line"
