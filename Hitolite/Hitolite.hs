{-# LANGUAGE OverloadedStrings #-}
import Data.Hitolite.GitCommand

import Data.ByteString.Char8

import System.Posix.Env.ByteString
import System.Posix.Process.ByteString

import System.Log.Logger
import System.Log.Handler.Syslog

findIn :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
findIn _   []         = Nothing
findIn key ((k,c):xs) = if (key == k) then Just c else findIn key xs

main = do
    s <- openlog "Hitolite" [PID] USER DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)
    userName <- getArgs
    envs <- getEnvironment
    case findIn "SSH_ORIGINAL_COMMAND" envs of
        Nothing   -> Prelude.putStrLn "error, command not found"
        Just ocmd -> do let theCmd = commandLineParser ocmd
                        warningM
                           ("user(" ++ (unpack $ Prelude.head userName) ++ ")")
                           (show theCmd)
                        executeFile (gitCmd theCmd) True (gitCmdArgs theCmd) (Just envs)
