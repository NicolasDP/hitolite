{-# LANGUAGE OverloadedStrings #-}
module Data.Hitolite.GitCommand
    ( GitCommand(..)
    , commandLineParser
    ) where

import Data.ByteString.Char8 (ByteString, split, empty)

data GitCommand = GitCommand
    { gitType    :: GitCommandType
    , gitCmd     :: ByteString
    , gitCmdArgs :: [ByteString]
    } deriving (Eq, Show)

data GitCommandType = GitUploadPack
                    | GitUploadArchive
                    | GitUnknown
    deriving (Eq, Show)

commandLineParser :: ByteString -> GitCommand
commandLineParser cl =
    let list = filter ((/=) empty) $ split ' ' cl
    in  GitCommand (getCommandType $ head list) (head list) (tail list)
    where
        getCommandType :: ByteString -> GitCommandType
        getCommandType "git-upload-pack" = GitUploadPack
        getCommandType _                 = GitUnknown
