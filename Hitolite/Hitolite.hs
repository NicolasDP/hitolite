import System.Environment
import System.Process

findIn :: String -> [(String, String)] -> Maybe String
findIn _   []         = Nothing
findIn key ((k,c):xs) = if (key == k) then Just c else findIn key xs

main = do
    userName <- getArgs
    envs <- getEnvironment
    case findIn "SSH_ORIGINAL_COMMAND" envs of
        Nothing   -> putStrLn "error, command not found"
        Just ocmd -> do returnCode <- system ocmd
                        putStrLn $ show returnCode
