import System.Environment
import System.Process

main = do
  args <- getArgs
  originalCmd <- getEnv "SSH_ORIGINAL_COMMAND"
  system originalCmd
