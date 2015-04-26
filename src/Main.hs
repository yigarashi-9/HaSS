import System.Environment
import System.IO
import System.Exit
import Parser
import Simulator

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then do
    hPutStrLn stderr $ "Error: 2 arguments expected"
    exitFailure
  else do
    insts  <- parseFile (args !! 0)  -- assembly file name
    result <- run insts (args !! 1)  -- memory   file name
    print result
