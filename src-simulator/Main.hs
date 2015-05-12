import System.Environment
import System.IO
import System.Exit
import Control.Exception
import Parser
import Simulator

main :: IO ()
main = do
  args <- getArgs
  catch ( if length args /= 2
          then error "number of arguments must be 2.\nUsage: hass <program> <data>"
          else do
            insts  <- parseFile (args !! 0)  -- assembly file name
            result <- run insts (args !! 1)  -- memory   file name
            print result
        ) err
    where
      err e = do
         hPutStrLn stderr ("Error: " ++ show (e :: SomeException))
         exitFailure
