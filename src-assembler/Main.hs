import System.Environment
import System.IO
import System.Exit
import Control.Exception
import Assembler

main :: IO ()
main = do
  args <- getArgs
  catch ( if length args /= 1
          then error "number of arguments must be 1\nUsage: hass-assembler <program>"
          else createMifFile $ head args
        ) err
    where
      err e = do
         hPutStrLn stderr ("Error: " ++ show (e :: SomeException))
         exitFailure
