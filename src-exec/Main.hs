import System.Environment
import System.IO
import System.Exit
import Control.Exception
import Parser
import Simulator
import Assembler

main :: IO ()
main = do
  args <- getArgs
  catch ( if length args < 0
          then error "Usage: \"-s <program> <data>\" or \"-a <program>\""
          else case head args of
                 "-s" -> simulator (tail args)
                 "-a" -> assembler (tail args)
                 _    -> error "Usage: \"-s <program> <data>\" or \"-a <program>\""
        ) err
    where
      err e = do
         hPutStrLn stderr ("Error" ++ show (e :: SomeException))
         exitFailure

assembler :: [String] -> IO ()
assembler args = if length args /= 1
                 then error "number of arguments must be 1"
                 else createMifFile $ head args

simulator :: [String] -> IO ()
simulator args = do
  if length args /= 2
  then do
    hPutStrLn stderr $ "Error: 2 arguments expected"
    exitFailure
  else do
    insts  <- parseFile (args !! 0)  -- assembly file name
    result <- run insts (args !! 1)  -- memory   file name
    print result
