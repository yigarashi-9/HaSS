import Control.Exception
import System.Environment
import System.FilePath
import System.IO
import System.Exit
import Data.Bits
import Data.Int
import Parser
import Syntax

main :: IO ()
main = do
  args <- getArgs
  catch (do
          if length args /= 1
          then error "number of arguments must be 1"
          else createMifFile $ head args) err
    where
      err e = do
           hPutStrLn stderr ("Error" ++ show (e :: SomeException))
           exitFailure

createMifFile :: FilePath -> IO ()
createMifFile path = do
  (insts, len) <- parseFile path
  let machineLang = assemble insts
      mifPath     = dropExtension path ++ ".mif"
  withFile mifPath WriteMode $ \handle ->
      do
        mapM_ (hPutStrLn handle) mifHeader
        mapM_ (hPutStrLn handle) (zipWith bond [0..len-1] machineLang)
        hPutStrLn handle $ concat ["[", show len, "..2048]:", replicate 16 '0', ";"]
        hPutStrLn handle "END;"
      where mifHeader = ["WITDH = 16;"
                        ,"DEPTH = 2048;"
                        ,"ADDRESS_RADIX = DEC;"
                        ,"DATA_RADIX    = BIN;"
                        ,"CONTENT BEGIN"]
            bond addr dat = concat [show addr, ":", dat, ";"]

assemble :: [Instruction] -> [String]
assemble = foldr (\i acc -> (conv i):acc) []

conv :: Instruction -> String
conv (Prim  op rd rs) = concat ["11", dec2bin rs 3, dec2bin rd 3, convArithOp op, "0000"]
conv (Shift op rd d)  = concat ["11", "000",        dec2bin rd 3, convShiftOp op, dec2bin d 4]
conv (Input rd)       = concat ["11", "000",        dec2bin rd 3, "1100",         "0000"]
conv (Output rs)      = concat ["11", dec2bin rs 3, "000",        "1101",         "0000"]
conv (Halt)           = "1100000011110000"
conv (Load ra d rb)   = concat ["00", dec2bin ra 3, dec2bin rb 3, dec2bin d 8]
conv (Store ra d rb)  = concat ["01", dec2bin ra 3, dec2bin rb 3, dec2bin d 8]
conv (LoadIm rb d)    = concat ["10", "000",        dec2bin rb 3, dec2bin d 8]
conv (UncondBr d)     = concat ["10", "100",        "000",        dec2bin d 8]
conv (CondBr op d)    = concat ["10", "111",        convBrOp op,  dec2bin d 8]

convArithOp :: String -> String
convArithOp "ADD" = "0000"
convArithOp "SUB" = "0001"
convArithOp "AND" = "0010"
convArithOp "OR"  = "0011"
convArithOp "XOR" = "0100"
convArithOp "CMP" = "0101"
convArithOp "MOV" = "0110"

convShiftOp :: String -> String
convShiftOp "SLL" = "1000"
convShiftOp "SLR" = "1001"
convShiftOp "SRL" = "1010"
convShiftOp "SRA" = "1011"

convBrOp :: String -> String
convBrOp "BE"  = "000"
convBrOp "BLT" = "001"
convBrOp "BLE" = "010"
convBrOp "BNE" = "011"
convBrOp _     = "111"

dec2bin :: Int16 -> Int -> String
dec2bin n len = lastN len $
                foldr (\i acc -> (head . show $ (n `shiftR` i) .&. 1):acc) "" [15,14..0]

zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs
