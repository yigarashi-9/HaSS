module Simulator(run) where

import Control.Monad.State
import Control.Monad
import Control.Applicative
import Control.Exception
import Data.Array
import Data.Bits
import Data.Int
import System.IO
import Syntax

type Simulator = StateT SIMPLE IO

initialSimple :: [Instruction] -> String -> IO SIMPLE
initialSimple insts ramFile = do
  withFile ramFile ReadMode $ \handle ->
      do
        ramData  <- liftM ((map read) . lines) $ hGetContents handle
        ramSize_ <- evaluate $ length ramData
        let ramSize = (fromIntegral ramSize_) :: Int16
        return $ SIMPLE { pc = 0
                        , instruction = insts
                        , registerFile = array (0, 7) [(i, 0) | i <- [0..7]]
                        , ram     = array (0, ramSize-1) (zipWith (,) [0..] ramData)
                        , code_c  = False
                        , code_v  = False
                        , code_s  = False
                        , code_z  = False }

run :: [Instruction] -> String -> IO (Int, SIMPLE)
run insts ramFile = do
  simple <- initialSimple insts ramFile
  runStateT (runBody 0) simple

runBody :: Int -> Simulator Int
runBody cnt = do
  inst <- updatePC 1
  case inst of
    Nothing  -> return cnt
    (Just i) -> runInst i >> runBody (cnt+1)

updatePC :: Int16 -> Simulator (Maybe Instruction)
updatePC d = do
  simple <- get
  let insts   = instruction simple
      prcnt16 = pc simple
      prcnt   = (fromIntegral prcnt16) :: Int
  put $ simple { pc = prcnt16 + d }
  return $ if prcnt < 0 || prcnt >= length insts
           then Nothing
           else Just (insts !! prcnt)

runInst :: Instruction -> Simulator ()
runInst (Prim op rd rs) = primOp  op rd rs
runInst (Shift op rd d) = shiftOp op rd d
runInst (Input rd)      = liftIO getLine >>= (writeReg rd) . read
runInst (Output rs)     = do { v <- readReg rs;
                               liftIO $ print v }
runInst Nop             = return ()
runInst Halt            = do { s <- get;
                               put $ s { pc = -1 } }
runInst (Load ra d rb)  = loadMem ra d rb
runInst (Store ra d rb) = storeMem ra d rb
runInst (LoadIm rb d)   = setCodeLogic d >> writeReg rb d
runInst (AddI rb d)    = do {
                            v <- readReg rb;
                            setCodeLogic (v+d) >> writeReg rb (v+d) }
runInst (UncondBr d)    = updatePC d >> return ()
runInst (CondBr op d)   = condBrOp op d

readReg :: Int16 -> Simulator Int16
readReg rs = do
  regFile <- liftM registerFile get
  return $ regFile ! rs

loadMem :: Int16 -> Int16 -> Int16 -> Simulator ()
loadMem ra d rb = do
  simple <- get
  let regFile = registerFile simple
      dataRam = ram simple
  put $ simple { registerFile = regFile // [(ra, dataRam ! ((regFile ! rb)+d))] }

storeMem :: Int16 -> Int16 -> Int16 -> Simulator ()
storeMem ra d rb = do
  simple <- get
  let regFile = registerFile simple
      dataRam = ram simple
  put $ simple { ram = dataRam // [((regFile ! rb) + d, regFile ! ra)] }

(.^.) :: Bool -> Bool -> Bool
(.^.) a b = (not a && b) || (a && not b)

condBrOp :: String -> Int16 -> Simulator ()
condBrOp op d = do
  simple <- get
  let br = case op of "BE"  -> code_c simple
                      "BLT" -> code_s simple .^. code_v simple
                      "BLE" -> code_z simple || (code_s simple .^. code_v simple)
                      "BNE" -> not $ code_z simple
  when br (put $ simple { pc = pc simple + d })

primOp :: String -> Int16 -> Int16 -> Simulator ()
primOp op rd rs = do
  simple <- get
  let reg = registerFile simple
      rdv = reg ! rd
      rsv = reg ! rs
  case op of
    "ADD" -> let res = rdv + rsv   in (setCodeArith res rdv rsv)    >> writeReg rd res
    "SUB" -> let res = rdv - rsv   in (setCodeArith res rdv (-rsv)) >> writeReg rd res
    "AND" -> let res = rdv.&.rsv   in (setCodeLogic res)            >> writeReg rd res
    "OR"  -> let res = rdv.|.rsv   in (setCodeLogic res)            >> writeReg rd res
    "XOR" -> let res = xor rdv rsv in (setCodeLogic res)            >> writeReg rd res
    "CMP" -> let res = rdv - rsv   in (setCodeArith res rdv (-rsv)) >> return ()
    "MOV" -> let res = rsv         in (setCodeLogic res)            >> writeReg rd res

setCodeArith :: Int16 -> Int16 -> Int16 -> Simulator ()
setCodeArith res rdv rsv = do
  simple <- get
  put $ simple { code_s = res < 0
               , code_z = res == 0
               , code_c = ((rdv < 0 || rsv < 0) && res >= 0) || (rdv < 0 && rsv < 0)
               , code_v = (rdv >= 0 && rsv >= 0 && res <  0) ||
                          (rdv <  0 && rsv  < 0 && res >= 0) }

setCodeLogic :: Int16 -> Simulator ()
setCodeLogic res = do
  simple <- get
  put $ simple { code_s = res < 0
               , code_z = res == 0
               , code_c = False
               , code_v = False }

shiftOp :: String -> Int16 -> Int16 -> Simulator ()
shiftOp op rd d = do
  simple <- get
  let rdv = (registerFile simple) ! rd
      d'  = (fromIntegral $ d) :: Int
  case op of
    "SLL" -> let res = rdv `shift`   d' in setCodeShift res rdv d' False >> writeReg rd res
    "SLR" -> let res = rdv `rotate`  d' in setCodeLogic res              >> writeReg rd res
    "SRL" -> let res = rdv `shiftRR` d' in setCodeShift res rdv d' True  >> writeReg rd res
    "SRA" -> let res = rdv `shift`(-d') in setCodeShift res rdv d' True  >> writeReg rd res

shiftRR :: Int16 -> Int -> Int16
shiftRR rdv d = (rdv `shiftR` d) .&. (bit (16-d+1))-1

setCodeShift :: Int16 -> Int16 -> Int -> Bool -> Simulator ()
setCodeShift res rdv d isRight = do
  simple <- get
  let res'     = (fromIntegral $ res) :: Int
      shiftOut = d > 0 && (if isRight then res .&. bit (d-1) else res .&. bit (16-d+1)) > 0
  put $ simple { code_s = res < 0
               , code_z = res == 0
               , code_c = shiftOut
               , code_v = False }

writeReg :: Int16 -> Int16 -> Simulator ()
writeReg rd res = do
  simple <- get
  let newRegFile = (registerFile simple) // [(rd, res)]
  put $ simple { registerFile = newRegFile }
