{-# LANGUAGE RankNTypes #-}
module Parser (parseFile) where

import           Text.Parsec.Language
import qualified Text.Parsec.Token as P
import           Text.Parsec.String
import           Text.Parsec
import           Control.Exception hiding(try)
import           System.Exit
import           System.IO
import           Data.Int
import           Control.Monad
import           Syntax

parseFile :: String -> IO ([Instruction], Int)
parseFile fileName = parseFromFile assembly fileName >>= either report ret
  where
    report err = error $ show err
    ret insts  = do
      len <- evaluate $ length insts
      return (insts, len)

def :: LanguageDef st
def = emptyDef { P.commentLine = "%" }

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

symbol :: String -> Parser String
symbol = P.symbol lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

natural :: Parser Integer
natural = P.natural lexer

integer :: Parser Integer
integer = P.integer lexer

register :: Parser Int16
register = do
  r <- immdval
  if r < 0 || r > 7
  then unexpected "register number is 0 to 7"
  else return r

immdval :: Parser Int16
immdval = do
  n <- liftM fromIntegral integer
  return (n :: Int16)

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

assembly :: Parser [Instruction]
assembly = do
  insts <- many1 instr
  eof >> (return insts)

instr :: Parser Instruction
instr = whiteSpace >> instBody

instBody :: Parser Instruction
instBody =  try primOp
        <|> try shiftOp
        <|> try inOp
        <|> try outOp
        <|> try nop
        <|> try hlt
        <|> try loadOp
        <|> try storeOp
        <|> try loadImOp
        <|> try addiOp
        <|> try uncondBrOp
        <|> condBrOp
        <?> "instruction"

primOp :: Parser Instruction
primOp = do
  op <- choice (map (try . symbol) ["ADD", "SUB", "AND", "OR", "XOR", "CMP", "MOV"])
  rd <- register
  rs <- (symbol ",") >> register
  return $ Prim op rd rs

shiftOp :: Parser Instruction
shiftOp = do
  op <- choice (map (try . symbol) ["SLL", "SLR", "SRL", "SRA"])
  rd <- register
  d  <- (symbol ",") >> immdval
  if d < 0 || d >= 16
  then unexpected "shift amount is 0 to 15"
  else return $ Shift op rd d

inOp :: Parser Instruction
inOp = symbol "IN" >> (liftM Input register)

outOp :: Parser Instruction
outOp = symbol "OUT" >> (liftM Output register)

nop :: Parser Instruction
nop = symbol "NOP" >> return Nop

hlt :: Parser Instruction
hlt = symbol "HLT" >> return Halt

loadOp :: Parser Instruction
loadOp = do
  ra <- (symbol "LD") >> register
  d  <- (symbol ",")  >> immdval
  rb <- parens register
  return $ Load ra d rb

storeOp :: Parser Instruction
storeOp = do
  ra <- (symbol "ST") >> register
  d  <- (symbol ",")  >> immdval
  rb <- parens register
  return $ Store ra d rb

loadImOp :: Parser Instruction
loadImOp = do
  rb <- (symbol "LI") >> register
  d  <- (symbol ",")  >> immdval
  return $ LoadIm rb d

addiOp :: Parser Instruction
addiOp = do
  rb <- (symbol "ADDI") >> register
  d  <- (symbol ",")  >> immdval
  return $ AddI rb d

uncondBrOp :: Parser Instruction
uncondBrOp = do
  d <- (symbol "B") >> immdval
  return $ UncondBr d

condBrOp :: Parser Instruction
condBrOp = do
  op <- choice (map (try . symbol) ["BE", "BLT", "BLE", "BNE"])
  d  <- immdval
  return $ CondBr op d
