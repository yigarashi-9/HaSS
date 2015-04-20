{-# LANGUAGE RankNTypes #-}
module Parser (parseFile) where

import           Text.Parsec.Language
import qualified Text.Parsec.Token as P
import           Text.Parsec.String
import           Text.Parsec
import           System.Exit
import           System.IO
import           Control.Monad
import           Syntax

parseFile :: String -> IO [Instruntion]
parseFile fileName = parseFromFile assembly fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

def :: LanguageDef st
def = emptyDef

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

symbol :: String -> Parser String
symbol = P.symbol lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

natural :: Parser Integer
natural = P.natural lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

assembly :: Parser [Instruntion]
assembly = do
  insts <- many1 instruction
  eof >> (return insts)

instruction :: Parser Instruntion
instruction = whiteSpace >> instBody

instBody :: Parser Instruntion
instBody =  try primOp
        <|> try shiftOp
        <|> try inOp
        <|> try outOp
        <|> try hlt
        <|> try loadOp
        <|> try storeOp
        <|> try loadImOp
        <|> try uncondBrOp
        <|> condBrOp
        <?> "instruction"

primOp :: Parser Instruntion
primOp = do
  op <- choice (map (try . symbol) ["ADD", "SUB", "AND", "OR", "XOR", "CMP", "MOV"])
  rd <- natural
  rs <- (symbol ",") >> natural
  return $ Prim op rd rs

shiftOp :: Parser Instruntion
shiftOp = do
  op <- choice (map (try . symbol) ["SLL", "SLR", "SRL", "SRA"])
  rd <- natural
  rs <- (symbol ",") >> natural
  return $ Shift op rd rs

inOp :: Parser Instruntion
inOp = symbol "IN" >> (liftM Input natural)

outOp :: Parser Instruntion
outOp = symbol "OUT" >> (liftM Output natural)

hlt :: Parser Instruntion
hlt = symbol "HLT" >> return Halt

loadOp :: Parser Instruntion
loadOp = do
  ra <- (symbol "LD") >> natural
  d  <- (symbol ",")  >> natural
  rb <- parens natural
  return $ Load ra d rb

storeOp :: Parser Instruntion
storeOp = do
  ra <- (symbol "ST") >> natural
  d  <- (symbol ",")  >> natural
  rb <- parens natural
  return $ Store ra d rb

loadImOp :: Parser Instruntion
loadImOp = do
  rb <- (symbol "LI") >> natural
  d  <- (symbol ",")  >> natural
  return $ LoadIm rb d

uncondBrOp :: Parser Instruntion
uncondBrOp = do
  rb <- (symbol "B") >> natural
  d  <- (symbol ",") >> natural
  return $ UncondBr d

condBrOp :: Parser Instruntion
condBrOp = do
  op <- choice (map (try . symbol) ["BE", "BLT", "BLE", "BNE"])
  d  <- natural
  return $ CondBr op d
