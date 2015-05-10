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
import           Data.Maybe
import           Control.Monad
import           Control.Applicative((<*))
import           Syntax

parseFile :: String -> IO [Instruction]
parseFile fileName = parseFromFile assembly fileName >>= either report (return . convLabel)
  where
    report err = error $ show err

convLabel :: [(String, Instruction)] -> [Instruction]
convLabel l = convLoop labels 1 (map snd l)
    where labels = zipWith (,) (map fst l) ([0..] :: [Int16])

convLoop :: [(String, Int16)] -> Int16 -> [Instruction] -> [Instruction]
convLoop _ _ [] = []
convLoop labels pc (l:ls)
    = (case l of
         (UncondBrLabel label)  -> (UncondBr $  (fromJust (lookup label labels)) - pc)
         (CondBrLabel op label) -> (CondBr op $ (fromJust (lookup label labels)) - pc)
         inst                   -> inst
      ) : convLoop labels (pc+1) ls

def :: LanguageDef st
def = emptyDef { P.commentLine = "%"
               , P.reservedNames = ["ADD", "SUB", "AND", "OR"  , "XOR", "CMP", "MOV", "SLL",
                                    "SLR", "SRL", "SRA", "IN"  , "OUT", "NOP", "HLT", "ADDI",
                                    "LD" , "ST" , "LI" , "B"   , "BE" , "BLT", "BLE", "BNE"]
               }

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

symbol :: String -> Parser String
symbol = P.symbol lexer

identifier :: Parser String
identifier = P.identifier lexer

colon :: Parser String
colon = P.colon lexer

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

assembly :: Parser [(String, Instruction)]
assembly = do
  insts <- many1 instr
  eof >> (return insts)

instr :: Parser (String, Instruction)
instr = liftM2 (,) (whiteSpace >> option "" (identifier <* colon)) instBody

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
        <|> try condBrOp
        <|> uncondBrOp
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
uncondBrOp = (symbol "B") >> choice [immdval    >>= (return . UncondBr),
                                     identifier >>= (return . UncondBrLabel)]

condBrOp :: Parser Instruction
condBrOp = do
  op <- choice (map (try . symbol) ["BE", "BLT", "BLE", "BNE"])
  choice [immdval    >>= (return . CondBr op),
          identifier >>= (return . CondBrLabel op)]
