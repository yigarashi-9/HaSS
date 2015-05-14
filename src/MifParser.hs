{-# LANGUAGE RankNTypes #-}
module MifParser(parseMif) where

import           Text.Parsec.Language
import qualified Text.Parsec.Token as P
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec
import           Data.Int
import           Data.Char
import           Control.Monad
import           Numeric

parseMif :: String -> IO [(Int16, Int16)]
parseMif fileName = parseFromFile mif fileName >>= either report return
  where report err = error $ show err

def :: LanguageDef st
def = emptyDef { P.identStart = alphaNum
               , P.commentLine  = "--"
               , P.commentStart = "%"
               , P.commentEnd = "%"
               , P.reservedNames = ["DEPTH", "WIDTH", "ADDRESS_RADIX", "DATA_RADIX"]}

lexer :: P.TokenParser st
lexer = P.makeTokenParser def

symbol :: String -> Parser String
symbol = P.symbol lexer

identifier :: Parser String
identifier = P.identifier lexer

colon :: Parser String
colon = P.colon lexer

semi :: Parser String
semi = P.semi lexer

natural :: Parser Integer
natural = P.natural lexer

integer :: Parser Integer
integer = P.integer lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

mif :: Parser [(Int16, Int16)]
mif = do
  width   <- whiteSpace >> symbol "WIDTH"         >> symbol "=" >> natural <* semi
  depth   <- whiteSpace >> symbol "DEPTH"         >> symbol "=" >> natural <* semi
  adr_rad <- whiteSpace >> symbol "ADDRESS_RADIX" >> symbol "=" >> radix   <* semi
  dat_rad <- whiteSpace >> symbol "DATA_RADIX"    >> symbol "=" >> radix   <* semi
  whiteSpace >> symbol "CONTENT" >> whiteSpace >> symbol "BEGIN"
  memData depth width adr_rad dat_rad <* (whiteSpace >> symbol "END" >> semi >> eof)

radix :: Parser String
radix = choice . map symbol $ ["BIN", "HEX", "OCT", "DEC", "UNS"]

memData :: Integer -> Integer -> String -> String -> Parser [(Int16, Int16)]
memData depth width adr_rad dat_rad = do
  memDat <- liftM concat $ many1 (try monoData <|> try multiData)
  return $ zeroFill (((fromIntegral depth)-1) :: Int16)  memDat
    where
      monoData :: Parser [(Int16, Int16)]
      monoData = do
        addr <- whiteSpace >> readByRadix adr_rad
        dat  <- colon >> readByRadix dat_rad <* semi
        if 0 <= addr && addr < depth
        then return [((fromIntegral addr) :: Int16,
                      (fromIntegral dat)  :: Int16)]
        else unexpected "address or data is invalid"

      multiData :: Parser [(Int16, Int16)]
      multiData = do
        left  <- whiteSpace >> symbol "["  >> readByRadix adr_rad
        right <- symbol ".." >> readByRadix adr_rad
        symbol "]" >> colon
        dat   <- readByRadix dat_rad <* semi
        if left < right && 0 <= left && right < depth
        then return $ zipWith (,) ([(fromIntegral left)..(fromIntegral right)])
                                  (repeat $ fromIntegral dat)
        else unexpected "address or data is invalid"

      zeroFill :: Int16 -> [(Int16, Int16)] -> [(Int16, Int16)]
      zeroFill (-1) _  = []
      zeroFill _ []    = []
      zeroFill cnt l   = (maybe (cnt, 0) (\d -> (cnt, d)) $ lookup cnt l):
                         (zeroFill (cnt-1) l)

readByRadix :: String -> Parser Integer
readByRadix "BIN" = binary
readByRadix "HEX" = liftM (fst . head . readHex) identifier
readByRadix "OCT" = liftM (fst . head . readOct) identifier
readByRadix "DEC" = integer
readByRadix "UNS" = natural

binary :: Parser Integer
binary = do
  bin <- many1 $ oneOf "01"
  return $ fst . head  $ readInt 2 (`elem` "01") digitToInt bin
