module Syntax where

import Data.Array
import Data.Int
import Data.List

data Instruction = Prim     String Int16 Int16
                 | Shift    String Int16 Int16
                 | Input    Int16
                 | Output   Int16
                 | Halt
                 | Nop
                 | Load     Int16 Int16 Int16
                 | Store    Int16 Int16 Int16
                 | LoadIm   Int16 Int16
                 | AddI     Int16 Int16
                 | UncondBr Int16
                 | UncondBrLabel String
                 | CondBr   String Int16
                 | CondBrLabel   String String
                 deriving(Show)

data SIMPLE = SIMPLE { pc :: Int16
                     , instruction :: [Instruction]
                     , registerFile :: Array Int16 Int16
                     , ram    :: Array Int16 Int16
                     , code_c :: Bool
                     , code_z :: Bool
                     , code_v :: Bool
                     , code_s :: Bool
                     }


instance Show SIMPLE where
    show simple = concat ["Registier File:\n",
                          showMemory 1 1 (registerFile simple),
                          "\n\nRam:\n",
                          showMemory 8 4 (ram simple)]


showMemory :: Int -> Int -> Array Int16 Int16 -> String
showMemory c w ary = concat . (intersperse "\n") . map column $
                       groupOf c (assocs ary)
    where
      groupOf :: Int -> [a] -> [[a]]
      groupOf _ []  = []
      groupOf num l = (take num l) : groupOf num (drop num l)

      column :: [(Int16, Int16)] -> String
      column l = "| " ++ (foldr (\(i, x) acc ->
                                 concat [alignr w i, ":", alignr 7 x, " | ", acc]) [] l)

      alignr :: Int -> Int16 -> String
      alignr w n = replicate (w - length n') ' ' ++ n'
          where n' = show n
