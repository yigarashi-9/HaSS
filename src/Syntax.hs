module Syntax where

import Data.Array
import Data.Int

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
                     } deriving(Show)
