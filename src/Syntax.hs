
module Syntax where

data Instruntion = Prim     String Integer Integer
                 | Shift    String Integer Integer
                 | Input    Integer
                 | Output   Integer
                 | Halt
                 | Load     Integer Integer Integer
                 | Store    Integer Integer Integer
                 | LoadIm   Integer Integer
                 | UncondBr Integer
                 | CondBr   String Integer
                 deriving(Show)
