module Main where

import Lib
import Data.Word

type Offset         = Word32
type Address        = Word32

data Register       = Register Word32

data RegisterNum    = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8

data Instruction    = ADD  Register Register 
                    | ADDI Register Word32
                    | CMP  Register Register
                    | LW   Register Offset
                    | LI   Register Word32 
                    | B    Address 
                    | J    Address
                    | BLTH Register Register Address


main :: IO ()
main = someFunc
