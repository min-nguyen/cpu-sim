module Main where

import Lib
import Data.Word
import qualified Data.Vector as V

type Offset             = Word32
type Address            = Word32

type Instruction        = Assembly Register Register Word32

data Memory             = Memory (V.Vector Word32)

data Register           = Register Word32

data Registers          = Registers {
                            r1 :: Register,
                            r2 :: Register,
                            r3 :: Register, 
                            r4 :: Register, 
                            r5 :: Register,
                            r6 :: Register,
                            r7 :: Register,
                            r8 :: Register
                        }

data Assembly dest source immediate
                        = ADD  dest source source 
                        | ADDI dest source immediate
                        | CMP  dest source 
                        | LW   dest source immediate
                        | LI   dest immediate 
                        | B    immediate 
                        | J    immediate
                        | BLTH dest source immediate

data InstructionResult  = InstructionResult { 
                            output :: Word32
                        }

data CPU    = CPU {
                memory          :: Memory,
                registers       :: Registers,
                pc              :: Int,
                executionUnit   :: ExecutionUnit
            }

data ExecutionUnit = ExecutionUnit { 
                    instruction :: Instruction
                }




-- executeInstruction :: Instruction -> InstructionResult

main :: IO ()
main = someFunc
