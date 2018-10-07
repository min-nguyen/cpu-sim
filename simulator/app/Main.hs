module Main where

import Lib
import Data.Word
import Data.Bits
import qualified Data.Vector as V

type Offset             = Word32
type Address            = Word32

type Instruction        = Assembly Register Register Word32

type Memory             =  (V.Vector Word32)

data Register           = Register { 
                            regVal  :: Word32
                        }

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
                        | BEQ  dest source source 
                        | LW   dest source immediate
                        | J    immediate
                        | BLTH dest source immediate

data InstructionResult  = InstructionResult { 
                            output :: (Instruction, Word32)
                        }

data CPU                = CPU {
                            memory          :: Memory,
                            registers       :: Registers,
                            pc              :: Int,
                            executionUnit   :: ExecutionUnit
                        }

data ExecutionUnit      = ExecutionUnit { 
                            instruction :: Instruction
                        }

updateCPU :: CPU -> CPU 
updateCPU cpu = cpu         


fetchInstruction :: CPU -> (CPU, Word32)
fetchInstruction cpu = (cpu, (memory cpu V.! pc cpu))

decodeInstruction :: Word32 -> Word32
decodeInstruction encodedInstruct 
    = let opcode = shiftR encodedInstruct 24 
      in  case opcode of    0  -> 0 -- ADD
                            8  -> 8 -- ADDI
                            4  -> 4 -- BEQ
                            35 -> 35 -- LW


execInstruction :: CPU -> Instruction -> InstructionResult
execInstruction cpu (ADD dest source_a source_b)     
    = InstructionResult (ADD dest source_a source_b, sum . map regVal $ [source_a, source_b])
execInstruction cpu (ADDI dest source i)  
    = InstructionResult (ADDI dest source i, i + regVal source)
execInstruction cpu (BEQ dest source_a source_b)     
    = let a = regVal source_a  
          b = regVal source_b 
      in case () of 
                _ | a == b        -> InstructionResult (BEQ dest source_a source_b, 1)
                _                 -> InstructionResult (BEQ dest source_a source_b, 0)
execInstruction cpu (LW dest source i)   
    = InstructionResult (LW dest source i, (memory cpu) V.! (fromIntegral $ regVal source))



main :: IO ()
main = someFunc
