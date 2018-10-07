module Main where

import Lib
import Data.Word
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V

type Offset             = Word32
type Address            = Word32

type Instruction        = Assembly RegisterNum RegisterNum Word32

type Memory             =  (V.Vector Word32)

data Register           = Register { 
                            regVal  :: Word32
                        }

data RegisterNum        = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving Enum

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
                        | ADDI source source immediate
                        | BEQ  source source immediate 
                        | LW   source source immediate
                        | J    immediate
                        | BLTZ dest source immediate

data InstructionResult  = InstructionResult { 
                            output :: (Instruction, Word32)
                        }

data CPU                = CPU {
                            memory          :: Memory,
                            registers       :: Registers,
                            pc              :: Word32,
                            executionUnit   :: ExecutionUnit
                        }

data ExecutionUnit      = ExecutionUnit { 
                            instruction :: Instruction
                        }

updateCPU :: CPU -> CPU 
updateCPU cpu = 
    let encodedInstruct = fetchInstruction cpu         
        decodedInstruct = decodeInstruction encodedInstruct
        -- need to pipeline
    in  cpu

fetchInstruction :: CPU -> Word32
fetchInstruction cpu = memory cpu V.! (fromIntegral $ pc cpu)

decodeInstruction :: Word32 -> CPU -> Instruction
decodeInstruction encodedInstruct cpu
    = let opcode = shiftR encodedInstruct 24 
      in  case () of _ | opcode == 0  -> decodeInstructionR encodedInstruct cpu 
                     _ | opcode <= 3  -> decodeInstructionJ encodedInstruct opcode cpu 
                     _ | opcode >= 4  -> decodeInstructionI encodedInstruct opcode cpu 

decodeFunct :: Word32 -> RegisterNum -> RegisterNum -> RegisterNum -> Assembly RegisterNum RegisterNum Word32
decodeFunct funct = 
    case funct of 32 -> ADD 

decodeInstructionR :: Word32 -> CPU -> Instruction
decodeInstructionR encodedInstruct cpu =
    let funct       = decodeFunct $ shiftR (shiftL encodedInstruct 26) 26 
        shiftAmount = shiftR (shiftL encodedInstruct 21) 27
        rd          = toRegisterNum $ shiftR (shiftL encodedInstruct 16) 27
        rt          = toRegisterNum $ shiftR (shiftL encodedInstruct 11) 27
        rs          = toRegisterNum $ shiftR (shiftL encodedInstruct 6) 27
    in  funct rd rt rs                      

decodeInstructionI :: Word32 -> Word32 -> CPU -> Instruction
decodeInstructionI encodedInstruct opcode cpu =
    let operation   = case opcode of 4 -> BEQ 
                                     6 -> BLTZ 
                                     8 -> ADDI
                                     35 -> LW 
        rt          = toRegisterNum $ shiftR (shiftL encodedInstruct 11) 27
        rs          = toRegisterNum $ shiftR (shiftL encodedInstruct 6) 27
        immediate   = shiftR (shiftL encodedInstruct 16) 16
    in  operation rt rs immediate

decodeInstructionJ :: Word32 -> Word32 -> CPU -> Instruction
decodeInstructionJ encodedInstruct opcode cpu =
    let operation = case opcode of 2 -> J
        address = shiftR (shiftL encodedInstruct 6) 6
    in  J address

execInstruction :: CPU -> Instruction -> InstructionResult
execInstruction cpu (ADD dest source_a source_b)     
    = let reg = map (readRegister $ registers cpu) [source_a, source_b] 
      in  InstructionResult (ADD dest source_a source_b, sum $ reg)
execInstruction cpu (ADDI dest source i)  
    = let [dest_reg, source_reg] = map (readRegister $ registers cpu) [dest, source] 
      in  InstructionResult (ADDI dest source i, i + source_reg)
execInstruction cpu (BEQ source_a source_b i)     
    = let [a, b] = map (readRegister (registers cpu)) [source_a, source_b]
      in case () of 
                _ | a == b        -> InstructionResult (BEQ source_a source_b i, 1)
                _                 -> InstructionResult (BEQ source_a source_b i, 0)
execInstruction cpu (LW dest source i)   
    = InstructionResult (LW dest source i, (memory cpu) V.! (fromIntegral $ (readRegister (registers cpu) source)))
-- execInstruction cpu (J i) 
--     = InstructionResult (J i, cpu {pc = i})


main :: IO ()
main = someFunc

readRegister :: Registers -> RegisterNum -> Word32
readRegister registers regNum 
    = case regNum of R1 -> regVal $ r1 registers 
                     R2 -> regVal $ r2 registers
                     R3 -> regVal $ r3 registers
                     R4 -> regVal $ r4 registers
                     R5 -> regVal $ r5 registers
                     R6 -> regVal $ r6 registers
                     R7 -> regVal $ r7 registers
                     R8 -> regVal $ r8 registers

toRegisterNum :: Word32 -> RegisterNum
toRegisterNum regNum 
    = case regNum of    1 -> R1 
                        2 -> R2 
                        3 -> R3
                        4 -> R4 
                        5 -> R5 
                        6 -> R6
                        7 -> R7
                        8 -> R8 