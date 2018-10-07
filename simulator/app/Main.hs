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

type Register           = Word32

data RegisterNum        = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving Enum

data Registers          = Registers {
                            r1 :: Word32,
                            r2 :: Word32,
                            r3 :: Word32, 
                            r4 :: Word32, 
                            r5 :: Word32,
                            r6 :: Word32,
                            r7 :: Word32,
                            r8 :: Word32
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
                            npc             :: Word32,
                            executionUnit   :: ExecutionUnit
                        }

data ExecutionUnit      = ExecutionUnit { 
                            instruction :: Instruction
                        }

updateCPU :: CPU -> CPU 
updateCPU cpu = 
    let encodedInstruct = fetchInstruction cpu         
        decodedInstruct = decodeInstruction encodedInstruct
        -- cpu'            = execInstruction
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

execInstruction :: CPU -> Instruction -> CPU
execInstruction cpu (ADD dest source_a source_b)     
    = let regs = registers cpu
          val  = sum $ map (readRegister regs) [source_a, source_b] 
      in  cpu { registers = writeRegister regs dest val } 
execInstruction cpu (ADDI dest source i)  
    = let regs = registers cpu
          [dest_reg, source_reg] = map (readRegister regs) [dest, source] 
          val = i + source_reg
      in  cpu { registers = writeRegister regs dest val } 
execInstruction cpu (BEQ source_a source_b i)     
    = let regs = registers cpu
          [a, b] = map (readRegister (registers cpu)) [source_a, source_b]
      in case () of 
                _ | a == b        -> cpu { pc = (npc cpu), npc = (npc cpu) + shiftL i 2}
                _                 -> cpu
execInstruction cpu (LW dest source i)   
    = let loadedWord = (memory cpu) V.! (fromIntegral $ (readRegister (registers cpu) source))
      in  cpu {  registers = writeRegister (registers cpu) dest loadedWord} 
-- execInstruction cpu (J i) 
--     = InstructionResult (J i, cpu {pc = i})


main :: IO ()
main = someFunc


writeRegister :: Registers -> RegisterNum -> Word32 -> Registers
writeRegister registers regNum writeVal
    = case regNum of R1 -> registers { r1 = writeVal } 
                     R2 -> registers { r2 = writeVal } 
                     R3 -> registers { r3 = writeVal } 
                     R4 -> registers { r4 = writeVal } 
                     R5 -> registers { r5 = writeVal } 
                     R6 -> registers { r6 = writeVal } 
                     R7 -> registers { r7 = writeVal } 
                     R8 -> registers { r8 = writeVal } 

readRegister :: Registers -> RegisterNum -> Word32
readRegister registers regNum 
    = case regNum of R1 -> r1 registers 
                     R2 -> r2 registers
                     R3 -> r3 registers
                     R4 -> r4 registers
                     R5 -> r5 registers
                     R6 -> r6 registers
                     R7 -> r7 registers
                     R8 -> r8 registers

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