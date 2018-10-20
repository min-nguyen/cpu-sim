module Utils where

import Lib
import Data.Word
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V

type Offset             = Word32
type Address            = Word32

type Instruction        = Assembly RegisterNum RegisterNum Word32

type Memory             = V.Vector Word32
type IMemory            = V.Vector Instruction
type Register           = Word32

data RegisterNum        = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 deriving (Enum, Show)

data Status             = Ready | Stalled  deriving Show

data Registers          = Registers {
                            r0 :: Word32,
                            r1 :: Word32,
                            r2 :: Word32,
                            r3 :: Word32, 
                            r4 :: Word32, 
                            r5 :: Word32,
                            r6 :: Word32,
                            r7 :: Word32
                        }  deriving Show

data Assembly dest source immediate
                        = ADD  dest source source
                        | ADDI source source immediate
                        | BEQ  source source immediate 
                        | LW   source source immediate
                        | LI   dest   immediate
                        | SW   source immediate
                        | JALR dest source
                        | BLTZ dest source immediate
                        deriving Show

data InstructionResult  = InstructionResult { 
                            output :: (Instruction, Word32)
                        } 

data CPU                = CPU {
                            i_memory        :: IMemory,
                            d_memory        :: Memory,
                            registers       :: Registers,
                            pc              :: Word32,
                            npc             :: Word32,
                            executionUnit   :: Unit,
                            fetchUnit       :: Unit,
                            decodeUnit      :: Unit
                        } deriving Show

data Unit               = Unit { 
                            cycles :: Int,
                            status :: Status,
                            instruction :: Maybe Instruction,
                            buffer :: Maybe Instruction
                            -- buffer      :: Maybe Word32
                        }  deriving Show

initRegisters :: Registers 
initRegisters = Registers (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

initUnit :: Unit 
initUnit = Unit 0 Ready Nothing Nothing

initCPU :: [Instruction] -> CPU 
initCPU instructions = let i_mem = V.fromList instructions 
                           d_mem = V.replicate 100 (fromIntegral 0)
                           registers = initRegisters
                           pc = fromIntegral 0
                           npc = fromIntegral 0
                           eunit = initUnit
                           funit = initUnit 
                           dunit = initUnit 
                        in CPU i_mem d_mem registers pc npc eunit funit dunit

writeRegister :: Registers -> RegisterNum -> Word32 -> Registers
writeRegister registers regNum writeVal
    = case regNum of R1 -> registers { r1 = writeVal } 
                     R2 -> registers { r2 = writeVal } 
                     R3 -> registers { r3 = writeVal } 
                     R4 -> registers { r4 = writeVal } 
                     R5 -> registers { r5 = writeVal } 
                     R6 -> registers { r6 = writeVal } 
                     R7 -> registers { r7 = writeVal } 
                     R0 -> registers { r0 = writeVal } 

readRegister :: Registers -> RegisterNum -> Word32
readRegister registers regNum 
    = case regNum of R1 -> r1 registers 
                     R2 -> r2 registers
                     R3 -> r3 registers
                     R4 -> r4 registers
                     R5 -> r5 registers
                     R6 -> r6 registers
                     R7 -> r7 registers
                     R0 -> r0 registers

toRegisterNum :: Word32 -> RegisterNum
toRegisterNum regNum 
    = case regNum of    1 -> R1 
                        2 -> R2 
                        3 -> R3
                        4 -> R4 
                        5 -> R5 
                        6 -> R6
                        7 -> R7
                        0 -> R0 

tick :: Unit -> Unit -> Unit 
tick unit nextUnit = unit {cycles = nextCycle, status = nextStatus} where
    nextCycle  = if cycles unit <= 1 then 0 else cycles unit - 1
    nextStatus = status nextUnit

flushPipeline :: CPU -> CPU  
flushPipeline cpu = cpu { decodeUnit = initUnit, fetchUnit = initUnit, executionUnit = initUnit }
                       
