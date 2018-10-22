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

-- data Status             = Ready | Stalled  deriving Show

data Registers          = Registers {
                            r0 :: Word32,
                            r1 :: Word32,
                            r2 :: Word32,
                            r3 :: Word32, 
                            r4 :: Word32, 
                            r5 :: Word32,
                            r6 :: Word32,
                            r7 :: Word32
                        }  

instance Show Registers where 
    show (Registers r0 r1 r2 r3 r4 r5 r6 r7) = 
        "[R0: " ++ show r0 ++ " R1: " ++ show r1 ++ " R2 : " ++ show r2 ++ " R3 : " ++ show r3 ++ " R4 : " ++ show r4 ++ " R5 : " ++ show r5 ++ " R6 : " ++ show r6 ++ " R7 : " ++ show r7 ++ "]"

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
                            rs_station      :: ReservationStation,
                            registers       :: Registers,
                            pc              :: Word32,
                            npc             :: Word32,
                            executionUnits  :: Units,
                            fetchUnit       :: Unit,
                            decodeUnit      :: Unit
                        } 

instance Show CPU where
    show (CPU imem dmem rs_station reg pc npc exec fetch decode) = 
        "InstructionMemory: " ++ show imem ++ "\n" ++
        "DataMemory: " ++ show dmem ++ "\n" ++
        "ReservationStation: " ++ show rs_station ++ "\n" ++
        "Registers: " ++ show reg ++ "\n" ++
        "PC: " ++ show pc ++ ", NPC: " ++ show npc ++ "\n" ++
        "ExecutionUnits: " ++ show exec ++ 
        "FetchUnit: " ++ show fetch ++
        "DecodeUnit: " ++ show decode

data UnitType           = IntUnit | MemUnit | BranchUnit

data Unit               = Unit { 
                            cycles      :: Int,
                            instruction :: Maybe Instruction,
                            buffer      :: V.Vector Instruction
                        }  

instance Show Unit where 
    show (Unit cycle instrct buff) = "[Cycles: " ++ show cycle ++ ", Instruction: " ++ show instrct ++ ", Buffer: " ++ show buff ++ "]"

data Units              = Units {
                            intUnit1    :: Unit,
                            intUnit2    :: Unit,
                            memUnit     :: Unit,
                            branchUnit  :: Unit 
                          } 

instance Show Units where 
    show (Units intunit1 intunit2 memunit branchunit) = "IntUnit1 : " ++ show intunit1 ++ " \n" ++ 
                                                        "IntUnit2: " ++ show intunit2 ++ " \n" ++ 
                                                        "MemUnit: " ++ show memunit ++ " \n" ++ 
                                                        "BranchUnit: " ++ show branchunit ++ " \n"

data RSEntry            = RSEntry {
                            rs_instruction  :: Instruction,
                            qj              :: Int,
                            qk              :: Int,
                            vj              :: Word32,
                            vk              :: Word32,
                            addr            :: Address,
                            busy            :: Bool
                        } deriving Show

data RegisterStatuses   = RegisterStatuses {
                            rs_r0 :: Int,
                            rs_r1 :: Int,
                            rs_r2 :: Int,
                            rs_r3 :: Int,
                            rs_r4 :: Int,
                            rs_r5 :: Int,
                            rs_r6 :: Int,
                            rs_r7 :: Int
                        } 

instance Show RegisterStatuses where 
    show (RegisterStatuses r0 r1 r2 r3 r4 r5 r6 r7) =
        "[R0: " ++ show r0 ++ ", R1: " ++ show r1 ++ ", R2: " ++ show r2 ++ ", R3: " ++ show r3 ++ ", R4: " ++ show r4 ++
        ", R5: " ++ show r5 ++ ", R6: " ++ show r6 ++ ", R7: " ++ show r7 ++ "]\n"

data ReservationStation = ReservationStation {
                            rs_entries      :: [RSEntry],
                            reg_statuses    :: RegisterStatuses
                        } deriving Show

initRegisterStatuses :: RegisterStatuses
initRegisterStatuses = RegisterStatuses 0 0 0 0 0 0 0 0

initReservationStation :: ReservationStation
initReservationStation = ReservationStation [] initRegisterStatuses

initRegisters :: Registers 
initRegisters = Registers (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

initUnit :: Unit 
initUnit = Unit 0 Nothing V.empty

initCPU :: [Instruction] -> CPU 
initCPU instructions = let i_mem = V.fromList instructions 
                           d_mem = V.replicate 30 (fromIntegral 0)
                           rs_station = initReservationStation
                           registers = initRegisters
                           pc = fromIntegral 0
                           npc = fromIntegral 1
                           eunits =  Units initUnit initUnit initUnit initUnit
                           funit = initUnit 
                           dunit = initUnit 
                        in CPU  i_mem d_mem rs_station registers pc npc eunits funit dunit

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

tick :: Unit -> Unit 
tick unit = unit {cycles = nextCycle} where
    nextCycle  = if cycles unit <= 1 then 0 else cycles unit - 1

flushPipeline :: CPU -> CPU  
flushPipeline cpu = cpu { decodeUnit = initUnit, fetchUnit = initUnit, executionUnits = Units initUnit initUnit initUnit initUnit }
                       
instructionToExecutionUnit :: Instruction -> UnitType
instructionToExecutionUnit instruction = 
        case instruction of ADD _ _ _ -> IntUnit
                            ADDI _ _ _-> IntUnit 
                            BEQ _ _ _-> BranchUnit
                            BLTZ _ _ _-> BranchUnit
                            JALR _ _ -> BranchUnit
                            LW _ _ _-> MemUnit 
                            LI _ _ -> MemUnit 
                            SW _ _ -> MemUnit 