module Utils where

import Lib
import Data.Word
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Maybe

type Offset             = Word32
type Address            = Word32

type Instruction        = Assembly RegisterNum RegisterNum Word32

type Memory             = V.Vector Word32
type IMemory            = V.Vector Instruction
type Register           = Word32

data RegisterNum        = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 deriving (Enum, Show, Eq)

instance Ord RegisterNum where
    compare r_1 r_2 = compare (show r_1) (show r_2) 



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
        "FetchUnit: " ++ show fetch ++ "\n" ++
        "DecodeUnit: " ++ show decode ++ "\n"

data UnitType           = IntUnit | MemUnit | BranchUnit

data UnitId             = Int_Unit1 | Int_Unit2 | Mem_Unit | Branch_Unit | Fetch_Unit | Decode_Unit

data Unit               = Unit { 
                            unitId      :: UnitId,
                            cycles      :: Int,
                            instruction :: Maybe Instruction,
                            rs_id       :: RSId,
                            rs_cycle    :: Int, 
                            buffer      :: V.Vector Instruction
                        }  

instance Show Unit where 
    show (Unit unit_id cycle instrct rsid rscycle buff) = "[Cycles: " ++ show cycle ++ ", Instruction: " ++ show instrct ++ ", RSId: " ++ show rsid ++ "RSCycle: " ++ show rscycle ++ ", Buffer: " ++ show buff ++ "]"

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

type RSId               = Int

type RSs                = Map.Map RSId (Int, Maybe RSEntry) 

data RSEntry            = RSEntry {
                            rs_instruction  :: Instruction,
                            qd              :: Int,
                            qj              :: Int,
                            qk              :: Int,
                            vj              :: Word32,
                            vk              :: Word32,
                            addr            :: Word32,
                            busy            :: Bool
                        } deriving (Show)



type RegisterStatuses   = Map.Map RegisterNum Int 

data ReservationStation = ReservationStation {
                            rs_entries      :: RSs,
                            reg_statuses    :: RegisterStatuses
                        } 

instance Show ReservationStation where 
    show (ReservationStation entries statuses) = "RS_Entries: "  ++ show entries ++ ", RS_Statuses: " ++ show statuses


-- moveUpRSEntries :: ReservationStation -> ReservationStation 
-- moveUpRSEntries rsstation 
--                     = let newRSEntries   = foldr (\rsId rss -> let rsentry = fromMaybe Nothing (Map.lookup rsId rss) in  Map.insert (rsId - 1) rsentry rss) rss [5,4,3,2] 
--                           newRegStatuses = Map.map (\regStat -> if regStat == 0 then regStat else regStat - 1) regs     
--                       in  rsstation {rs_entries = newRSEntries, reg_statuses = newRegStatuses}
--                       where rss = rs_entries rsstation 
--                             regs = reg_statuses rsstation

initRegisterStatuses :: RegisterStatuses
initRegisterStatuses = Map.fromList [(R0, 0), (R1, 0), (R2, 0), (R3, 0), (R4, 0), (R5, 0), (R6, 0), (R7, 0)]

initReservationStation :: ReservationStation
initReservationStation = ReservationStation (Map.fromList $ map (\i -> (i, (0, Nothing))) [1..4]) initRegisterStatuses

initRegisters :: Registers 
initRegisters = Registers (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

initUnit :: UnitId -> Unit 
initUnit unit_id = Unit unit_id 0 Nothing 0 0 V.empty 

initCPU :: [Instruction] -> CPU 
initCPU instructions = let i_mem = V.fromList instructions 
                           d_mem = V.replicate 30 (fromIntegral 0)
                           rs_station = initReservationStation
                           registers = initRegisters
                           pc = fromIntegral 0
                           npc = fromIntegral 0
                           eunits =  Units (initUnit Int_Unit1) (initUnit Int_Unit2) (initUnit Mem_Unit) (initUnit Branch_Unit)
                           funit = initUnit Fetch_Unit
                           dunit = initUnit Decode_Unit
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

getRegStat :: RegisterNum -> RegisterStatuses -> Maybe Int
getRegStat regNum regstats 
    =  Map.lookup regNum regstats

setRegStat :: RegisterNum -> Int -> RegisterStatuses -> RegisterStatuses
setRegStat regNum  value regstats
    =  Map.insert regNum value regstats
 

tick :: Unit -> Unit 
tick unit = unit {cycles = nextCycle} where
    nextCycle  = if cycles unit <= 1 then 0 else cycles unit - 1

flushPipeline :: CPU -> CPU  
flushPipeline cpu = cpu { decodeUnit = initUnit Decode_Unit, fetchUnit = initUnit Fetch_Unit, executionUnits = Units (initUnit Int_Unit1) (initUnit Int_Unit2) (initUnit Mem_Unit) (initUnit Branch_Unit)}
                       
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


allocateRegStats :: RegisterStatuses -> Instruction -> RegisterStatuses
allocateRegStats regstats instrct = 
    case instrct of 
            ADD  d s1 s2 -> (setRegStat s1 0 . setRegStat s2 0 . setRegStat d 0) regstats
            ADDI s1 s2 i -> (setRegStat s1 0 . setRegStat s2 0) regstats
            BEQ  s1 s2 i -> (setRegStat s1 0 . setRegStat s2 0) regstats
            LW   s1 s2 i -> (setRegStat s1 0 . setRegStat s2 0) regstats
            LI   d i     -> (setRegStat d 0) regstats
            SW   d i     -> (setRegStat d 0) regstats
            JALR d s     -> (setRegStat s 0 . setRegStat d 0) regstats

deallocateRegStats :: RegisterStatuses -> Instruction -> RSId ->  RegisterStatuses
deallocateRegStats regstats instrct rsid = 
    case instrct of 
            ADD  d s1 s2 -> (setRegStat s1 rsid . setRegStat s2 rsid . setRegStat d rsid) regstats
            ADDI s1 s2 i -> (setRegStat s1 rsid . setRegStat s2 rsid) regstats
            BEQ  s1 s2 i -> (setRegStat s1 rsid . setRegStat s2 rsid) regstats
            LW   s1 s2 i -> (setRegStat s1 rsid . setRegStat s2 rsid) regstats
            LI   d i     -> (setRegStat d rsid) regstats
            SW   d i     -> (setRegStat d rsid) regstats
            JALR d s     -> (setRegStat s rsid . setRegStat d rsid) regstats

allocateRSEntry :: RSs -> RSId -> RSs 
allocateRSEntry rs rsid = Map.adjust (\(cycle, _) -> (cycle, Nothing)) rsid rs

