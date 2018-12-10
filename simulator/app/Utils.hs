module Utils where

import Lib
import Data.Word
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Maybe

type Offset             = Int
type Address            = Int

type Instruction        = Assembly RegisterNum RegisterNum Int

type InstructionAndPc   = (Instruction, Int)

type Memory             = V.Vector Int
type IMemory            = V.Vector Instruction
type Register           = Int

data RegisterNum        = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving (Enum, Show, Eq)

instance Ord RegisterNum where
    compare r_1 r_2 = compare (show r_1) (show r_2) 



-- data Status             = Ready | Stalled  deriving Show

data Registers          = Registers {
                            r0 :: Int,
                            r1 :: Int,
                            r2 :: Int,
                            r3 :: Int, 
                            r4 :: Int, 
                            r5 :: Int,
                            r6 :: Int,
                            r7 :: Int,
                            r8 :: Int,
                            r9 :: Int,
                            r10 :: Int,
                            r11 :: Int, 
                            r12 :: Int, 
                            r13 :: Int,
                            r14 :: Int,
                            r15 :: Int
                        }  

instance Show Registers where 
    show (Registers r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) = 
        "[R0: " ++ show r0 ++ " R1: " ++ show r1 ++ " R2 : " ++ show r2 ++ " R3 : " ++ show r3 ++ " R4 : " ++ show r4 ++ " R5 : " ++ show r5 ++ " R6 : " ++ show r6 ++ " R7 : " ++ show r7 ++ "]\n" ++
        "[R8: " ++ show r8 ++ " R9: " ++ show r9 ++ " R10 : " ++ show r10 ++ " R11 : " ++ show r11 ++ " R12 : " ++ show r12 ++ " R13 : " ++ show r13 ++ " R14 : " ++ show r14 ++ " R15 : " ++ show r15 ++ "]"

data Asm dest source immediate =
    -- Memory
    MoveI        dest immediate                     -- r <- val
    -- | MoveLabel    RegIdx LabelAddr                                    -- r <- *label
    | Move         dest source                   -- r <- [from]
    | LoadIdx      dest source immediate   -- r <- [[base] + offset]
    | LoadBaseIdx  dest source source -- r <- [[base] + [R_offset]]
    | StoreIdx     dest source immediate   -- r -> [[base] + offset]
    | StoreBaseIdx dest source source -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  dest source source -- r <- [x] + [y]
    | AddI dest source immediate    -- r <- [x] + i
    | Sub  dest source source -- r <- [x] - [y]
    | SubI dest source immediate     -- r <- [x] - i
    | Mult dest source source -- r <- [x] * [y]
    | Div  dest source source -- r <- [x] / [y]
    | Eq   dest source source -- r <- [x] == [y]
    | Lt   dest source source -- r <- [x] < [y]
    | Or   dest source source -- r <- [x] || [y]
    | And  dest source source -- r <- [x] && [y]
    | Not  dest source              -- r <- ![x]
    -- Branching
    | B  immediate              -- Unconditional branch to label.
    | BT dest immediate -- Branch to label if r == 1
    | BF dest immediate -- Branch to label if r == 0
    | Ret                                -- Branch to address in link register.
    | SysCall                            -- Terminates execution.
    -- Debugging
    | Print  source -- Print value in a register.
    | PrintC source -- Print value in a register as an ASCII character.
    | PrintLn                -- Print a newline.
    -- Extra
    | NoOp
    | Label immediate

data Assembly dest source immediate
                        = ADD  dest source source
                        | ADDI source source immediate
                        | BEQ  source source immediate 
                        | LW   dest source 
                        | LI   dest   immediate
                        | SW   source dest
                        | SI   source immediate
                        | CMP  dest source source
                        | LTH  dest source source
                        | JMP  immediate
                        | BLT  source source immediate
                        deriving Show

data InstructionResult  = InstructionResult { 
                            output :: (Instruction, Int)
                        } 

data CPU                = CPU {
                            i_memory        :: IMemory,
                            d_memory        :: Memory,
                            rs_station      :: ReservationStation,
                            rob             :: ReorderBuffer,
                            registers       :: Registers,
                            pc              :: Int,
                            npc             :: Int,
                            executionUnits  :: Units,
                            fetchUnit       :: Unit,
                            decodeUnit      :: Unit,
                            branch_predictor :: BranchPredictor,
                            renamer         :: RenamingTable
                        } 

instance Show CPU where
    show (CPU imem dmem rs_station rob reg pc npc exec fetch decode branch_predictor renamer) = 
        "InstructionMemory: " ++ show imem ++ "\n" ++
        "DataMemory: " ++ show dmem ++ "\n" ++
        "ReservationStation: " ++ show rs_station ++ "\n" ++
        "ReorderBuffer: " ++ show rob ++ "\n" ++
        "Registers: " ++ show reg ++ "\n" ++
        "PC: " ++ show pc ++ ", NPC: " ++ show npc ++ "\n" ++
        "ExecutionUnits: " ++ show exec ++ 
        "FetchUnit: " ++ show fetch ++ "\n" ++
        "DecodeUnit: " ++ show decode ++ "\n" ++
        "BranchPredictor: " ++ show branch_predictor ++ "\n" ++
        "Renamer: " ++ show renamer ++ "\n"



data UnitType           = IntUnit | MemUnit | BranchUnit

data UnitId             = Int_Unit1 | Int_Unit2 | Mem_Unit | Branch_Unit | Fetch_Unit | Decode_Unit

data Unit               = Unit { 
                            unitId      :: UnitId,
                            cycles      :: Int,
                            instruction :: Maybe InstructionAndPc,
                            rs_id       :: RSId,
                            rs_cycle    :: Int, 
                            buffer      :: V.Vector InstructionAndPc
                        }  

instance Show Unit where 
    show (Unit unit_id cycle instrct  rsid rscycle buff) 
        = "[Cycles: " ++ show cycle ++ ", Instruction: " ++ show instrct ++ ", RSId: " ++ show rsid ++ 
            ", RSCycle: " ++ show rscycle ++ ", Buffer: " ++ show buff ++ "]"

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
                            rs_instruction  :: InstructionAndPc,
                            qd              :: Int,
                            qj              :: Int,
                            qk              :: Int,
                            vj              :: Int,
                            vk              :: Int,
                            addr            :: Int,
                            busy            :: Bool
                        } deriving (Show)

type ROBId              = Int 

data ROBEntry           = ROBEntry {
                            rob_instruction :: InstructionAndPc,
                            rob_value       :: Int
                        } deriving Show

data ReorderBuffer      = ReorderBuffer { 
                            rob_buffer :: [(ROBId, Maybe ROBEntry)]
                          } deriving Show

type RegisterStatuses   = Map.Map RegisterNum Int 

data ReservationStation = ReservationStation {
                            rs_entries      :: RSs,
                            reg_statuses    :: RegisterStatuses
                        } 

instance Show ReservationStation where 
    show (ReservationStation entries statuses) = "RS_Entries: \n"  ++ unwords (map (\entry -> show entry ++ "\n") (Map.toList entries)) ++ ", RS_Statuses: " ++ show statuses

data BranchHistory      = B00 | B01 | B10 | B11 deriving (Eq, Show)

instance Ord BranchHistory where 
    compare branch1 branch2 = (branchBinary branch1) `compare` (branchBinary branch2)
        where branchBinary branch = case branch of 
                                            B00 -> 0
                                            B01 -> 1
                                            B10 -> 2
                                            B11 -> 3

data BranchPredictor    = BranchPredictor { 
                            branch_table :: Map.Map BranchHistory Int,
                            branch_reg   :: BranchHistory,
                            predictions  :: Map.Map Int Bool
                        } deriving Show


data RenamingTable      = RenamingTable {
                            renameTable :: Map.Map RegisterNum RegisterNum,
                            freeRegisters :: Map.Map RegisterNum Bool
                        } deriving Show

initRenamingTable :: RenamingTable
initRenamingTable = RenamingTable (Map.fromList []) (Map.fromList (zip allRegNums (replicate 16 True)))

initBranchPredictor :: BranchPredictor
initBranchPredictor = BranchPredictor (Map.fromList [(B00, 1), (B01, 1), (B10, 1), (B11, 1)]) B00 (Map.fromList [])

initReorderBuffer :: ReorderBuffer
initReorderBuffer   = ReorderBuffer [ (i, Nothing) | i <- [1 .. 10]]

initRegisterStatuses :: RegisterStatuses
initRegisterStatuses = Map.fromList [(R0, 0), (R1, 0), (R2, 0), (R3, 0), (R4, 0), (R5, 0), (R6, 0), (R7, 0), (R8, 0), (R9, 0), (R10, 0), (R11, 0), (R12, 0), (R13, 0), (R14, 0), (R15, 0)]

initReservationStation :: ReservationStation
initReservationStation = ReservationStation (Map.fromList $ map (\i -> (i, (0, Nothing))) [1..4]) initRegisterStatuses

initRegisters :: Registers 
initRegisters = Registers (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)
                          (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

initUnit :: UnitId -> Unit 
initUnit unit_id = Unit unit_id 0 Nothing 0 0 V.empty 

initCPU :: [Instruction] -> CPU 
initCPU instructions = let i_mem = V.fromList instructions 
                           d_mem = V.replicate 30 (fromIntegral 0)
                           rs_station = initReservationStation
                           reorderBuff = initReorderBuffer
                           registers = initRegisters
                           pc = fromIntegral 0
                           npc = fromIntegral 0
                           eunits =  Units (initUnit Int_Unit1) (initUnit Int_Unit2) (initUnit Mem_Unit) (initUnit Branch_Unit)
                           funit = initUnit Fetch_Unit
                           dunit = initUnit Decode_Unit
                           branch_pred = initBranchPredictor
                           renamer = initRenamingTable
                        in CPU  i_mem d_mem rs_station reorderBuff registers pc npc eunits funit dunit branch_pred renamer

writeMemory :: CPU -> Int -> RegisterNum -> Memory
writeMemory cpu i s = d_memory cpu V.// [(fromIntegral i, (fromIntegral $ readRegister (registers cpu) s))]

writeRegister :: Registers -> RegisterNum -> Int -> Registers
writeRegister registers regNum writeVal
    = case regNum of R0 -> registers { r0 = writeVal }
                     R1 -> registers { r1 = writeVal } 
                     R2 -> registers { r2 = writeVal } 
                     R3 -> registers { r3 = writeVal } 
                     R4 -> registers { r4 = writeVal } 
                     R5 -> registers { r5 = writeVal } 
                     R6 -> registers { r6 = writeVal } 
                     R7 -> registers { r7 = writeVal } 
                     R8 -> registers { r8 = writeVal }
                     R9 -> registers { r9 = writeVal } 
                     R10 -> registers { r10 = writeVal } 
                     R11 -> registers { r11 = writeVal } 
                     R12 -> registers { r12 = writeVal } 
                     R13 -> registers { r13 = writeVal } 
                     R14 -> registers { r14 = writeVal } 
                     R15 -> registers { r15 = writeVal } 

readRegister :: Registers -> RegisterNum -> Int
readRegister registers regNum
    = case regNum of 
                    R0 -> r0 registers
                    R1 -> r1 registers 
                    R2 -> r2 registers
                    R3 -> r3 registers
                    R4 -> r4 registers
                    R5 -> r5 registers
                    R6 -> r6 registers
                    R7 -> r7 registers
                    R8 -> r8 registers
                    R9 -> r9 registers 
                    R10 -> r10 registers
                    R11 -> r11 registers
                    R12 -> r12 registers
                    R13 -> r13 registers
                    R14 -> r14 registers
                    R15 -> r15 registers

allRegNums :: [RegisterNum]
allRegNums = [R0 , R1 , R2 , R3 , R4 , R5 , R6 , R7 , R8 , R9 , R10 , R11 , R12 , R13 , R14 , R15]

toRegisterNum :: Int -> RegisterNum
toRegisterNum regNum 
    = case regNum of    
                        0 -> R0 
                        1 -> R1 
                        2 -> R2 
                        3 -> R3
                        4 -> R4 
                        5 -> R5 
                        6 -> R6
                        7 -> R7
                        8 -> R0 
                        9 -> R9
                        10 -> R10 
                        11 -> R11
                        12 -> R12 
                        13 -> R13 
                        14 -> R14
                        15 -> R15

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
flushPipeline cpu = cpu { rs_station = initReservationStation, 
                          rob = initReorderBuffer,
                          decodeUnit = initUnit Decode_Unit, fetchUnit = initUnit Fetch_Unit, executionUnits = Units (initUnit Int_Unit1) (initUnit Int_Unit2) (initUnit Mem_Unit) (initUnit Branch_Unit)}
                       
instructionToExecutionUnit :: Instruction -> UnitType
instructionToExecutionUnit instruction = 
        case instruction of ADD _ _ _ -> IntUnit
                            ADDI _ _ _-> IntUnit 
                            BEQ _ _ _-> BranchUnit
                            BLT _ _ _-> BranchUnit
                            LTH _ _ _ -> IntUnit
                            CMP _ _ _ -> IntUnit
                            JMP  _ -> BranchUnit
                            LW _ _ -> MemUnit 
                            LI _ _ -> MemUnit 
                            SW _ _ -> MemUnit 
                            SI _ _ -> MemUnit



allocateRegStats :: RegisterStatuses -> Instruction -> RegisterStatuses
allocateRegStats regstats instrct = 
    case instrct of 
            ADD  d s1 s2 -> (setRegStat s1 0 . setRegStat s2 0 . setRegStat d 0) regstats
            CMP  d s1 s2 -> (setRegStat s1 0 . setRegStat s2 0 . setRegStat d 0) regstats
            ADDI s1 s2 i -> (setRegStat s1 0 . setRegStat s2 0) regstats
            LTH   d s1 s2 -> (setRegStat s1 0 . setRegStat s2 0 . setRegStat d 0) regstats
            BEQ  s1 s2 i -> (setRegStat s1 0 . setRegStat s2 0) regstats
            BLT  s1 s2 i -> (setRegStat s1 0 . setRegStat s2 0) regstats
            LW   s1 s2   -> (setRegStat s1 0 . setRegStat s2 0) regstats
            LI   d i     -> (setRegStat d 0) regstats
            SW   s1 s2   -> (setRegStat s1 0 . setRegStat s2 0) regstats
            SI   d i     -> (setRegStat d 0) regstats
            JMP i        ->  regstats

deallocateRegStats :: RegisterStatuses -> Instruction -> RSId -> Int -> Int -> Int -> RegisterStatuses
deallocateRegStats regstats instrct rsid d_status s1_status s2_status = 
    let f = \reg rsID status reg_stats -> if status == 0 then setRegStat reg rsID reg_stats else reg_stats
    in case instrct of 
            ADD  d s1 s2 -> (f s1 rsid s1_status . f s2 rsid s2_status . f d rsid d_status) regstats
            CMP  d s1 s2 -> (f s1 rsid s1_status . f s2 rsid s2_status . f d rsid d_status) regstats 
            ADDI s1 s2 i -> (f s1 rsid s1_status . f s2 rsid s2_status)  regstats
            LTH   d s1 s2 -> (f s1 rsid s1_status . f s2 rsid s2_status . f d rsid d_status) regstats
            BEQ  s1 s2 i -> (f s1 rsid s1_status . f s2 rsid s2_status) regstats
            BLT  s1 s2 i -> (f s1 rsid s1_status . f s2 rsid s2_status) regstats
            LW   s1 s2   -> (f s1 rsid s1_status . f s2 rsid s2_status) regstats
            LI   d i     -> (f d rsid d_status) regstats
            SW   s1 s2   -> (f s1 rsid s1_status . f s2 rsid s2_status) regstats
            SI   d i     -> (f d rsid d_status) regstats
            JMP  i     ->  regstats

allocateRSEntry :: RSs -> RSId -> RSs 
allocateRSEntry rs rsid = Map.adjust (\(cycle, _) -> (cycle, Nothing)) rsid rs

changeRSEntry :: RSs -> RSId -> RSEntry -> RSs 
changeRSEntry rs rsid entry = Map.adjust (\(cycle, _) -> (cycle, Just entry)) rsid rs

euToROB :: (InstructionAndPc, Int) -> ROBEntry
euToROB (instrct, val) = ROBEntry instrct val

sameRegs :: Instruction -> Bool
sameRegs instrct = 
    case instrct of 
            ADD  d s1 s2 -> s1 == d || s2 == d
            CMP  d s1 s2 -> s1 == d || s2 == d
            ADDI s1 s2 i -> s1 == s2
            LTH   d s1 s2 -> s1 == d || s2 == d
            BEQ  s1 s2 i -> False
            BLT  s1 s2 i -> False
            LW   s1 s2   -> s1 == s2
            LI   d i     -> False
            SW   s1 s2   -> False
            SI   d i     -> False
            JMP i        -> False
