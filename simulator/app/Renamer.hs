module Renamer where

import Lib
import Data.Word
import Data.Maybe
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace
import qualified Data.Map.Strict as Map
import Utils
import Data.List
import Data.Ord

getFreeRegister :: CPU -> RegisterNum -> Maybe RegisterNum
getFreeRegister cpu reg_num = case available_regs of [] -> Nothing 
                                                     xs -> if reg_num `elem` xs 
                                                           then Just reg_num
                                                           else Just $ head xs
    where free_regs = freeRegisters (renamer cpu)  
          available_regs = map fst $ filter (\(r, b) -> b == True) (Map.toList free_regs)

renameInstructionRegs :: Instruction -> CPU -> (CPU, Instruction)
renameInstructionRegs instrct cpu =
    case instrct of
        ADD  d s1 s2 -> let (cpu', d', available) = renameRegister d cpu
                            (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                            s1' = remapRegister s1 cpu
                            s2' = remapRegister s2 cpu
                        in  (cpu'', ADD d'' s1' s2')
        LTH   d s1 s2 -> let (cpu', d', available) = renameRegister d cpu
                             (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                             s1' = remapRegister s1 cpu
                             s2' = remapRegister s2 cpu
                         in  (cpu'', LTH d'' s1' s2')
        CMP   d s1 s2 -> let (cpu', d', available) = renameRegister d cpu
                             (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                             s1' = remapRegister s1 cpu
                             s2' = remapRegister s2 cpu
                         in  (cpu'', CMP d'' s1' s2')
                                
        ADDI d s i   -> let (cpu', d', available) = renameRegister d cpu
                            (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                            s' = remapRegister s cpu
                        in  (cpu'', ADDI d'' s' i)
        BEQ  s1 s2 i -> let s1' = remapRegister s1 cpu 
                            s2' = remapRegister s2 cpu
                        in  (cpu, BEQ s1' s2' i)
        BLT  s1 s2 i -> let s1' = remapRegister s1 cpu 
                            s2' = remapRegister s2 cpu
                        in  (cpu, BLT s1' s2' i)
        LW   d s    ->  let (cpu', d', available) = renameRegister d cpu
                            (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                            s' = remapRegister s cpu
                        in  (cpu'', LW d'' s' )
        LI   d i     -> let (cpu', d', available) = renameRegister d cpu
                            (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')   
                        in  (cpu'', LI d'' i)
        SI   s i     -> let 
                            s' = remapRegister s cpu
                        in  (cpu, SI s' i)
        SW   s1 s2   -> let 
                            s1' = remapRegister s1 cpu
                            s2' = remapRegister s2 cpu
                        in  (cpu, SW s1' s2' )
        JMP i           -> (cpu, JMP i)
    -- [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
    -- [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
    -- invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
    -- [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]


renameRegister :: RegisterNum -> CPU -> (CPU, RegisterNum, Bool)
renameRegister reg_num cpu = 
        case getFreeRegister cpu reg_num  of 
            Nothing -> let rename_table' = Map.insert reg_num reg_num rename_table
                       in trace ("no free reg for : " ++ show reg_num) $ (cpu {renamer = (renamer cpu) {renameTable = rename_table'}}, reg_num, False)
            Just r' -> let rename_table' = Map.insert reg_num r' rename_table
                          
                           free_regs'    = Map.insert reg_num False free_regs
                       in (cpu {renamer = (renamer cpu) {renameTable = rename_table', freeRegisters = free_regs'}}, r', True)
    where rename_table = renameTable (renamer cpu)   
          free_regs = freeRegisters (renamer cpu)

remapRegister :: RegisterNum -> CPU -> RegisterNum
remapRegister reg_num cpu = 
    case Map.lookup reg_num rename_table of 
        Nothing -> reg_num
        Just r  -> r 
    where rename_table = renameTable (renamer cpu)   

updateFreeRegisters :: Instruction -> CPU -> CPU
updateFreeRegisters instruction cpu = 
    let free_regs = freeRegisters (renamer cpu)
        free_regs' = case instruction of ADD d _ _ -> Map.insert d True free_regs
                                         ADDI d _ _-> Map.insert d True free_regs
                                         BEQ _ _ _-> free_regs
                                         BLT _ _ _ -> free_regs
                                         LW d _  -> Map.insert d True free_regs
                                         LTH d _ _ -> Map.insert d True free_regs
                                         CMP d _ _ -> Map.insert d True free_regs
                                         LI d _ -> Map.insert d True free_regs
                                         SW _ _ -> free_regs
                                         SI _ _ -> free_regs
                                         JMP _  -> free_regs
    in  cpu {renamer = (renamer cpu) {freeRegisters = free_regs'}}