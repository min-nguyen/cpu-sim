module ReorderBuffer where

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
import Renamer
import Data.Ord
import BranchPredictor 

-- type ROBId              = Int 

-- data ROBEntry           = ROBEntry {
--                             rob_instruction :: Instruction,
--                             rob_value       :: Word32
--                         } deriving Show

-- data ReorderBuffer      = ReorderBuffer [(ROBId, Maybe ROBEntry)] deriving Show

updateROB :: CPU -> CPU
updateROB cpu =
    let commitRobEntries cp  = case popReorderBuffer cp of 
                                    (cp', True) -> commitRobEntries cp'
                                    (cp', False) -> cp'
    in  commitRobEntries cpu

insertReorderBuffer :: ROBId -> ROBEntry -> ReorderBuffer -> ReorderBuffer
insertReorderBuffer robId robEntry reorderBuff = 
    let ReorderBuffer entries = reorderBuff
        headId = fst $ head entries
        offset = robId - headId
        entries' =  replaceNth offset (robId, Just robEntry) entries 
    in  ReorderBuffer entries'
    where replaceNth :: Int -> a -> [a] -> [a]
          replaceNth _ _ [] = []
          replaceNth n newVal (x:xs)
           | n == 0 = newVal:xs
           | otherwise = x:replaceNth (n-1) newVal xs

popReorderBuffer :: CPU -> (CPU, Bool)
popReorderBuffer cpu = 
    let reorderBuff = rob cpu
        (robId, maybeEntry) = head (rob_buffer reorderBuff)
    in  case maybeEntry of Nothing -> (cpu, False)
                           Just entry -> commitReorderBuffer entry reorderBuff cpu

commitReorderBuffer :: ROBEntry -> ReorderBuffer -> CPU -> (CPU, Bool)            
commitReorderBuffer entry reorderBuff cpu =
    let lastRobId    = fst (last ( rob_buffer reorderBuff)) + 1

        reorderBuff' = ReorderBuffer $ tail (rob_buffer reorderBuff) ++ [(lastRobId, Nothing)]
       
        cpu'' = case entry of 
            ROBEntry (ADD  d s1 s2, pc) value -> let registers' = writeRegister (registers cpu) d value 
                                                 in  (cpu {registers = registers', rob = reorderBuff'}, True)
            ROBEntry (LTH  d s1 s2, pc) value -> let registers' = writeRegister (registers cpu) d value 
                                                 in  (cpu {registers = registers', rob = reorderBuff'}, True)
            ROBEntry (CMP  d s1 s2, pc) value -> let registers' = writeRegister (registers cpu) d value 
                                                 in  (cpu {registers = registers', rob = reorderBuff'}, True)
            ROBEntry (ADDI s1 s2 i, pc) value -> let registers' = writeRegister (registers cpu) s1 value 
                                                 in  (cpu {registers = registers', rob = reorderBuff'}, True)
            ROBEntry (BEQ  s1 s2 i, pc) value -> let (cpu', correctBranch) = case value of 0 -> (updateBranchPredictor False (rob_instruction entry) cpu)
                                                                                           1 -> (updateBranchPredictor True (rob_instruction entry) cpu)
                                                     npc' = if correctBranch then npc cpu' else ( case value of 0 -> (fromIntegral $ snd $ rob_instruction entry) + 1
                                                                                                                1 -> i)
                                                     cpu'' = (cpu' {npc = npc'})
                                                 in  if correctBranch 
                                                     then (cpu'' {rob = reorderBuff'}, True)
                                                     else (flushPipeline cpu'', False) -- << ---
            ROBEntry (BLT  s1 s2 i, pc) value -> let (cpu', correctBranch) = case value of 0 -> (updateBranchPredictor False (rob_instruction entry) cpu)
                                                                                           1 -> (updateBranchPredictor True (rob_instruction entry) cpu)
                                                     npc' = if correctBranch then npc cpu' else ( case value of 0 -> (fromIntegral $ snd $ rob_instruction entry) + 1
                                                                                                                1 -> i)
                                                     cpu'' = (cpu' {npc = npc'})
                                                 in  if correctBranch 
                                                     then (cpu'' {rob = reorderBuff'}, True)
                                                     else (flushPipeline cpu'', False) -- << ---
            ROBEntry (LW   d s1  , pc) value -> let registers' = writeRegister (registers cpu) d value 
                                                in  (cpu {registers = registers', rob = reorderBuff'}, True )
            ROBEntry (LI   d i, pc) value    -> let registers' = writeRegister (registers cpu) d value 
                                                in  (cpu {registers = registers', rob = reorderBuff'}, True)
            ROBEntry (SW  s1 s2, pc) value   -> let memory' = writeMemory cpu (fromIntegral value) s2
                                                in  (cpu {d_memory = memory', rob = reorderBuff'} , True)
            ROBEntry (SI   d i, pc) value    -> let memory' = writeMemory cpu (fromIntegral i) d
                                                in  (cpu {d_memory = memory', rob = reorderBuff'} , True)
            ROBEntry (JMP i, pc) value       -> (cpu {npc = i, rob = reorderBuff'}, True) 
    in trace ("ROB ENTRY COMMITED : " ++ show entry ++ "\nCPU AFTER : \n" ++ show cpu'' ++ "\n\n") cpu''