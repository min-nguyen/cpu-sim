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
import Data.Ord

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
        entries' =  trace ("OFFSET" ++ show offset ++ " " ++ show headId ++ " " ++ show robId) $ replaceNth offset (robId, Just robEntry) entries 
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
                           Just entry -> (commitReorderBuffer entry reorderBuff cpu, True)

commitReorderBuffer :: ROBEntry -> ReorderBuffer -> CPU ->  CPU                       
commitReorderBuffer entry reorderBuff cpu =
    let lastRobId    = fst $ last ( rob_buffer reorderBuff)

        reorderBuff' = trace (show lastRobId) $ ReorderBuffer $ tail (rob_buffer reorderBuff) ++ [(lastRobId, Nothing)]
        cpu' = case entry of 
            ROBEntry (ADD  d s1 s2) value -> let registers' = writeRegister (registers cpu) d value 
                                             in  cpu {registers = registers', rob = reorderBuff'}
            ROBEntry (ADDI s1 s2 i) value -> let registers' = writeRegister (registers cpu) s1 value 
                                             in  cpu {registers = registers', rob = reorderBuff'}
            ROBEntry (BEQ  s1 s2 i) value -> let npc' = case value of 0 -> npc cpu
                                                                      1 -> i 
                                             in  cpu {npc = npc'}
            ROBEntry (LW   d s1 i) value -> let registers' = writeRegister (registers cpu) d value 
                                            in  cpu {registers = registers', rob = reorderBuff'}
            ROBEntry (LI   d i) value    -> let registers' = writeRegister (registers cpu) d value 
                                            in  cpu {registers = registers', rob = reorderBuff'}
            ROBEntry (SW   d i) value    -> let memory' = writeMemory cpu (fromIntegral i) d
                                            in  cpu {d_memory = memory', rob = reorderBuff'} 
            -- (JALR d s, value)     -> 
    in cpu'