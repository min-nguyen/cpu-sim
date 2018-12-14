module ReorderBuffer where

import Lib
import Data.Word
import Data.Maybe
import Data.Bits
import Control.Applicative hiding (Const)
import qualified Data.Vector as V
import Debug.Trace
import qualified Data.Map.Strict as Map
import Utils
import Data.List
import Renamer
import Data.Ord
import BranchPredictor 
import Execution

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

popReorderBuffer :: CPU -> (CPU, Bool)
popReorderBuffer cpu = 
    let reorderBuff = rob cpu
        (robId, maybeEntry) = head (rob_buffer reorderBuff)
    in  trace ("ROB BUFFER " ++ show (rob_buffer reorderBuff)) $
        case maybeEntry of Nothing -> (cpu, False)
                           Just entry -> commitReorderBuffer entry reorderBuff cpu

commitReorderBuffer :: ROBEntry -> ReorderBuffer -> CPU -> (CPU, Bool)            
commitReorderBuffer entry reorderBuff cpu =
    let lastRobId    = fst (last ( rob_buffer reorderBuff)) + 1

        reorderBuff' = ReorderBuffer $ tail (rob_buffer reorderBuff) ++ [(lastRobId, Nothing)]
        cpu' = cpu -- updateFreeRegisters (fst $ rob_instruction entry) 
        
        cpu'' = case execInstruction cpu' (rob_instruction entry) of 
            ((Add  d s1 s2, pc), execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in   (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Sub  d s1 s2, pc), execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Mult  d s1 s2, pc), execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Div  d s1 s2, pc), execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Or  d s1 s2, pc), execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((And  d s1 s2, pc), execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)                   
            ((Lt  d s1 s2, pc), execResult)  -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Eq  d s1 s2, pc), execResult) ->  case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Not d x, pc), execResult)      -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((AddI s1 s2 i, pc) ,execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') s1 value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((SubI s1 s2 i, pc) ,execResult) -> case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') s1 value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((MoveI d i, pc) ,execResult) ->   case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d i 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((Move d s, pc), execResult) ->    case execResult of
                                                        Const value ->
                                                            let registers' = writeRegister (registers cpu') d value 
                                                            in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((LoadIdx d s1 i, pc) ,execResult) -> case execResult of
                                                            Const value ->
                                                                let registers' = writeRegister (registers cpu') d value 
                                                                in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((LoadBaseIdx d s1 s2, pc), execResult) ->  case execResult of
                                                                Const value ->
                                                                    let registers' = writeRegister (registers cpu') d value 
                                                                    in  (cpu' {registers = registers', rob = reorderBuff'}, True)
            ((StoreIdx r b i, pc), execResult) ->  case execResult of
                                                                Tuple (value, addr) ->
                                                                    let memory' = writeMemoryI cpu' value addr
                                                                    in  (cpu' {d_memory = memory', rob = reorderBuff'} , True)
            ((StoreBaseIdx r b o, pc), execResult) -> case execResult of
                                                                Tuple (value, addr) ->
                                                                    let memory' = writeMemoryI cpu' (fromIntegral value) addr
                                                                    in  (cpu' {d_memory = memory', rob = reorderBuff'} , True)
            ((B i, pc), execResult)         -> case execResult of
                                                        Tuple (value, link) -> (cpu' {rob = reorderBuff'}, False)     
            ((BT  s1    i, pc), execResult) ->  case execResult of
                                                        Tuple (value, link) -> 
                                                            let (cpu_', correctBranch) = case value of  0 -> (updateBranchPredictor False (BT  s1    i, pc) cpu')
                                                                                                        1 -> (updateBranchPredictor True (BT  s1    i, pc) cpu')
                                                                npc' = if correctBranch then npc cpu_' else ( case value of 0 -> pc + 1
                                                                                                                            1 -> i)
                                                                cpu'' = (cpu_' {npc = npc'})
                                                            in  
                                                                if correctBranch 
                                                                then (cpu'' {rob = reorderBuff'}, True)
                                                                else (flushPipeline cpu'', False) -- << ---
            ((BF  s1    i, pc), execResult) ->  case execResult of
                                                        Tuple (value, link) ->    
                                                            let (cpu_', correctBranch) = case value of  0 -> (updateBranchPredictor False (BF  s1    i, pc) cpu')
                                                                                                        1 -> (updateBranchPredictor True (BF  s1    i, pc) cpu')
                                                                npc' = if correctBranch then npc cpu_' else ( case value of 0 -> pc + 1
                                                                                                                            1 -> i)
                                                                cpu'' = (cpu_' {npc = npc'})
                                                            in  if correctBranch 
                                                                then (cpu'' {rob = reorderBuff'}, False)
                                                                else (flushPipeline cpu'', False) -- << ---
            ((End, pc) ,execResult)     -> case execResult of
                                                            Const value -> (cpu' {rob = reorderBuff',
                                                                                  active = False} , False)

    in trace ("ROB ENTRY COMMITED : " ++ show entry ++ "\n" ++ show cpu'' ++ "\n\n") cpu''