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
import Control.Lens hiding (Const)
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
    in  
        case maybeEntry of Nothing -> (cpu, False)
                           Just entry -> commitReorderBuffer entry reorderBuff cpu

commitReorderBuffer :: ROBEntry -> ReorderBuffer -> CPU -> (CPU, Bool)            
commitReorderBuffer entry reorderBuff cpu =
    let lastRobId    = fst (last ( rob_buffer reorderBuff)) + 1

        reorderBuff' = ReorderBuffer $ tail (rob_buffer reorderBuff) ++ [(lastRobId, Nothing)]
        cpu' = cpu { stats = (stats cpu) & instructions_committed %~ (+1) }
        -- updateFreeRegisters (fst $ rob_instruction entry) 
        
        cpu'' = case (rob_instruction entry, rob_value entry) of --execInstruction cpu' 
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
                                                            Tuple (addr,value) ->
                                                                let cpu_ = loadAddress cpu' addr value d
                                                                in (cpu_ {rob = reorderBuff'}, True)

            ((LoadBaseIdx d s1 s2, pc), execResult) ->  case execResult of
                                                            Tuple (addr,value) ->
                                                                    let cpu_ = loadAddress cpu' addr value d
                                                                    in (cpu_ {rob = reorderBuff'}, True)
            ((StoreIdx r b i, pc), execResult) ->  case execResult of
                                                                Tuple (addr, value ) ->
                                                                    let cpu_' = storeAddress cpu' addr value
                                                                    in  (cpu_' {rob = reorderBuff'} , True)
            ((StoreBaseIdx r b o, pc), execResult) -> case execResult of
                                                                Tuple (addr, value ) ->
                                                                    let cpu_' = storeAddress cpu' addr value
                                                                        
                                                                    in  (cpu_' {rob = reorderBuff'} , True)
            ((B i, pc), execResult)         -> case execResult of
                                                        Tuple (value, link) -> (cpu' {rob = reorderBuff'}, False)     
            ((BT  s1    i, pc), execResult) ->  case execResult of
                                                        Tuple (value, link) -> 
                                                            let (cpu_', correctBranch) = case value of  0 -> (updateBranchPredictor False (BT  s1    i, pc) cpu')
                                                                                                        1 -> (updateBranchPredictor True  (BT  s1    i, pc) cpu')
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
                                                            Const value ->  (writeCacheToMem (cpu' {rob = reorderBuff',
                                                                                                    active = False}) , False)
                                                                            

    in trace ("ROB ENTRY COMMITED : " ++ show entry ++ "\n" ++ show cpu'' ++ "\n\n") $ 
        cpu'' 




loadAddress  :: CPU -> Int -> Int -> RegisterNum -> CPU
loadAddress cpu addr value d = 
    case cache_config (config cpu) of 
        NoCache -> let registers' = writeRegister (registers cpu) d value  in  (cpu {registers = registers'}) 
        L1Cache -> loadAddressL1Config  cpu addr value d
        L1L2Cache -> loadAddressL1L2Config cpu addr value d

loadAddressL1Config ::  CPU -> Int -> Int -> RegisterNum -> CPU
loadAddressL1Config cpu addr value d =
    case cache_policy (config cpu) of 
        Fifo -> let l1' = l1_fifo (l1_cache cpu)
                    
                in case Map.lookup addr l1' of 
                        Nothing ->  let registers' = writeRegister (registers cpu) d value  
                                        cpu' = insertL1Cache addr value cpu
                                                  
                                    in  (cpu' {registers = registers'})
                        Just (time, l1_val) ->  let registers' =  writeRegister (registers cpu) d l1_val 
                                                in (cpu {registers = registers'})        
        Lru  -> let l1' = l1_lru (l1_cache cpu)
                   
                in case find (\(address, val) -> address == addr) l1' of 
                        Nothing ->  let registers' = writeRegister (registers cpu) d value  
                                        cpu' = insertL1Cache addr value cpu
                                    in  (cpu' {registers = registers'}) 

                        Just (address, l1_val) ->  let registers' =  writeRegister (registers cpu) d l1_val 
                                                       cpu' = insertL1Cache addr value cpu
                                                   in (cpu' {registers = registers'}) 
        Mru  -> let l1' = l1_mru (l1_cache cpu)
                in case find (\(address, val) -> address == addr) l1' of 
                        Nothing ->  let registers' = writeRegister (registers cpu) d value  
                                        cpu' = insertL1Cache addr value cpu
                                    in  (cpu' {registers = registers'}) 
                                     
                        Just (address, l1_val) ->  let registers' =  writeRegister (registers cpu) d l1_val 
                                                       cpu' = insertL1Cache addr value cpu
                                                   in (cpu' {registers = registers'}) 


loadAddressL1L2Config ::  CPU -> Int -> Int -> RegisterNum -> CPU
loadAddressL1L2Config cpu addr value d =
    case cache_policy (config cpu) of 
        Fifo -> let l1' = l1_fifo (l1_cache cpu)
                    l2' = l2_fifo (l2_cache cpu)
                in case Map.lookup addr l1' of 
                        Nothing ->  case Map.lookup addr l2' of 
                                            Nothing ->  let registers' = writeRegister (registers cpu) d value  
                                                            cpu' = insertL1Cache addr value cpu
                                                            -- cpu_'' = insertL2Cache addr value cpu_'
                                                        in  (cpu' {registers = registers'}) 
                                            Just (time, l2_val) ->  let registers' = writeRegister (registers cpu) d l2_val  
                                                                        cpu' = insertL1Cache addr l2_val cpu
                                                                        l2'' = L2Fifo $ Map.delete addr (l2_fifo $ l2_cache cpu') 
                                                                    in  (cpu' {l2_cache = l2'', registers = registers'}) 
                        Just (time, l1_val) ->  let registers' =  writeRegister (registers cpu) d l1_val 
                                                in (cpu {registers = registers'})        
        Lru  -> let l1' = l1_lru (l1_cache cpu)
                    l2' = l2_lru (l2_cache cpu)
                in case find (\(address, val) -> address == addr) l1' of 
                        Nothing ->  case find (\(address, val) -> address == addr) l2' of 
                                            Nothing ->  let registers' = writeRegister (registers cpu) d value  
                                                            cpu' = insertL1Cache addr value cpu
                                                            -- cpu_'' = insertL2Cache addr value cpu_'
                                                        in  (cpu' {registers = registers'}) 
                                            Just (address, l2_val) ->   let registers' = writeRegister (registers cpu) d l2_val  
                                                                            cpu' = insertL1Cache addr l2_val cpu
                                                                            l2'' = L2Lru $ removeItem (address, l2_val) (l2_lru $ l2_cache cpu') 
                                                                        in  (cpu' {l2_cache = l2'', registers = registers'}) 

                        Just (address, l1_val) ->  let registers' =  writeRegister (registers cpu) d l1_val 
                                                   in (cpu {registers = registers'}) 
        Mru  -> let l1' = l1_mru (l1_cache cpu)
                    l2' = l2_mru (l2_cache cpu)
                in case find (\(address, val) -> address == addr) l1' of 
                        Nothing ->  case find (\(address, val) -> address == addr) l2' of 
                                            Nothing ->  let registers' = writeRegister (registers cpu) d value  
                                                            cpu' = insertL1Cache addr value cpu
                                                            -- cpu_'' = insertL2Cache addr value cpu_'
                                                        in  (cpu' {registers = registers'}) 
                                            Just (address, l2_val) ->   let registers' = writeRegister (registers cpu) d l2_val  
                                                                            cpu' = insertL1Cache addr l2_val cpu
                                                                            l2'' = L2Mru $ removeItem (address, l2_val) (l2_mru $ l2_cache cpu') 
                                                                        in  (cpu' {l2_cache = l2'', registers = registers'}) 

                        Just (address, l1_val) ->  let registers' =  writeRegister (registers cpu) d l1_val 
                                                   in (cpu {registers = registers'}) 



storeAddress :: CPU -> Int -> Int -> CPU 
storeAddress cpu addr value = 
    case cache_config (config cpu) of 
        NoCache -> cpu {d_memory = writeMemoryI cpu value addr} 
        L1Cache -> storeAddressL1Config  cpu addr value 
        L1L2Cache -> storeAddressL1L2Config cpu addr value 

storeAddressL1Config :: CPU -> Int -> Int -> CPU 
storeAddressL1Config cpu addr value =
    case cache_policy (config cpu) of 
        Fifo -> let l1' = l1_fifo (l1_cache cpu)
                    cpu_' = if Map.member (addr) l1'
                            then cpu { l1_cache = L1Fifo $ Map.adjust (\(time, val) -> (time, value)) addr l1' } 
                            else cpu {d_memory = writeMemoryI cpu value addr} 

                in  (cpu_')
        Lru ->  let l1' = l1_lru (l1_cache cpu)
                    cpu_' = if (addr) `elem` (map fst l1')
                            then cpu { l1_cache = L1Lru $ (removeUsing (\(address, val) -> address == addr) l1') ++ [(addr, value)] }
                            else cpu { d_memory = writeMemoryI cpu value addr} 

                in  (cpu_')
        Mru ->  let l1' = l1_mru (l1_cache cpu)
                    cpu_' = if (addr) `elem` (map fst l1')
                            then cpu { l1_cache = L1Mru $ [(addr, value)] ++ (removeUsing (\(address, val) -> address == addr) l1')}
                             else cpu { d_memory = writeMemoryI cpu value addr} 
                in  (cpu_')                                       

storeAddressL1L2Config :: CPU -> Int -> Int -> CPU 
storeAddressL1L2Config cpu addr value =
    case cache_policy (config cpu) of 
        Fifo -> let l1' = l1_fifo (l1_cache cpu)
                    l2' = l2_fifo (l2_cache cpu)
                    cpu_' = if Map.member (addr) l1'
                            then cpu { l1_cache = L1Fifo $ Map.adjust (\(time, val) -> (time, value)) addr l1' } 
                                else if Map.member (addr) l2' 
                                        then cpu { l2_cache = L2Fifo $ Map.adjust (\(time, val) -> (time, value)) addr l2' }  
                                        else cpu {d_memory = writeMemoryI cpu value addr} 

                in  (cpu_')
        Lru ->  let l1' = l1_lru (l1_cache cpu)
                    l2' = l2_lru (l2_cache cpu)
                    cpu_' = if (addr) `elem` (map fst l1')
                            then cpu { l1_cache = L1Lru $ (removeUsing (\(address, val) -> address == addr) l1') ++ [(addr, value)] }
                            else if (addr) `elem` (map fst l2')
                                 then  cpu { l2_cache = L2Lru $ (removeUsing (\(address, val) -> address == addr) l2') ++ [(addr, value)] }
                                 else cpu { d_memory = writeMemoryI cpu value addr} 

                in  (cpu_')
        Mru ->  let l1' = l1_mru (l1_cache cpu)
                    l2' = l2_mru (l2_cache cpu)
                    cpu_' = if (addr) `elem` (map fst l1')
                            then cpu { l1_cache = L1Mru $ [(addr, value)]  ++ (removeUsing (\(address, val) -> address == addr) l1')  }
                            else if (addr) `elem` (map fst l2')
                                 then  cpu { l2_cache = L2Mru $   [(addr, value)] ++ (removeUsing (\(address, val) -> address == addr) l2') }
                                 else cpu { d_memory = writeMemoryI cpu value addr} 

                in  (cpu_')