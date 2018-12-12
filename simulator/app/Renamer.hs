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
        Add  d s1 s2        -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Add d' s1' s2')
        AddI d s  i         -> let (cpu', (d', s')) = rename2regs d s in (cpu', AddI d' s' i)
        Sub  d s1 s2        -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Sub d' s1' s2')
        SubI d s  i         -> let (cpu', (d', s')) = rename2regs d s in (cpu', SubI d' s' i)
        Mult  d s1 s2       -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Mult d' s1' s2')
        Div  d s1 s2        -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Div d' s1' s2')
        Or  d s1 s2         -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Or d' s1' s2')
        Lt  d s1 s2         -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Lt d' s1' s2')
        Eq  d s1 s2         -> let (cpu', (d', s1', s2')) = rename3regs d s1 s2 in (cpu', Eq d' s1' s2')
        Not  d s            -> let (cpu', (d', s')) = rename2regs d s in (cpu', Not d' s')
        
        Move   d s          ->  let (cpu', (d', s')) = rename2regs d s in (cpu', Move d' s')
        MoveI  s1 i         ->  let (cpu', s1') = rename1reg s1 in (cpu', MoveI s1' i)
        LoadIdx d s i       ->  let (cpu', (d', s')) = rename2regs d s in (cpu', LoadIdx d' s' i)
        LoadBaseIdx d s1 s2 -> let (cpu', (d', s1', s2')) = rename3regs d s1 (toRegisterNum s2) in (cpu', LoadBaseIdx d' s1' (fromRegisterNum s2'))
        StoreIdx d s i      ->  let (cpu', (d', s')) = remap2regs d s in (cpu', StoreIdx d' s' i)
        StoreBaseIdx d s1 s2 -> let (cpu', (d', s1', s2')) = remap3regs d s1 (toRegisterNum s2) in (cpu', StoreBaseIdx d' s1' (fromRegisterNum s2'))
    
        B i -> (cpu, B i)
        BT r i ->  (cpu, BT (remapRegister r cpu) i)
        BF r i ->  (cpu, BF (remapRegister r cpu) i)
        Ret -> (cpu, Ret) 
        End -> (cpu, End) 
        Label i -> (cpu, Label i)

    where rename3regs d s1 s2 = let (cpu', d', available) = renameRegister d cpu
                                    (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                                    s1' = remapRegister s1 cpu
                                    s2' = remapRegister s2 cpu
                                in  (cpu'', (d'', s1', s2'))
          rename2regs d s     = let (cpu', d', available) = renameRegister d cpu
                                    (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                                    s' = remapRegister s cpu
                                    
                                in  (cpu'', (d'', s'))
          rename1reg  d       = let (cpu', d', available) = renameRegister d cpu
                                    (cpu'', d'') = if sameRegs instrct then (cpu, d) else (cpu', d')
                                in  (cpu'', d'' )
          remap3regs d s1 s2  = let d' = remapRegister d cpu
                                    s1' = remapRegister s1 cpu 
                                    s2' = remapRegister s2 cpu
                                in  (cpu, (d' , s1' , s2'))
          remap2regs s1 s2    = let s1' = remapRegister s1 cpu 
                                    s2' = remapRegister s2 cpu
                                in  (cpu, (s1' , s2'))
                                                                


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
        free_regs' = case instruction of 
            Add  d s1 s2        -> Map.insert d True free_regs
            AddI d s  i         -> Map.insert d True free_regs
            Sub  d s1 s2        -> Map.insert d True free_regs
            SubI d s  i         -> Map.insert d True free_regs
            Mult  d s1 s2       -> Map.insert d True free_regs
            Div  d s1 s2        -> Map.insert d True free_regs
            Or  d s1 s2         -> Map.insert d True free_regs
            Lt  d s1 s2         -> Map.insert d True free_regs
            Eq  d s1 s2         -> Map.insert d True free_regs
            Not  d s            -> Map.insert d True free_regs
            
            Move   s1 s2        -> Map.insert s1 True free_regs
            MoveI  s1 i         -> Map.insert s1 True free_regs
            LoadIdx d s i       -> Map.insert d True free_regs
            LoadBaseIdx d s1 s2 -> Map.insert d True free_regs
            StoreIdx d s i      -> free_regs
            StoreBaseIdx d s1 s2 -> free_regs
        
            B i -> free_regs 
            BT r i ->  free_regs
            BF r i ->  free_regs
            Ret -> free_regs 
            End -> free_regs
            Label i -> free_regs
    in  cpu {renamer = (renamer cpu) {freeRegisters = free_regs'}}



