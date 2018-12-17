module Execution where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative hiding (Const)
import qualified Data.Vector as V
import Debug.Trace
import ReservationStation
import Data.Ord
import Data.List 
import Renamer
import Control.Lens hiding (Const)

updateExec :: CPU -> CPU
updateExec cpu = let decoder        = (decodeUnit cpu)
       
                 in  updateExecUnits cpu


updateExecUnits :: CPU -> CPU 
updateExecUnits cpu = 
    let Units intunit1 intunit2 memunit branchunit = executionUnits cpu 
        -- need to execute in cycle order
        
        performExec cpuArg unitArg = case instruction unitArg of
            Nothing -> cpuArg
            Just instructionAndPc -> 
                        if (cycles unitArg) == 1
                        then    let instrct = fst instructionAndPc

                                    robEntry = euToROB $ execInstruction cpuArg instructionAndPc
                                    rsId      = rs_id unitArg  
                                    rsCycle   = rs_cycle unitArg
                                    cpu'      = cpu {rob = (insertReorderBuffer rsCycle robEntry (rob cpu))}
                                    
                                    rsentries = rs_entries $ rs_station cpu'
                                    regstats  = reg_statuses $ rs_station cpu'

                                    unit' = unitArg { instruction = Nothing, rs_id = 0 }

                                    rsentries' = allocateRSEntry rsentries rsId
                                    regstats' =   allocateRegStats regstats instrct --
                                    rsStation' = (rs_station cpu') { reg_statuses = regstats', rs_entries = rsentries' }
                                    
                                    cpu'' =  cpu' { rs_station = rsStation'}
                                    cpu''' = case unitId unit' of   Int_Unit1 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit1 =  unit' {cycles = (cycles unit') - 1 }}}  
                                                                    Int_Unit2 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit2 = unit' {cycles = (cycles unit') - 1 }}}  
                                                                    Mem_Unit  -> cpu'' { executionUnits = (executionUnits cpu'') { memUnit = unit' {cycles = (cycles unit') - 1 }}}  
                                                                    Branch_Unit -> cpu'' { executionUnits = (executionUnits cpu'') { branchUnit = unit' {cycles = (cycles unit') - 1 }} ,
                                                                                           stats = (stats cpu) & branches_made %~ (+1)  } 
                                      
                                in cpu'''
                        else case unitId unitArg of Int_Unit1 -> cpuArg { executionUnits = (executionUnits cpuArg) { intUnit1 = unitArg {cycles = (cycles unitArg) - 1 }}}  
                                                    Int_Unit2 -> cpuArg { executionUnits = (executionUnits cpuArg) { intUnit2 = unitArg {cycles = (cycles unitArg) - 1}}}  
                                                    Mem_Unit  -> cpuArg { executionUnits = (executionUnits cpuArg) { memUnit = unitArg {cycles = (cycles unitArg) - 1}}}  
                                                    Branch_Unit -> cpuArg { executionUnits = (executionUnits cpuArg) { branchUnit = unitArg {cycles = (cycles unitArg) - 1}}} 
        units =    map snd $ sortBy (comparing fst) $ map (\unit -> (rs_cycle unit, unit)) [intunit1, intunit2, memunit, branchunit]
        
        cpu' =  foldl (\cp unit  -> performExec cp unit) cpu units  

    in  cpu' 

execInstruction :: CPU -> InstructionAndPc -> (InstructionAndPc, ExecutionResult)
-- BRANCH
execInstruction cpu (BT source_a i, pc)     
    = let regs   = registers cpu
          a =  readRegister (registers cpu) source_a
          link_reg_val = readRegister regs R14
      in case () of 
                _ | a == 1        -> ((BT source_a i, pc), Tuple (1, link_reg_val))
                _                 -> ((BT source_a i, pc), Tuple (0, link_reg_val))
execInstruction cpu (BF source_a i, pc)     
    = let regs   = registers cpu
          a =  readRegister (registers cpu) source_a
          link_reg_val = readRegister regs R14
      in case () of 
                _ | a == 0        -> ((BF source_a  i, pc), Tuple (1, link_reg_val))
                _                 -> ((BF source_a  i, pc), Tuple (0, link_reg_val))
execInstruction cpu (B i, pc) =
    let  link_reg_val = readRegister (registers cpu) R14
    in   ((B i, pc), Tuple (1, link_reg_val))
-- MOVE
execInstruction cpu (MoveI d i, pc)
    = ((MoveI d i, pc), Const i)
execInstruction cpu (Move d s, pc)
    = let regs   = registers cpu
          a      = readRegister (registers cpu) s
      in  ((Move d s, pc), Const a)
-- LOAD
execInstruction cpu (LoadIdx d s i, pc)
    = let regs   = registers cpu
          base   = readRegister (registers cpu) s
          offset = i 
          addr   = (fromIntegral $ base + offset)
          loadedWord = (d_memory cpu) V.! addr
      in  ((LoadIdx d s i, pc), Tuple (addr, loadedWord))
execInstruction cpu (LoadBaseIdx d s1 s2, pc)
    = let regs   = registers cpu
          base   = readRegister (registers cpu) s1
          r_offset = readRegister (registers cpu) s2 
          loadedWord = (d_memory cpu) V.! (fromIntegral $ base + r_offset)
      in  ((LoadBaseIdx d s1 s2, pc), Const loadedWord)    
-- STORE
execInstruction cpu (StoreIdx r b i, pc)
    = let regs   = registers cpu
          val    = readRegister (registers cpu) r
          base   = readRegister (registers cpu) b
          offset = i 
      in  ((StoreIdx r b i, pc), Tuple (val, base + offset))
execInstruction cpu (StoreBaseIdx r s1 s2, pc)
    = let regs   = registers cpu
          val    = readRegister (registers cpu) r
          base   = readRegister (registers cpu) s1
          r_offset = readRegister (registers cpu) s2
      in  ((StoreBaseIdx r s1 s2, pc), Tuple (val, fromIntegral $ base + r_offset))
-- ARITHMETIC
execInstruction cpu (Add dest source_a source_b, pc)     
    = let regs = registers cpu
          [source_a_val, source_b_val] = map (readRegister regs) [source_a, source_b] 
          val = source_a_val + source_b_val
      in  ((Add dest source_a source_b, pc), Const val)
execInstruction cpu (AddI dest source i, pc)  
    = let regs = registers cpu
          [dest_reg, source_reg] = map (readRegister regs) [dest, source] 
          val = i + source_reg
      in  ((AddI dest source i, pc), Const val)     
execInstruction cpu (Sub dest source_a source_b, pc)     
    = let regs = registers cpu
          [source_a_val, source_b_val] = map (readRegister regs) [source_a, source_b] 
          val = source_a_val - source_b_val
      in  ((Sub dest source_a source_b, pc), Const val)
execInstruction cpu (SubI dest source i, pc)  
    = let regs = registers cpu
          source_reg = readRegister regs source
          val = source_reg - i
      in  ((SubI dest source i, pc), Const val)         
execInstruction cpu (Mult dest source_a source_b, pc)     
    = let regs = registers cpu
          [source_a_val, source_b_val] = map (readRegister regs) [source_a, source_b] 
          val = source_a_val * source_b_val
      in  ((Mult dest source_a source_b, pc), Const val)
execInstruction cpu (Div dest source_a source_b, pc)     
    = let regs = registers cpu
          [source_a_val, source_b_val] = map (readRegister regs) [source_a, source_b] 
          val = fromIntegral $  floor $ fromIntegral source_a_val / fromIntegral source_b_val
      in  ((Div dest source_a source_b, pc), Const val)    
-- LOGIC
execInstruction cpu (Lt dest source_a source_b, pc)     
    = let regs = registers cpu
          val  = if (readRegister regs source_a) < (readRegister regs source_b) then 1 else 0
      in  ((Lt dest source_a source_b, pc), Const val)
execInstruction cpu (Eq dest source_a source_b, pc)     
    = let regs = registers cpu
          val  = if (readRegister regs source_a) == (readRegister regs source_b) then 1 else 0
      in  ((Eq dest source_a source_b, pc), Const val)
execInstruction cpu (Or dest source_a source_b, pc)     
    = let regs = registers cpu
          val  = (readRegister regs source_a) .|. (readRegister regs source_b) 
      in  ((Or dest source_a source_b, pc), Const val)
execInstruction cpu (And dest source_a source_b, pc)     
    = let regs = registers cpu
          val  = (readRegister regs source_a) .&. (readRegister regs source_b)
      in  ((And dest source_a source_b, pc), Const val)
execInstruction cpu (Not dest source, pc)     
    = let regs = registers cpu
          val  = if (readRegister regs source) == 0 then 1 else 0
      in  ((Not dest source, pc), Const val)
-- Subroutines
execInstruction cpu (Ret, pc)     
    = ((Ret, pc), Const 1)
execInstruction cpu (End, pc)     
    = ((End, pc), Const 1)
