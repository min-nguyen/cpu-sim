module Execution where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace
import ReservationStation
import Data.Ord
import Data.List 
import ReorderBuffer 

updateExec :: CPU -> CPU
updateExec cpu = let decoder        = (decodeUnit cpu)
       
                 in  updateExecUnits cpu


updateExecUnits :: CPU -> CPU 
updateExecUnits cpu = 
    let Units intunit1 intunit2 memunit branchunit = executionUnits cpu 
        -- need to execute in cycle order
        performExec cpuArg unitArg = case instruction unitArg of
            Nothing -> cpuArg
            Just instrct -> let robEntry = euToROB $ execInstruction cpuArg instrct
                                rsId      = rs_id unitArg  
                                rsCycle   = rs_cycle unitArg
                                cpu'      = cpu {rob = (insertReorderBuffer rsCycle robEntry (rob cpu))}
                                
                                rsentries = rs_entries $ rs_station cpu'
                                regstats  = reg_statuses $ rs_station cpu'

                                unit' = unitArg { instruction = Nothing, rs_id = 0 }

                                rsentries' = allocateRSEntry rsentries rsId
                                regstats' =  trace ("Executing " ++ show instrct ++ "\n") $ allocateRegStats regstats instrct --
                                rsStation' = (rs_station cpu') { reg_statuses = regstats', rs_entries = rsentries' }

                                cpu'' = cpu' { rs_station = rsStation'}
                                cpu''' = case unitId unit' of   Int_Unit1 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit1 = unit'}}  
                                                                Int_Unit2 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit2 = unit'}}  
                                                                Mem_Unit  -> cpu'' { executionUnits = (executionUnits cpu'') { memUnit = unit'}}  
                                                                Branch_Unit -> cpu'' { executionUnits = (executionUnits cpu'') { branchUnit = unit'}} 
                                      
                            in  cpu'''
        units =    map snd $ sortBy (comparing fst) $ map (\unit -> (rs_cycle unit, unit)) [intunit1, intunit2, memunit, branchunit]
        
        cpu' =  foldl (\cp unit  -> performExec cp unit) cpu units  

    in  cpu' 

execInstruction :: CPU -> Instruction -> (Instruction, Word32)
execInstruction cpu (ADD dest source_a source_b)     
    = let regs = registers cpu
          val  = sum $ map (readRegister regs) [source_a, source_b] 
      in  (ADD dest source_a source_b, val)
execInstruction cpu (ADDI dest source i)  
    = let regs = registers cpu
          [dest_reg, source_reg] = map (readRegister regs) [dest, source] 
          val = i + source_reg
      in  (ADDI dest source i, val)  
execInstruction cpu (BEQ source_a source_b i)     
    = let regs   = registers cpu
          [a, b] = map (readRegister (registers cpu)) [source_a, source_b]
      in case () of 
                _ | a == b        -> (BEQ source_a source_b i, 1)
                _                 -> (BEQ source_a source_b i, 0)
execInstruction cpu (LW dest source i)   
    = let loadedWord = (d_memory cpu) V.! (fromIntegral $ (readRegister (registers cpu) source))
      in  (LW dest source i, loadedWord)
execInstruction cpu (SW s i)
    = (SW s i, 1)
execInstruction cpu (LI d i)
    = (LI d i, i)
-- execInstruction cpu (JALR dest source) 
--     = let regs = registers cpu
--           regs' = writeRegister regs dest (pc cpu + 1)
--       in  cpu { npc = readRegister regs' source }
