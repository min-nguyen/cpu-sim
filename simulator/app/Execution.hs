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
import Renamer


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
                            let instrct = fst instructionAndPc
                                robEntry = euToROB $ execInstruction cpuArg instructionAndPc
                                rsId      = rs_id unitArg  
                                rsCycle   = rs_cycle unitArg
                                cpu'      = cpu {rob = (insertReorderBuffer rsCycle robEntry (rob cpu))}
                                
                                rsentries = rs_entries $ rs_station cpu'
                                regstats  = reg_statuses $ rs_station cpu'

                                unit' = unitArg { instruction = Nothing, rs_id = 0 }

                                rsentries' = allocateRSEntry rsentries rsId
                                regstats' =  trace ("Executing " ++ show instrct ++ " into reorder buffer with PC " ++ show rsCycle ++ " \n") $ allocateRegStats regstats instrct --
                                rsStation' = (rs_station cpu') { reg_statuses = regstats', rs_entries = rsentries' }
                           
                                cpu'' = updateFreeRegisters instrct $ cpu' { rs_station = rsStation'}
                                cpu''' = case unitId unit' of   Int_Unit1 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit1 = unit'}}  
                                                                Int_Unit2 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit2 = unit'}}  
                                                                Mem_Unit  -> cpu'' { executionUnits = (executionUnits cpu'') { memUnit = unit'}}  
                                                                Branch_Unit -> cpu'' { executionUnits = (executionUnits cpu'') { branchUnit = unit'}} 
                                      
                            in  cpu'''
        units =    map snd $ sortBy (comparing fst) $ map (\unit -> (rs_cycle unit, unit)) [intunit1, intunit2, memunit, branchunit]
        
        cpu' =  foldl (\cp unit  -> performExec cp unit) cpu units  

    in  cpu' 

execInstruction :: CPU -> InstructionAndPc -> (InstructionAndPc, Word32)
execInstruction cpu (ADD dest source_a source_b, pc)     
    = let regs = registers cpu
          val  = sum $ map (readRegister regs) [source_a, source_b] 
      in  ((ADD dest source_a source_b, pc), val)
execInstruction cpu (ADDI dest source i, pc)  
    = let regs = registers cpu
          [dest_reg, source_reg] = map (readRegister regs) [dest, source] 
          val = i + source_reg
      in  ((ADDI dest source i, pc), val)  
execInstruction cpu (BEQ source_a source_b i, pc)     
    = let regs   = registers cpu
          [a, b] = map (readRegister (registers cpu)) [source_a, source_b]
      in case () of 
                _ | a == b        -> ((BEQ source_a source_b i, pc), 1)
                _                 -> ((BEQ source_a source_b i, pc), 0)
execInstruction cpu (BLT source_a source_b i, pc)     
    = let regs   = registers cpu
          [a, b] = map (readRegister (registers cpu)) [source_a, source_b]
      in case () of 
                _ | a < b         -> ((BLT source_a source_b i, pc), 1)
                _                 -> ((BLT source_a source_b i, pc), 0)
execInstruction cpu (LW dest source i, pc)   
    = let loadedWord = (d_memory cpu) V.! (fromIntegral $ (readRegister (registers cpu) source))
      in  ((LW dest source i, pc), loadedWord)
execInstruction cpu (SW s i, pc)
    = ((SW s i, pc), 1)
execInstruction cpu (LI d i, pc)
    = ((LI d i, pc), i)
-- execInstruction cpu (JALR dest source) 
--     = let regs = registers cpu
--           regs' = writeRegister regs dest (pc cpu + 1)
--       in  cpu { npc = readRegister regs' source }
