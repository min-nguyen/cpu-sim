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

updateExec :: CPU -> CPU
updateExec cpu = let decoder        = (decodeUnit cpu)
                    --  executor'      = case (instruction executor) of 
                    --                        Just instrct -> executor 
                    --                        Nothing      -> executor {instruction = instruction decoder}
                        
                                     
                 in  updateExecUnits cpu


updateExecUnits :: CPU -> CPU 
updateExecUnits cpu = 
    let Units intunit1 intunit2 memunit branchunit = executionUnits cpu 
        -- need to execute in cycle order
        performExec cpuArg unitArg = case instruction unitArg of
            Nothing -> cpuArg
            Just instrct -> let cpu' = execInstruction cpuArg instrct

                                rsentries = rs_entries $ rs_station cpu'
                                regstats  = reg_statuses $ rs_station cpu'

                                rsId      = rs_id unitArg  

                                unit' = unitArg { instruction = Nothing, rs_id = 0 }

                                rsentries' = allocateRSEntry rsentries rsId
                                regstats' =  allocateRegStats regstats instrct --trace ("Executing " ++ show instrct ++ "\n") $
                                rsStation' = (rs_station cpu') { reg_statuses = regstats', rs_entries = rsentries' }

                                cpu'' = cpu' { rs_station = rsStation'}
                                cpu''' = case unitId unit' of   Int_Unit1 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit1 = unit'}}  
                                                                Int_Unit2 -> cpu'' { executionUnits = (executionUnits cpu'') { intUnit2 = unit'}}  
                                                                Mem_Unit  -> cpu'' { executionUnits = (executionUnits cpu'') { memUnit = unit'}}  
                                                                Branch_Unit -> cpu'' { executionUnits = (executionUnits cpu'') { branchUnit = unit'}} 
                                      
                            in  cpu'''
        units =    map snd $ sortBy (comparing fst) $ map (\unit -> (rs_cycle unit, unit)) [intunit1, intunit2, memunit, branchunit]
        
        cpu' =  foldl (\cp unit  -> performExec cp unit) cpu units  

    in  cpu' -- trace ("RS ENTRIES : " ++ show (rs_entries $ rs_station cpu') ++ "\n")

execInstruction :: CPU -> Instruction -> CPU
execInstruction cpu (ADD dest source_a source_b)     
    = let regs = registers cpu
          val  = sum $ map (readRegister regs) [source_a, source_b] 
      in  cpu { registers = writeRegister regs dest val } 
execInstruction cpu (ADDI dest source i)  
    = let regs = registers cpu
          [dest_reg, source_reg] = map (readRegister regs) [dest, source] 
          val = i + source_reg
      in  cpu { registers = writeRegister regs dest val } 
execInstruction cpu (BEQ source_a source_b i)     
    = let regs   = registers cpu
          [a, b] = map (readRegister (registers cpu)) [source_a, source_b]
      in case () of 
                _ | a == b        -> cpu { npc = i }
                _                 -> cpu
execInstruction cpu (LW dest source _)   
    = let loadedWord = (d_memory cpu) V.! (fromIntegral $ (readRegister (registers cpu) source))
      in  cpu {  registers = writeRegister (registers cpu) dest loadedWord} 
execInstruction cpu (JALR dest source) 
    = let regs = registers cpu
          regs' = writeRegister regs dest (pc cpu + 1)
      in  cpu { npc = readRegister regs' source }
execInstruction cpu (SW s i)
    = let d_memory' = d_memory cpu V.// [(fromIntegral i, (fromIntegral $ readRegister (registers cpu) s))]
      in  cpu { d_memory = d_memory' }
execInstruction cpu (LI d i)
    = let regs = registers cpu 
      in  cpu { registers = writeRegister regs d i }     
-- execInstruction cpu (J i) 
--     = let pcHi4Bits = (shiftL (shiftR (pc cpu) 28) 28)
--           jumpAddress = (shiftL i 2) .&. pcHi4Bits
--       in  cpu { pc = npc cpu, npc = jumpAddress}


