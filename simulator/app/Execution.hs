module Execution where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace
import ReservationStation

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
            Nothing -> (cpuArg, unitArg) 
            Just instrct -> let cpu' = execInstruction cpuArg instrct

                                rsentries = rs_entries $ rs_station cpu'
                                regstats  = reg_statuses $ rs_station cpu'

                                rsId      = rs_id unitArg  

                                unit' = unitArg { instruction = Nothing, rs_id = 0 }

                                rsentries' = allocateRSEntry rsentries rsId
                                regstats' = trace ("Executing " ++ show instrct ++ "\n") $ allocateRegStats regstats instrct 
                                rsStation' = (rs_station cpu') { reg_statuses = regstats', rs_entries = rsentries' }

                                cpu'' = cpu' { rs_station = rsStation'}
                            in  (cpu'', tick unit')
        -- units =    map fst $ sortBy (comparing snd) $ map (\unit -> (rs_cycle unit, unit)) [intunit1, intunit2, memunit, branchunit]
        (cpu1, intunit1') = foldl (performExec cpu) cpu intunit1  
        (cpu2, intunit2') = performExec cpu1 intunit2
        (cpu3, memunit')  = performExec cpu2 memunit 
        (cpu4, branchunit') = performExec cpu3 branchunit

    in  trace ("RS ENTRIES : " ++ show (rs_entries $ rs_station cpu4) ++ "\n") cpu4 { executionUnits = Units intunit1' intunit2' memunit' branchunit' }

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


