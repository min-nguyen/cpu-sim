module Execution where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import qualified Data.Vector as V

updateExec :: CPU -> CPU
updateExec cpu = let executor       = (executionUnit cpu)  
                     decoder        = (decodeUnit cpu)
                     cpu'           = case (instruction decoder, status decoder) of 
                                        (Just instrct, Ready) -> execInstruction cpu instrct
                                        _ -> cpu 
                 in  cpu' { executionUnit = tick (executionUnit cpu') (decodeUnit cpu') }

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
                _ | a == b        -> cpu { pc = (npc cpu), npc = (npc cpu) + shiftL i 2}
                _                 -> cpu
execInstruction cpu (LW dest source _)   
    = let loadedWord = (d_memory cpu) V.! (fromIntegral $ (readRegister (registers cpu) source))
      in  cpu {  registers = writeRegister (registers cpu) dest loadedWord} 
execInstruction cpu (J i) 
    = let pcHi4Bits = (shiftL (shiftR (pc cpu) 28) 28)
          jumpAddress = (shiftL i 2) .&. pcHi4Bits
      in  cpu { pc = npc cpu, npc = jumpAddress}
execInstruction cpu (SW s i)
    = let d_memory' = d_memory cpu V.// [(fromIntegral i, (fromIntegral $ readRegister (registers cpu) s))]
      in cpu { d_memory = d_memory' }
execInstruction cpu (LI d i)
    = let regs = registers cpu 
      in  cpu { registers = writeRegister regs d i }     