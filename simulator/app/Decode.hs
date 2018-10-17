module Decode where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import qualified Data.Vector as V



-- updateDecode :: CPU -> CPU 
-- updateDecode cpu
--     = let decoder  = decodeUnit cpu
--           executor = executionUnit cpu 
--           maybeInstruction = buffer (fetchUnit cpu)
--           dUnit = case maybeInstruction of 
--             Nothing -> decoder
--             Just encodedInstruct -> 
--                 let opcode = shiftR encodedInstruct 24 
--                 in  case status executor  of 
--                         Ready | cycles decoder == 0 ->
--                             case () of 
--                                 _ | opcode == 0 ->  decodeInstructionR encodedInstruct cpu 
--                                 _ | opcode <= 3  -> decodeInstructionJ encodedInstruct opcode cpu 
--                                 _ | opcode >= 4  -> decodeInstructionI encodedInstruct opcode cpu 
--                         _ -> decoder 
--       in cpu { decodeUnit = tick dUnit executor }


-- decodeFunct :: Word32 -> RegisterNum -> RegisterNum -> RegisterNum -> Assembly RegisterNum RegisterNum Word32
-- decodeFunct funct = 
--     case funct of 32 -> ADD 

-- decodeInstructionR :: Word32 -> CPU -> Unit
-- decodeInstructionR encodedInstruct cpu =
--     let funct       = decodeFunct $ shiftR (shiftL encodedInstruct 26) 26 
--         shiftAmount = shiftR (shiftL encodedInstruct 21) 27
--         rd          = toRegisterNum $ shiftR (shiftL encodedInstruct 16) 27
--         rt          = toRegisterNum $ shiftR (shiftL encodedInstruct 11) 27
--         rs          = toRegisterNum $ shiftR (shiftL encodedInstruct 6) 27
--     in  (decodeUnit cpu) {instruction = Just $ funct rd rt rs }                  

-- decodeInstructionI :: Word32 -> Word32 -> CPU -> Unit
-- decodeInstructionI encodedInstruct opcode cpu =
--     let operation   = case opcode of 4 -> BEQ 
--                                      6 -> BLTZ 
--                                      8 -> ADDI
--                                      35 -> LW 


--         rt          = toRegisterNum $ shiftR (shiftL encodedInstruct 11) 27
--         rs          = toRegisterNum $ shiftR (shiftL encodedInstruct 6) 27
--         immediate   = shiftR (shiftL encodedInstruct 16) 16
--     in  (decodeUnit cpu) {instruction = Just $ operation rt rs immediate}  


-- decodeInstructionJ :: Word32 -> Word32 -> CPU -> Unit
-- decodeInstructionJ encodedInstruct opcode cpu =
--     let operation = case opcode of 2 -> J
--         address = shiftR (shiftL encodedInstruct 6) 6
--     in  (decodeUnit cpu) {instruction = Just $ J address}