module Fetch where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace


-- updateFetch :: CPU -> CPU
-- updateFetch cpu = 
--     let fUnit = case status (decodeUnit cpu) of 
--                 Ready ->  (fetchUnit cpu) { buffer = Just (i_memory cpu V.! (fromIntegral $ pc cpu)),
--                                             cycles = 1, status = Stalled }
                         
--                 Stalled -> fetchUnit cpu 
--     in  cpu { fetchUnit = tick fUnit (decodeUnit cpu) } 

updateFetch :: CPU -> CPU
updateFetch cpu = 
        if fetchN == 0 || freeBufferSpace == 0
        then trace "FetchUnit buffer full or all instructions fetched " $ cpu { fetchUnit = tick (fetchUnit cpu) } 
        else trace "Fetching new instruction" $   
                    let fUnit = (fetchUnit cpu) {   buffer = buff V.++ (V.slice current_pc (current_pc + freeBufferSpace) (i_memory cpu)), 
                                                    cycles = 1 }
                        idOrFlush = if npc cpu == pc cpu  then id else flushPipeline

                    in  idOrFlush $ cpu { fetchUnit = tick fUnit , 
                                          pc = (fromIntegral $ current_pc + freeBufferSpace), 
                                          npc = (fromIntegral $ current_pc + freeBufferSpace) } 

        where fetchN = if freeBufferSpace > endlen && endlen >= 0 
                       then endlen
                       else freeBufferSpace
              freeBufferSpace = 4 - V.length buff
              endlen = (V.length (i_memory cpu) - current_pc)
              current_pc = (fromIntegral $ pc cpu)
              buff = buffer (fetchUnit cpu)