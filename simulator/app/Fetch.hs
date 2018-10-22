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
        if (fromIntegral $ pc cpu) >= length (i_memory cpu)
        then cpu { fetchUnit = tick (fetchUnit cpu) } 
        else case (instruction (fetchUnit cpu)) of 
                        (Nothing)  -> trace "Fetching new instruction" $   
                                    let fUnit = (fetchUnit cpu) { instruction = Just (i_memory cpu V.! (fromIntegral $ pc cpu)), 
                                                                cycles = 1 }
                                        idOrFlush = if npc cpu == pc cpu + 1 then id else flushPipeline

                                    in  idOrFlush $ cpu { fetchUnit = tick fUnit , pc = npc cpu, npc = npc cpu + 1 } 
                                
                        (Just instrct) -> trace "FetchUnit buffer full" $ cpu { fetchUnit = tick (fetchUnit cpu)  } 

