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
    then trace (show $ pc cpu) $ cpu { fetchUnit = tick (fetchUnit cpu) (decodeUnit cpu) } 
    else
        let fUnit = case status (decodeUnit cpu) of 
                    Ready ->  (fetchUnit cpu) { buffer = Just (i_memory cpu V.! (fromIntegral $ pc cpu)),
                                                cycles = 1, status = Stalled }
                            
                    Stalled -> fetchUnit cpu 
        in  cpu { fetchUnit = tick fUnit (decodeUnit cpu) } 
