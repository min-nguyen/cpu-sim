module Main where

import Lib
import Data.Word
import Data.Bits
import Utils
import Execution
import Decode 
import Fetch 
import Control.Applicative
import qualified Data.Vector as V


updateCPU :: CPU -> CPU 
updateCPU cpu = 
    let cpu1 = updateFetch cpu         
        cpu2 = updateDecode cpu1
        cpu3 = updateExec cpu2
    in  cpu3 

-- updateUnit ::      (toSelf -> fromPrev -> fromNext -> fromMem -> (state, toPrev, toMem))
--                 -> (state -> (toSelf, toNext)) -- splitter
--                 -> state 
--                 -> (Signal fromPrev, Signal fromNext, Signal fromMem)
--                 -> (Signal toPrev,   Signal toNext,   Signal toMem) 
-- updateUnit update splitter initialState (fromPrev, fromNext, fromMem) =
--     (toPrev, toNext, toMem) where 
--         (newState, toPrev, toMem) = update  toSelf  fromPrev  fromNext fromMem 
--         (toSelf, toNext) = splitter initialState
        -- newState        = 

main :: IO ()
main = someFunc
