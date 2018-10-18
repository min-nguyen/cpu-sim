module Main where

import Lib
import Data.Word
import Data.Bits
import Utils
import Execution
import Decode 
import System.Environment
import Assembler
import Fetch 
import Control.Applicative
import qualified Data.Vector as V


updateCPU :: CPU -> CPU 
updateCPU cpu = 
    let cpu1 = updateExec cpu   
        cpu2 = updateDecode cpu1
        cpu3 = updateFetch cpu2    
      

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
main = do 
    filename <- getArgs
    print filename
    instructions <- parseFile (head filename)
    print instructions
    let cpu = initCPU instructions 
    let cpu' = foldr (\x a -> x a) cpu (replicate 20 updateCPU) 
    print cpu'