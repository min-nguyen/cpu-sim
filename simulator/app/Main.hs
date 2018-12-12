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
import Debug.Trace
import ReservationStation
import ReorderBuffer

updateCPU :: CPU -> CPU 
updateCPU cpu = 
    let   
        cpu1 = updateFetch cpu
        cpu2 = updateDecode cpu1
        cpu3 = updateRS  cpu2
        cpu4 = updateExec  cpu3
        cpu5 = updateROB cpu4
    in  cpu5

main :: IO ()
main = do 
    filename <- getArgs
    print filename
    instructions <- parseFile (head filename)
    print instructions
    let cpu = initCPU instructions 
    let cpu' = foldr (\x a -> (x a)) cpu (replicate 500 updateCPU) 
    print cpu'