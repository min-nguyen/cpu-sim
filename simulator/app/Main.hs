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
import System.CPUTime
import Text.Printf 

updateCPU :: CPU -> Int -> (CPU, Int)
updateCPU cpu i = 
    let   
        cpu1 = updateFetch cpu
        cpu2 = updateDecode cpu1
        cpu3 = updateRS  cpu2
        cpu4 = updateExec  cpu3
        cpu5 = updateROB cpu4
    in  if  active cpu5 
        then updateCPU cpu5 (i + 1)
        else (cpu5, i)


executeProgram :: [Instruction] -> String -> String -> String -> String -> String -> IO Double 
executeProgram instructions  branch_method cache_config cache_policy rob_size pipeline_size = do 

    let cpu = initCPU instructions  branch_method cache_config cache_policy rob_size pipeline_size
    start <- getCPUTime
    let (cpu', count) = updateCPU cpu 0
    print (  cpu')
    end <- getCPUTime 
    let diff = (fromIntegral (end - start)) / (10^12)
    return diff        

main :: IO ()
main = do 
    [filename, branch_method, cache_config, cache_policy, rob_size, pipeline_size] <- getArgs
    print filename
    instructions <- parseFile ( filename)
    print instructions
    time1 <- executeProgram instructions branch_method cache_config cache_policy rob_size pipeline_size
    -- time2 <- executeProgram instructions
    -- time3 <- executeProgram instructions
    -- time4 <- executeProgram instructions
    -- time5 <- executeProgram instructions
    -- time6 <- executeProgram instructions
    -- time7 <- executeProgram instructions
    -- time8 <- executeProgram instructions
    -- time9 <- executeProgram instructions
    -- time10 <- executeProgram instructions

    -- let t = (sum [time1, time2, time3, time4, time5, time6, time7, time8, time9, time10])/10
    printf "Computation time : %0.3f sec\n" (time1 :: Double)
    -- print $ stats cpu'
    -- printf "Computation time : %0.3f sec\n" (diff :: Double)