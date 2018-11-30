module Renamer where

import Lib
import Data.Word
import Data.Maybe
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace
import qualified Data.Map.Strict as Map
import Utils
import Data.List
import Data.Ord

getFreeRegister :: CPU -> Maybe RegisterNum
getFreeRegister cpu = case available_regs of [] -> Nothing 
                                             xs -> Just $ head xs
    where rename_table = renameTable (renamer cpu)  
          available_regs = filter (\r -> not (r `elem` Map.keys rename_table)) allRegNums



renameRegister :: RegisterNum -> CPU -> (CPU, RegisterNum, Bool)
renameRegister reg_num cpu = 
    case Map.lookup reg_num rename_table of 
        Nothing -> (cpu, reg_num, True)
        Just r  -> case getFreeRegister cpu of 
            Nothing -> (cpu, reg_num, False) 
            Just r' -> let rename_table' = Map.insert reg_num r' rename_table
                       in (cpu {renamer = (renamer cpu) {renameTable = rename_table'}}, reg_num, True)
    where rename_table = renameTable (renamer cpu)   

remapRegister :: RegisterNum -> CPU -> RegisterNum
remapRegister reg_num cpu = 
    case Map.lookup reg_num rename_table of 
        Nothing -> reg_num
        Just r  -> r 
    where rename_table = renameTable (renamer cpu)   