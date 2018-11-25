module BranchPredictor where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import Data.Map.Strict as Map
import qualified Data.Vector as V
import Debug.Trace
import ReservationStation
import Data.Ord
import Data.List 
import Data.Maybe 

predictBranch :: BranchPredictor -> Bool
predictBranch branchPredictor =
    let branch_reg' = branch_reg branchPredictor 
        branch_table' = branch_table branchPredictor
        branch_prob = (fromMaybe 1 (Map.lookup branch_reg' branch_table' :: Maybe Int)) :: Int
    in  if (branch_prob <= 2) then False else True

updateBranchPredictor :: Bool -> BranchPredictor -> BranchPredictor
updateBranchPredictor branched branchPredictor = 
    let inc x = if x >= 4 then 4 else x + 1
        dec x = if x <= 1 then 1 else x - 1

        branch_reg' = case branch_reg branchPredictor of 
                        B00 -> if branched then B01 else B00
                        B01 -> if branched then B11 else B10 
                        B10 -> if branched then B01 else B00 
                        B11 -> if branched then B11 else B10 
        branch_table' = Map.adjust (\x -> if branched then inc x else dec x) (branch_reg branchPredictor) (branch_table branchPredictor)
        
    in  branchPredictor {branch_table = branch_table', branch_reg = branch_reg'} 
