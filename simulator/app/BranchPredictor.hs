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

predictBranch :: BranchPredictor -> InstructionAndPc -> (BranchPredictor, Bool)
predictBranch branchPredictor instrctAndPc =
    let instrct_pc = snd instrctAndPc
        branchPredictor' = case (Map.lookup instrct_pc (branch_table branchPredictor)) of 
                                    Nothing ->  let branch_table' = Map.insert instrct_pc (Map.fromList [(B00, 1), (B01, 1), (B10, 1), (B11, 1)]) (branch_table branchPredictor)  
                                                    branch_reg'   = Map.insert instrct_pc B00 (branch_reg branchPredictor)
                                                in  branchPredictor { branch_table = branch_table', 
                                                                      branch_reg   = branch_reg'   }
                                    Just t  -> branchPredictor
        branch_history =   fromJust $ Map.lookup instrct_pc (branch_reg branchPredictor')
        branch_table'  =   fromJust $ Map.lookup instrct_pc (branch_table branchPredictor')
        branch_prob = case (Map.lookup branch_history branch_table') of 
                                    Nothing -> trace "\n somethings gone wrong \n" 1 
                                    Just p  -> p
    in  (branchPredictor', if (branch_prob <= 2) then False else True)

updateBranchPredictor :: Bool -> InstructionAndPc -> CPU -> (CPU, Bool)
updateBranchPredictor branched instrctAndPc cpu = 
    let branchPredictor = branch_predictor cpu
        instrct_pc = snd instrctAndPc
        inc x = if x >= 4 then 4 else x + 1
        dec x = if x <= 1 then 1 else x - 1

        correctBranch        = fromJust $ Map.lookup instrct_pc (predictions branchPredictor)
        
        (branch_reg', branch_history) = case Map.lookup instrct_pc (branch_reg branchPredictor) of 
                                                Just B00 -> (Map.insert instrct_pc (if branched then B01 else B00) (branch_reg branchPredictor), B00)
                                                Just B01 -> (Map.insert instrct_pc (if branched then B11 else B10) (branch_reg branchPredictor), B01)
                                                Just B10 -> (Map.insert instrct_pc (if branched then B01 else B00) (branch_reg branchPredictor), B10) 
                                                Just B11 -> (Map.insert instrct_pc (if branched then B11 else B10) (branch_reg branchPredictor), B11)
        branch_table' = case Map.lookup instrct_pc (branch_table branchPredictor) of 
                                Just t -> let t' = (Map.adjust (\x -> if branched then inc x else dec x) branch_history t)
                                          in Map.insert instrct_pc t' (branch_table branchPredictor)
        
        branchPredictor' = branchPredictor {branch_table = branch_table', branch_reg = branch_reg'} 
    in  (cpu {branch_predictor = branchPredictor'}, branched == correctBranch)


-- predictBranch :: BranchPredictor -> Bool
-- predictBranch branchPredictor =
--     let branch_reg' = branch_reg branchPredictor 
--         branch_table' = branch_table branchPredictor
--         branch_prob = (fromMaybe 1 (Map.lookup branch_reg' branch_table' :: Maybe Int)) :: Int
--     in  if (branch_prob <= 2) then False else True

-- updateBranchPredictor :: Bool -> InstructionAndPc -> CPU -> (CPU, Bool)
-- updateBranchPredictor branched instrctAndPc cpu = 
--     let branchPredictor = branch_predictor cpu
       
--         inc x = if x >= 4 then 4 else x + 1
--         dec x = if x <= 1 then 1 else x - 1

--         correctBranch        = fromJust $ Map.lookup (snd instrctAndPc) (predictions branchPredictor)
        
--         branch_reg' = case branch_reg branchPredictor of 
--                         B00 -> if branched then B01 else B00
--                         B01 -> if branched then B11 else B10 
--                         B10 -> if branched then B01 else B00 
--                         B11 -> if branched then B11 else B10 
--         branch_table' = Map.adjust (\x -> if branched then inc x else dec x) (branch_reg branchPredictor) (branch_table branchPredictor)
        
--         branchPredictor' = branchPredictor {branch_table = branch_table', branch_reg = branch_reg'} 
--     in  (cpu {branch_predictor = branchPredictor'}, branched == correctBranch)


-- Two Bit Saturing Counter
-- predictBranch :: BranchPredictor -> Int -> (BranchPredictor, Bool)
-- predictBranch branchPredictor instrct_pc =
--     let branch_prob = branch_reg branchPredictor
--         prediction = if (branch_prob <= 2) then False else True
--         branchPredictor' = case (Map.lookup instrct_pc (predictions branchPredictor)) of 
--             Nothing ->  let predictions' = Map.insert instrct_pc prediction (predictions branchPredictor)  
--                         in  branchPredictor { predictions = predictions'  }
--             Just t  -> branchPredictor
--     in  (branchPredictor', prediction)

-- updateBranchPredictor :: Bool -> InstructionAndPc -> CPU -> (CPU, Bool)
-- updateBranchPredictor branched instrctAndPc cpu = 
--     let branchPredictor = branch_predictor cpu
--         instrct_pc = snd instrctAndPc
--         inc x = if x >= 4 then 4 else x + 1
--         dec x = if x <= 1 then 1 else x - 1

--         correctBranch        = fromJust $ Map.lookup instrct_pc (predictions branchPredictor)
        
--         branch_reg' = if branched then inc (branch_reg branchPredictor) else dec (branch_reg branchPredictor)
--         branchPredictor' = branchPredictor {branch_reg = branch_reg'} 
--     in  (cpu {branch_predictor = branchPredictor'}, branched == correctBranch)

