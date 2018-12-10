module Fetch where

import Lib
import Data.Word
import Data.Bits
import Utils
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace
import BranchPredictor
import Data.Map.Strict as Map

-- updateFetch :: CPU -> CPU
-- updateFetch cpu = 
--     let fUnit = case status (decodeUnit cpu) of 
--                 Ready ->  (fetchUnit cpu) { buffer = Just (i_memory cpu V.! (fromIntegral $ pc cpu)),
--                                             cycles = 1, status = Stalled }
                         
--                 Stalled -> fetchUnit cpu 
--     in  cpu { fetchUnit = tick fUnit (decodeUnit cpu) } 

updateFetch :: CPU -> CPU
updateFetch cpu = let cpu' = fetch cpu current_pc
                  in cpu'
                  where current_pc = if npc cpu == pc cpu 
                                     then fromIntegral (pc cpu) 
                                     else fromIntegral $ npc cpu


fetch :: CPU -> Int -> CPU 
fetch cpu current_pc = 
        if current_pc >= V.length (i_memory cpu)
        then cpu
        else 
        let nextInstruction = (((i_memory cpu) V.! current_pc ), (current_pc) )
        in   trace (show $ fst nextInstruction) $
             case fst nextInstruction of 
                        BEQ s1 s2 i -> let (current_pc', branched) 
                                                        = if   predictBranch (branch_predictor cpu)
                                                          then (i, True)
                                                          else (fromIntegral (current_pc + 1), False) -- keep fetching until buffer full, mem empty, or branch occurs and then predict
                                           buffer' = buff V.++ (V.fromList [nextInstruction])
                                           fUnit = (fetchUnit cpu) { buffer = buffer', cycles = 1 }
                                           branchPred = (branch_predictor cpu) {predictions = Map.insert (snd nextInstruction) branched (predictions (branch_predictor cpu)) }
                                           cpu' = cpu { fetchUnit = tick fUnit , 
                                                pc  = (fromIntegral $ current_pc'),
                                                npc = (fromIntegral $ current_pc'),
                                                branch_predictor = branchPred}
                                       in  cpu' 
                        BLT s1 s2 i -> let (current_pc', branched) 
                                                        = if   predictBranch (branch_predictor cpu)
                                                          then (i, True)
                                                          else (fromIntegral (current_pc + 1), False) -- keep fetching until buffer full, mem empty, or branch occurs and then predict
                                           buffer' = buff V.++ (V.fromList [nextInstruction])
                                           fUnit = (fetchUnit cpu) { buffer = buffer', cycles = 1 }
                                           branchPred = (branch_predictor cpu) {predictions = Map.insert (snd nextInstruction) branched (predictions (branch_predictor cpu)) }
                                           cpu' = cpu { fetchUnit = tick fUnit , 
                                                pc  = (fromIntegral $ current_pc'),
                                                npc = (fromIntegral $ current_pc'),
                                                branch_predictor = branchPred}
                                       in   cpu' 
                        _           -> let current_pc' = fromIntegral (current_pc + 1) -- keep fetching until buffer full, mem empty, or branch occurs and then predict
                                           buffer' = buff V.++ (V.fromList [nextInstruction])
                                           fUnit = (fetchUnit cpu) { buffer = buffer', cycles = 1 }
                                           cpu' = cpu { fetchUnit = tick fUnit , 
                                                pc  = (fromIntegral $ current_pc'),
                                                npc = (fromIntegral $ current_pc')}
                                       in  fetch cpu' (fromIntegral $ current_pc')

        where   buff = buffer (fetchUnit cpu)

                                       
