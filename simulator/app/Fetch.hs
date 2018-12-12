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
          if current_pc >= V.length (i_memory cpu) || V.length buff >= 4
          then cpu
          else 
          let nextInstruction = (((i_memory cpu) V.! current_pc ), (current_pc) )
          in   trace (show $ fst nextInstruction) $
               case fst nextInstruction of 
                              B i    -> unconditionalFetch i nextInstruction
                              BT s i -> conditionalFetch i nextInstruction
                              BF s i -> conditionalFetch i nextInstruction 
                              Ret    -> let link_reg_val = r14 $ registers cpu 
                                        in unconditionalFetch link_reg_val nextInstruction
                              Label i -> noopFetch
                              PrintLn -> noopFetch 
                              Print _ -> noopFetch
                              _      -> normalFetch nextInstruction                          
          where conditionalFetch i nextInstruction = 
                         let  (current_pc', branched) = if predictBranch (branch_predictor cpu)
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
                unconditionalFetch i nextInstruction =  
                         let  current_pc' = i
                              buffer' = buff V.++ (V.fromList [nextInstruction])
                              fUnit = (fetchUnit cpu) { buffer = buffer', cycles = 1 }
                              cpu' = cpu { fetchUnit = tick fUnit , 
                                   pc  = (fromIntegral $ current_pc'),
                                   npc = (fromIntegral $ current_pc')}
                         in   cpu' 
                normalFetch nextInstruction =
                         let  current_pc' = fromIntegral (current_pc + 1) -- keep fetching until buffer full, mem empty, or branch occurs and then predict
                              buffer' = buff V.++ (V.fromList [nextInstruction])
                              fUnit = (fetchUnit cpu) { buffer = buffer', cycles = 1 }
                              cpu' = cpu { fetchUnit = tick fUnit , 
                                   pc  = (fromIntegral $ current_pc'),
                                   npc = (fromIntegral $ current_pc')}
                         in  fetch cpu' (fromIntegral $ current_pc')
                noopFetch = 
                         let  current_pc' = fromIntegral (current_pc + 1)
                              fUnit = (fetchUnit cpu) {cycles = 1 }
                              cpu' = cpu { fetchUnit = tick fUnit , 
                                   pc  = (fromIntegral $ current_pc'),
                                   npc = (fromIntegral $ current_pc')}
                         in   cpu' 
                buff = buffer (fetchUnit cpu)

                                       
