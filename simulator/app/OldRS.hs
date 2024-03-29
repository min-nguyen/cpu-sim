module OldRS where

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

-- data RSEntry            = RSEntry {
--                             rs_instruction  :: Instruction,
--                             qd              :: Int,
--                             qj              :: Int,
--                             qk              :: Int,
--                             vj              :: Word32,
--                             vk              :: Word32,
--                             addr            :: Word32,
--                             busy            :: Bool
--                         } deriving Show

-- updateRS :: CPU -> CPU
-- updateRS cpu
--          = let  decoder        = decodeUnit cpu
--                 rs             = rs_station cpu
--                 entries        = rs_entries rs 
--                 statuses       = reg_statuses rs
--                 free_rsids     = getFreeRSEntries rs
--                 (entries', decoder') = trace (show $ length free_rsids) $
--                                        if (length free_rsids > 0 && length free_rsids <= 4) && not (V.null $ buffer (decodeUnit cpu))
--                                        then let fillSpace = length free_rsids
--                                                 instrcts  = V.toList $ V.take fillSpace (buffer $ decodeUnit cpu)
--                                                 -- new_rs_entries = V.toList $ V.map ((\(cyc, ent) -> (cyc, Just ent)) . createRSEntry cpu statuses ) instrcts
                                                
--                                             in  trace ("Free rs_id being used : " ++ show free_rsids ++ "\n") $ 
--                                                     ( foldl (\rsMap (key, instrct) ->  let new_entry = ((\(cyc, ent) -> (cyc, Just ent)) . createRSEntry cpu statuses ) instrct
--                                                                                        in  Map.insert key new_entry rsMap) entries (zip free_rsids instrcts), decoder { buffer = V.drop fillSpace (buffer $ decodeUnit cpu) })

--                                             -- issue occuring with instructions being executed in wrong order, due to simply using any available rs slots


--                                        else (entries, decoder)
--                 resStation = rs { rs_entries = entries' }
--                 cpu'       = cpu { rs_station = resStation, decodeUnit = decoder'}
--             in  updateRSEntries cpu'
              
                
-- updateRSEntries :: CPU -> CPU
-- updateRSEntries cpu
--             = let   rs             = rs_station cpu
--                     entries        = rs_entries rs 
--                     statuses       = reg_statuses rs
--                     updateRegStats cp  entry cycle rsid = let   resstation = rs_station cp
--                                                                 resentries = rs_entries resstation
--                                                                 regstats   = reg_statuses resstation
                                
--                                                                 (cycle, RSEntry instrct d_status s1_status s2_status s1_val s2_val addr busy) = updateRSEntry cp regstats cycle entry
--                                                                 (cp', issued)  =   if (d_status, s1_status, s2_status) == (0, 0, 0) 
--                                                                                 then let regstats' = deallocateRegStats regstats instrct rsid in issueInstruction (cp { rs_station = resstation {reg_statuses = regstats'}}) entry rsid
--                                                                                 else (cp, False)
--                                                           in  (cp', issued)
--                     cycleOrder     = map fst $ sortBy (\(k1, c1) (k2, c2) -> if c1 == c2 && k1 > k2 then GT else if c1 == c2 && k1 < k2 then LT else if c1 > c2 then GT else LT) (map (\(k, (c, x)) -> (k, c)) (Map.toList entries)  )                                          
--               in    trace ("CYCLE ORDER: " ++ show cycleOrder ++ "\n" ) $ foldr
--                             (\rsid cpu -> let maybeEntry = Map.lookup rsid (rs_entries $ rs_station cpu)
--                                           in  case maybeEntry of 
--                                                           Just (cycle, Just entry) ->    
--                                                                             let (cpu', issued)  = updateRegStats cpu entry cycle rsid  -- should only issue instruction if regstatuses are (0,0,0) !!    
--                                                                             in  cpu'
                                                          
--                                                           _ -> cpu ) cpu cycleOrder                           

-- createRSEntry :: CPU ->  RegisterStatuses -> Instruction -> (Int, RSEntry) 
-- createRSEntry cpu  statuses   instruction  = 
--     let regs             = registers cpu
--         maxCycle         = 1 + (maximum $ map (fst . snd) $ Map.toList (rs_entries $ rs_station cpu) ) :: Int
--     in                  case instruction of 
--                                     ADD  d s1 s2 -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
--                                                         [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
--                                                     in  (maxCycle, RSEntry instruction d_status s1_status s2_status  v1 v2 0 False)
--                                     ADDI s1 s2 i -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2, s2]
--                                                         [v1] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s2, s2_status)]
--                                                     in  (maxCycle, RSEntry instruction s1_status s2_status s2_status v1 i 0 False)
--                                     BEQ  s1 s2 i -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2, s2]
--                                                         [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
--                                                     in  (maxCycle, RSEntry instruction s1_status s2_status s2_status v1 v2 i False) 
--                                     LW   s1 s2 i -> let [s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2]
--                                                         [v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s2, s2_status)]
--                                                     in  (maxCycle, RSEntry instruction s1_status s2_status s2_status v2 v2 0 False) 
--                                     LI   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
--                                                     in  (maxCycle, RSEntry instruction d_status 0 0 i i 0 False )
--                                     SW   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
--                                                     in  (maxCycle, RSEntry instruction d_status 0 0 i i 0 False )
--                                     JALR d s     -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
--                                                         [addr] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]
--                                                     in  (maxCycle, RSEntry instruction d_status s_status 0 addr 0 addr False )
--                                     -- BLTZ d s i   ->

-- updateRSEntry :: CPU ->  RegisterStatuses -> Int -> RSEntry -> (Int, RSEntry) 
-- updateRSEntry cpu  statuses cycle entry  = 
--     let regs             = registers cpu
--         instruction      = rs_instruction entry
      
--     in                  case instruction of 
--                                     ADD  d s1 s2 -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
--                                                         [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
--                                                     in  (cycle, RSEntry instruction d_status s1_status s2_status  v1 v2 0 False)
--                                     ADDI s1 s2 i -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2, s2]
--                                                         [v1] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s2, s2_status)]
--                                                     in  (cycle, RSEntry instruction s1_status s2_status s2_status v1 i 0 False)
--                                     BEQ  s1 s2 i -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2, s2]
--                                                         [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
--                                                     in  (cycle, RSEntry instruction s1_status s2_status s2_status v1 v2 i False) 
--                                     LW   s1 s2 i -> let [s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2]
--                                                         [v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s2, s2_status)]
--                                                     in  (cycle, RSEntry instruction s1_status s2_status s2_status v2 v2 0 False) 
--                                     LI   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
--                                                     in  (cycle, RSEntry instruction d_status 0 0 i i 0 False )
--                                     SW   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
--                                                     in  (cycle, RSEntry instruction d_status 0 0 i i 0 False )
--                                     JALR d s     -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
--                                                         [addr] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]
--                                                     in  (cycle, RSEntry instruction d_status s_status 0 addr 0 addr False )

-- issueInstruction :: CPU -> RSEntry -> RSId -> (CPU, Bool)
-- issueInstruction cpu rs_entry rsId
--                      =  let  decoder        = (decodeUnit cpu)
--                              resstation     = rs_station cpu
--                              regstats       = reg_statuses resstation

--                              RSEntry instrct d_s s1_s s2_s _ _ _ _ = rs_entry 
                             

--                              (cpu', issueAgain) 
--                                     =       case instructionToExecutionUnit instrct of 
--                                                     IntUnit -> case instruction (intUnit1 $ executionUnits cpu) of 
--                                                                         Just _ ->   let  units = executionUnits cpu
--                                                                                          unit = intUnit2 units
--                                                                                     in   case instruction unit of Just _    -> (cpu, False)
--                                                                                                                   Nothing   -> (cpu { executionUnits = units { intUnit2 = unit { instruction = Just instrct, rs_id = rsId }}}, True )
--                                                                         Nothing ->  let units = executionUnits cpu
--                                                                                         unit = intUnit1 units
--                                                                                     in   case instruction unit of Just _    -> (cpu, False)
--                                                                                                                   Nothing   -> (cpu { executionUnits = units { intUnit1 = unit { instruction = Just instrct, rs_id = rsId }}}, True )
--                                                     MemUnit ->      let units = executionUnits cpu
--                                                                         unit  = memUnit units
--                                                                     in  case instruction unit of Just _ ->  (cpu, False)
--                                                                                                  Nothing -> (cpu { executionUnits = units { memUnit = unit { instruction = Just instrct, rs_id = rsId }}}, True )
--                                                     BranchUnit ->   let units = executionUnits cpu
--                                                                         unit  = branchUnit units
--                                                                     in  case instruction unit of Just _ ->  (cpu, False)
--                                                                                                  Nothing -> (cpu { executionUnits = units { branchUnit = unit { instruction = Just instrct, rs_id = rsId }}}, True )
--                         in (cpu', issueAgain)


-- getFreeRSEntries :: ReservationStation -> [Int]
-- getFreeRSEntries rsstation = let rsentries = rs_entries rsstation
--                              in  map fst $ filter (\(k,(cycle, x)) -> case x of Nothing -> True
--                                                                                 Just _  -> False) (Map.toList rsentries)