module ReservationStation where

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

updateRS :: CPU -> CPU
updateRS cpu
         = let  decoder        = decodeUnit cpu
                rs             = rs_station cpu
                entries        = rs_entries rs 
                statuses       = reg_statuses rs
                free_rsids     = getFreeRSEntries rs
                (cpu', decoder') = 
                                        if (length free_rsids > 0 && length free_rsids <= 4) && not (V.null $ buffer (decodeUnit cpu))
                                        then let fillSpace = length free_rsids
                                                 instrcts  = V.toList $ V.take fillSpace (buffer $ decodeUnit cpu)
                                                -- new_rs_entries = V.toList $ V.map ((\(cyc, ent) -> (cyc, Just ent)) . createRSEntry cpu statuses ) instrcts
                                                
                                            in 
                                                    ( foldl (\cpu (key, instrct) ->  let    rsEntries = rs_entries (rs_station cpu)
                                                                                            new_entry = ((\(cyc, ent) -> (cyc, Just ent)) . createRSEntry cpu statuses ) instrct
                                                                                            rsEntries' = Map.insert key new_entry rsEntries
                                                                                     in     cpu { rs_station = (rs_station cpu) { rs_entries = rsEntries' }}) cpu (zip free_rsids instrcts), decoder { buffer = V.drop fillSpace (buffer $ decodeUnit cpu) })

                                            -- issue occuring with instructions being executed in wrong order, due to simply using any available rs slots

                                        else (cpu, decoder)
                
                cpu''       = cpu' { decodeUnit = decoder'}
            in  updateRSEntries cpu''
                
                
updateRSEntries :: CPU -> CPU
updateRSEntries cpu
            = let   rs             = rs_station cpu
                    entries        = rs_entries rs 
                    statuses       = reg_statuses rs
                    updateRegStation cp  entry cycle rsid = let resstation = rs_station cp
                                                                resentries = rs_entries resstation
                                                                regstats   = reg_statuses resstation
                                
                                                                updatedEntry = updateRSEntry cp regstats cycle entry
                                                                RSEntry instrctAndPC d_status s1_status s2_status s1_val s2_val addr busy = updatedEntry
                                                                
                                                                instrct = fst instrctAndPC

                                                                (cp', issued)  =     if (d_status, s1_status, s2_status) == (0, 0, 0) 
                                                                                     then let   (cpi, issued) = issueInstruction cp entry rsid cycle
                                                                                          in    if issued 
                                                                                                then let    regstats' = (deallocateRegStats regstats instrct rsid d_status s1_status s2_status) :: RegisterStatuses
                                                                                                            resstation' = resstation { rs_entries = changeRSEntry resentries rsid updatedEntry, reg_statuses = regstats'}

                                                                                                            cpi' = cpi {rs_station = resstation'} 
                                                                                                     in     (cpi', True)
                                                                                                else let    resstation' = resstation { rs_entries = changeRSEntry resentries rsid updatedEntry }
                                                                                                     in     (cpi {rs_station = resstation'}, False)
                                                                                                
                                                                                     else let   resstation' = resstation { rs_entries = changeRSEntry resentries rsid updatedEntry }
                                                                                          in    (cp {rs_station = resstation'}, False)
                                                            in  (cp', issued)
                                                            
                    cycleOrder     = map fst $ sortBy (\(k1, c1) (k2, c2) -> if c1 == c2 && k1 > k2 then GT else if c1 == c2 && k1 < k2 then LT else if c1 > c2 then GT else LT) (map (\(k, (c, x)) -> (k, c)) (Map.toList entries)  )                                          
                in  foldl   (\cpu  rsid  -> let maybeEntry = Map.lookup rsid (rs_entries $ rs_station cpu)
                                            in  case maybeEntry of 
                                                            Just (cycle, Just entry) ->    
                                                                            let (cpu', issued)  = updateRegStation cpu entry cycle rsid  -- should only issue instruction if regstatuses are (0,0,0) !!    
                                                                            in  cpu'
                                                            
                                                            _ -> cpu ) cpu cycleOrder                           

createRSEntry :: CPU ->  RegisterStatuses -> InstructionAndPc -> (Int, RSEntry) 
createRSEntry cpu  statuses   instructionAndPC  = 
    let instruction = fst instructionAndPC
        regs        = registers cpu
        resentries  = rs_entries $ rs_station cpu 
        maxCycle    = 1 + (maximum $ map (fst . snd) $ Map.toList (rs_entries $ rs_station cpu) ) :: Int
        higherPriorityEntries = map (snd . snd) $ filter (\(rsId, (cyc, ent)) -> cyc < maxCycle) $ sortBy (comparing (\(rsId, (cyc, ent)) -> cyc))  (Map.toList resentries)

    in                  case instruction of 
                                    ADD  d s1 s2 -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                  
                                                    in  (maxCycle, (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False))
                                    ADDI s1 s2 i -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2, s2]
                                                        [v1] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s2, s2_status)]
                                                    in  (maxCycle, RSEntry instructionAndPC s1_status s2_status s2_status v1 i 0 False)
                                    BEQ  s1 s2 i -> let [s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status', s2_status'] = [findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                    in  (maxCycle, RSEntry instructionAndPC 0 s1_status' s2_status' v1 v2 i False) 
                                    LW   d s1 i ->  let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                        [v1] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status)]

                                                        invalidEntries = compareEntries [d, s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status]

                                                    in  (maxCycle, ( RSEntry instructionAndPC d_status' s1_status' s1_status' v1 v1 0 False) ) 
                                    LI   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
                                                        invalidEntries = compareEntries [d] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status'] = [findEntry d invalidEntries d_status]

                                                    in  (maxCycle, RSEntry instructionAndPC d_status' 0 0 i i 0 False )
                                    SW   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
                                                        invalidEntries = compareEntries [d] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status'] = [findEntry d invalidEntries d_status]
                                                    in  (maxCycle, (RSEntry instructionAndPC d_status' 0 0 i i 0 False ))
                                    JALR d s     -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                        [addr] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]
                                                    in  (maxCycle, RSEntry instructionAndPC d_status s_status 0 addr 0 addr False )
    where   foo maybeEntry  = case maybeEntry of  Nothing -> []
                                                  Just anEntry -> case fst (rs_instruction anEntry) of 
                                                                    ADD  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    ADDI d s1 i  -> zip [d, s1] [qd anEntry, qj anEntry]
                                                                    BEQ  s1 s2 i -> zip [s1, s2] [qj anEntry, qk anEntry]
                                                                    LW   d s1 i  -> zip [d, s1] [qd anEntry, qj anEntry]
                                                                    LI   d i     -> zip [d] [qd anEntry]
                                                                    SW   d i     -> zip [d] [qd anEntry]
                                                                    JALR d s     -> zip [d,s] [qd anEntry, qj anEntry]
            compareEntries entries1 existingEntries = foldr (\(regNum) entries ->   if length (filter (\existingEntry -> length (filter (\(regnum, regentry) -> regnum == regNum && (regentry == 0)) existingEntry) > 0 ) existingEntries) > 0
                                                                                    then ((regNum):entries)
                                                                                    else entries) [] entries1
            findEntry ent entries defaultStatus = case find (ent==) entries of Nothing -> defaultStatus
                                                                               Just _  -> (-1)


updateRSEntry :: CPU ->  RegisterStatuses -> Int -> RSEntry -> RSEntry
updateRSEntry cpu  statuses cycle entry  = 
    let regs             = registers cpu
        instructionAndPC      = rs_instruction entry
        instruction = fst instructionAndPC
        resentries = rs_entries $ rs_station cpu 
        -- need to check other entries with same registers have less priority, and then permanently update the rs entry in updateRSEntries
        higherPriorityEntries = map (snd . snd) $ filter (\(rsId, (cyc, ent)) -> cyc < cycle) $ sortBy (comparing (\(rsId, (cyc, ent)) -> cyc))  (Map.toList resentries)

    in  --trace ("higher priority entries : " ++ show higherPriorityEntries ++ "\n") $
                         case instruction of 
                                    ADD  d s1 s2 -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]

                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    ADDI d s1 i ->  let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                        [v1] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status)]
                                                    in  (RSEntry instructionAndPC d_status s1_status 0 v1 i 0 False)
                                    BEQ  s1 s2 i -> let [s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status', s2_status'] = [findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]
                                                    in  (RSEntry instructionAndPC 0 s1_status' s2_status' v1 v2 i False) 
                                    LW   d s1 i  -> let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                        [v1] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status)]

                                                        invalidEntries = compareEntries [d, s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status]

                                                    in  ( RSEntry instructionAndPC d_status' s1_status' s1_status' v1 v1 0 False) 
                                    LI   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]
                                                        invalidEntries = compareEntries [d] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status'] = [findEntry d invalidEntries d_status]

                                                    in  (RSEntry instructionAndPC d_status' 0 0 i i 0 False )
                                    SW   d i     -> let [d_status] = map (fromMaybe 0 . flip getRegStat statuses) [d]

                                                        invalidEntries = compareEntries [d] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status'] = [findEntry d invalidEntries d_status]

                                                    in  (RSEntry instructionAndPC d_status' 0 0 i i 0 False )
                                    JALR d s     -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                        [addr] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]
                                                    in  (RSEntry instructionAndPC d_status s_status 0 addr 0 addr False )
    where foo maybeEntry  = case maybeEntry of Nothing -> []
                                               Just anEntry -> case fst (rs_instruction anEntry) of 
                                                                    ADD  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    ADDI d s1 i  -> zip [d, s1] [qd anEntry, qj anEntry]
                                                                    BEQ  s1 s2 i -> zip [s1, s2] [qj anEntry, qk anEntry]
                                                                    LW   d s1 i  -> zip [d, s1] [qd anEntry, qj anEntry]
                                                                    LI   d i     -> zip [d] [qd anEntry]
                                                                    SW   d i     -> zip [d] [qd anEntry]
                                                                    JALR d s     -> zip [d,s] [qd anEntry, qj anEntry]
          compareEntries entries1 existingEntries = foldr (\(regNum) entries ->     if length (filter (\existingEntry -> length (filter (\(regnum, regentry) -> regnum == regNum && (regentry == 0)) existingEntry) > 0 ) existingEntries) > 0
                                                                                    then ((regNum):entries)
                                                                                    else entries) [] entries1
          findEntry ent entries defaultStatus = case find (ent==) entries of Nothing -> defaultStatus
                                                                             Just _  -> (-1)

issueInstruction :: CPU -> RSEntry -> RSId -> Int -> (CPU, Bool)
issueInstruction cpu rs_entry rsId rsCycle
                        =  let  decoder        = (decodeUnit cpu)
                                resstation     = rs_station cpu
                                regstats       = reg_statuses resstation

                                RSEntry instructionAndPC d_s s1_s s2_s _ _ _ _ = rs_entry 
                                instrct = fst instructionAndPC

                                (cpu', issueAgain) 
                                    =       case instructionToExecutionUnit instrct of 
                                                    IntUnit -> case instruction (intUnit1 $ executionUnits cpu) of 
                                                                        Just _ ->   let  units = executionUnits cpu
                                                                                         unit = intUnit2 units
                                                                                    in   case instruction unit of Just _    -> (cpu, False)
                                                                                                                  Nothing   -> (cpu { executionUnits = units { intUnit2 = unit { instruction = Just instructionAndPC, rs_id = rsId, rs_cycle = rsCycle  }}}, True )
                                                                        Nothing ->  let units = executionUnits cpu
                                                                                        unit = intUnit1 units
                                                                                    in   case instruction unit of Just _    -> (cpu, False)
                                                                                                                  Nothing   -> (cpu { executionUnits = units { intUnit1 = unit { instruction = Just instructionAndPC, rs_id = rsId, rs_cycle = rsCycle  }}}, True )
                                                    MemUnit ->      let units = executionUnits cpu
                                                                        unit  = memUnit units
                                                                    in  case instruction unit of Just _ ->  (cpu, False)
                                                                                                 Nothing -> (cpu { executionUnits = units { memUnit = unit { instruction = Just instructionAndPC, rs_id = rsId, rs_cycle = rsCycle }}}, True )
                                                    BranchUnit ->   let units = executionUnits cpu
                                                                        unit  = branchUnit units
                                                                    in  case instruction unit of Just _ ->  (cpu, False)
                                                                                                 Nothing -> (cpu { executionUnits = units { branchUnit = unit { instruction = Just instructionAndPC, rs_id = rsId, rs_cycle = rsCycle }}}, True )
                        in (cpu', issueAgain)


getFreeRSEntries :: ReservationStation -> [Int]
getFreeRSEntries rsstation = let rsentries = rs_entries rsstation
                                in  map fst $ filter (\(k,(cycle, x)) -> case x of Nothing -> True
                                                                                   Just _  -> False) (Map.toList rsentries)