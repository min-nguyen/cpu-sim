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
import Renamer
-- data RSEntry            = RSEntry {
--                             rs_instruction  :: Instruction,
--                             qd              :: Int,
--                             qj              :: Int,
--                             qk              :: Int,
--                             vj              :: Int,
--                             vk              :: Int,
--                             addr            :: Int,
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
                                                    ( foldl (\cpu (key, instrct) ->  let    rsEntries = (rs_entries (rs_station cpu))
                                                                                            (cpu_', new_entry) = ((\(cpu_, (cyc, ent)) -> (cpu_, (cyc, Just ent))) . createRSEntry cpu statuses ) instrct
                                                                                            rsEntries' = Map.insert key new_entry rsEntries
                                                                                     in     cpu_' { rs_station = (rs_station  cpu_') { rs_entries = rsEntries' }}) cpu (zip free_rsids instrcts), decoder { buffer = V.drop fillSpace (buffer $ decodeUnit  cpu') })

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
                                                            
                    cycleOrder     = map fst $ sortBy (\(k1, c1) (k2, c2) -> if c1 < c2 then LT else GT) (map (\(k, (c, x)) -> (k, c)) (Map.toList entries)  )                                          
                in  
                    foldl   (\cpu  rsid  -> let maybeEntry = Map.lookup rsid (rs_entries $ rs_station cpu)
                                            in  case maybeEntry of 
                                                            Just (cycle, Just entry) ->    
                                                                            let (cpu', issued)  = updateRegStation cpu entry cycle rsid 
                                                                            in  cpu'
                                                            
                                                            _ -> cpu ) cpu cycleOrder                           

createRSEntry :: CPU ->  RegisterStatuses -> InstructionAndPc -> (CPU, (Int, RSEntry)) 
createRSEntry cpu  statuses   instructionAndPC  = 
    let instruction = fst instructionAndPC
        regs        = registers cpu
        resentries  = rs_entries $ rs_station cpu 
        maxCycle    = 1 + (maximum $ map (fst . snd) $ Map.toList (rs_entries $ rs_station cpu) ) :: Int
        higherPriorityEntries = map (snd . snd) $ filter (\(rsId, (cyc, ent)) -> cyc < maxCycle) $ sortBy (comparing (\(rsId, (cyc, ent)) -> cyc))  (Map.toList resentries)
        
        (cpu', renamedInstruction) = (cpu, fst instructionAndPC) --  renameInstructionRegs instruction cpu --
        renamedInstructionAndPc =  instructionAndPC -- (renamedInstruction, snd instructionAndPC) --
    in              case renamedInstruction of 
                                    Add  d s1 s2 -> let 
                                                        [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]
                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Sub  d s1 s2 -> let 
                                                        [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    AddI d s i   -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                        [v] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]

                                                        invalidEntries = compareEntries [d, s] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s_status'] = [findEntry d invalidEntries d_status, findEntry s invalidEntries s_status]

                                                    in  (cpu',  (maxCycle, RSEntry renamedInstructionAndPc d_status' s_status' 0 v v 0 False))
                                    SubI d s i   -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                        [v] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]

                                                        invalidEntries = compareEntries [d, s] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s_status'] = [findEntry d invalidEntries d_status, findEntry s invalidEntries s_status]

                                                    in  (cpu',  (maxCycle, RSEntry renamedInstructionAndPc d_status' s_status' 0 v v 0 False))
                                    Mult d s1 s2 -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Div d s1 s2 ->  let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Lt d s1 s2   -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Eq  d s1 s2  -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Or  d s1 s2  -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    And d s1 s2  -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Not  d s1    -> let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [d, s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status]
                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' 0  v1 0 0 False)))
                                    Ret         -> (cpu',  (maxCycle, RSEntry renamedInstructionAndPc 0 0 0 0 0 0 False ))
                                    B   i       -> (cpu',  (maxCycle, RSEntry renamedInstructionAndPc 0 0 0 i 0 i False ))
                                    End         -> (cpu',  (maxCycle, RSEntry renamedInstructionAndPc 0 0 0 0 0 0 False ))
                                    Label i     -> (cpu',  (maxCycle, RSEntry renamedInstructionAndPc 0 0 0 i 0 i False ))
                                    BT  s1 i ->     let s1_status =  (fromMaybe 0 . flip getRegStat statuses) s1
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status'] = [findEntry s1 invalidEntries s1_status]

                                                    in  (cpu',  (maxCycle, RSEntry renamedInstructionAndPc 0 s1_status' 0 v1 0 i False) )
                                    BF  s1 i ->     let s1_status =  (fromMaybe 0 . flip getRegStat statuses) s1
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status'] = [findEntry s1 invalidEntries s1_status]

                                                    in  (cpu',  (maxCycle, RSEntry renamedInstructionAndPc 0 s1_status' 0 v1 0 i False) )
                                    LoadIdx d s i ->  let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                          [v] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]

                                                          invalidEntries = compareEntries [d, s] (map foo higherPriorityEntries) :: [RegisterNum]
                                                          [d_status', s_status'] = [findEntry d invalidEntries d_status, findEntry s invalidEntries s_status]

                                                      in  (cpu',  (maxCycle, RSEntry renamedInstructionAndPc d_status' s_status' 0 v v 0 False))
                                    LoadBaseIdx d s1 s2  -> 
                                                    let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), ( s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry  s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    StoreIdx s1 s2 i -> 
                                                    let [s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status', s2_status'] = [findEntry s1 invalidEntries s1_status, findEntry  s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc 0 s1_status' s2_status'  v1 v2 0 False)))
                                    StoreBaseIdx d s1 s2  -> 
                                                    let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), ( s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry  s2 invalidEntries s2_status]

                                                        
                                                    in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' s2_status'  v1 v2 0 False)))
                                    Move  d s1    -> let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                         v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                         invalidEntries = compareEntries [d, s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                         [d_status', s1_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status]
                                                        
                                                     in  (cpu',  (maxCycle, (RSEntry renamedInstructionAndPc d_status' s1_status' 0  v1 0 0 False)))  
                                    MoveI  s1 i ->  let s1_status =  (fromMaybe 0 . flip getRegStat statuses) s1
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status'] = [findEntry s1 invalidEntries s1_status]

                                                    in  (cpu',  (maxCycle, RSEntry renamedInstructionAndPc s1_status' 0 0 v1 0 i False) )
                                      
                                    
    where   foo maybeEntry  = case maybeEntry of  Nothing -> []
                                                  Just anEntry -> case fst (rs_instruction anEntry) of 
                                                                    Ret -> []
                                                                    Add  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    AddI d s i ->  zip [d, s] [qd anEntry, qj anEntry]
                                                                    Sub   d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Mult   d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Div  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Eq  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Lt  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Or  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    And  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    LoadBaseIdx  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    StoreBaseIdx d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    MoveI d i     -> zip [d] [qd anEntry]
                                                                    Move d s     -> zip [d,s] [qd anEntry, qj anEntry]
                                                                    LoadIdx d s i -> zip [d, s] [qd anEntry, qj anEntry]
                                                                    StoreIdx s1 s2 i -> zip [s1, s2] [qj anEntry, qk anEntry]
                                                                    Not d s  -> zip [d, s] [qd anEntry, qj anEntry]
                                                                    BT s i ->  zip [s] [qj anEntry]
                                                                    End -> []
                                                                    BF s i -> zip [s] [qj anEntry]
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
                                    Add  d s1 s2 -> let 
                                                        [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Sub  d s1 s2 -> let 
                                                        [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    AddI d s i   -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                        [v] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]

                                                        invalidEntries = compareEntries [d, s] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s_status'] = [findEntry d invalidEntries d_status, findEntry s invalidEntries s_status]

                                                    in  (RSEntry instructionAndPC d_status' s_status' 0 v v 0 False)
                                    SubI d s i   -> let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                        [v] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]

                                                        invalidEntries = compareEntries [d, s] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s_status'] = [findEntry d invalidEntries d_status, findEntry s invalidEntries s_status]

                                                    in  (RSEntry instructionAndPC d_status' s_status' 0 v v 0 False)
                                    Mult d s1 s2 -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Div d s1 s2 ->  let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Lt d s1 s2   -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Eq  d s1 s2  -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Or  d s1 s2  -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    And d s1 s2  -> let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Not  d s1    -> let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [d, s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status]
                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' 0  v1 0 0 False)
                                    
                                    B   i               -> (RSEntry instructionAndPC 0 0 0 i 0 i False )
                                    End                 -> (RSEntry instructionAndPC 0 0 0 0 0 0 False )
                                    Ret                 -> (RSEntry instructionAndPC 0 0 0 0 0 0 False )
                                    Label i             -> (RSEntry instructionAndPC 0 0 0 i 0 i False )
                                    BT  s1 i ->     let s1_status =  (fromMaybe 0 . flip getRegStat statuses) s1
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status'] = [findEntry s1 invalidEntries s1_status]

                                                    in  (RSEntry instructionAndPC 0 s1_status' 0 v1 0 i False) 
                                    BF  s1 i ->     let s1_status =  (fromMaybe 0 . flip getRegStat statuses) s1
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status'] = [findEntry s1 invalidEntries s1_status]

                                                    in  (RSEntry instructionAndPC 0 s1_status' 0 v1 0 i False) 
                                    LoadIdx d s i ->  let [d_status, s_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s]
                                                          [v] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s, s_status)]

                                                          invalidEntries = compareEntries [d, s] (map foo higherPriorityEntries) :: [RegisterNum]
                                                          [d_status', s_status'] = [findEntry d invalidEntries d_status, findEntry s invalidEntries s_status]

                                                      in  (RSEntry instructionAndPC d_status' s_status' 0 v v 0 False)
                                    LoadBaseIdx d s1 s2  -> 
                                                    let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), ( s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry  s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    StoreIdx s1 s2 i -> let [s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [s1, s2]
                                                            [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), (s2, s2_status)]
                                                            invalidEntries = compareEntries [s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                            [s1_status', s2_status'] = [findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                            
                                                        in  (RSEntry instructionAndPC 0 s1_status' s2_status'  v1 v2 0 False)
                                    StoreBaseIdx d s1 s2  -> 
                                                    let [d_status, s1_status, s2_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1, s2]
                                                        [v1, v2] = map (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) [(s1, s1_status), ( s2, s2_status)]
                                                        invalidEntries = compareEntries [d, s1, s2] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [d_status', s1_status', s2_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status, findEntry s2 invalidEntries s2_status]

                                                        
                                                    in  (RSEntry instructionAndPC d_status' s1_status' s2_status'  v1 v2 0 False)
                                    Move  d s1    -> let [d_status, s1_status] = map (fromMaybe 0 . flip getRegStat statuses) [d, s1]
                                                         v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                         invalidEntries = compareEntries [d, s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                         [d_status', s1_status'] = [findEntry d invalidEntries d_status, findEntry s1 invalidEntries s1_status]
                                                        
                                                     in  (RSEntry instructionAndPC d_status' s1_status' 0  v1 0 0 False)
                                    MoveI  s1 i ->  let s1_status =  (fromMaybe 0 . flip getRegStat statuses) s1
                                                        v1 =  (\(source, stat) -> if stat == 0 then (readRegister regs source) else 0) (s1, s1_status)
                                                        invalidEntries = compareEntries [s1] (map foo higherPriorityEntries) :: [RegisterNum]
                                                        [s1_status'] = [findEntry s1 invalidEntries s1_status]

                                                    in  (RSEntry instructionAndPC s1_status' 0 0 v1 0 i False) 
                                      
                                    
    where foo maybeEntry  = case maybeEntry of  Nothing -> []
                                                Just anEntry -> case fst (rs_instruction anEntry) of 
                                                                    Add  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Sub   d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    SubI  d s1 i -> zip [d, s1] [qd anEntry, qj anEntry]
                                                                    AddI  d s1 i -> zip [d, s1] [qd anEntry, qj anEntry]
                                                                    Mult   d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Div  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Eq  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Lt  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    Or  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    And  d s1 s2 -> zip [d,s1,s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    LoadBaseIdx  d s1 s2 -> zip [d,s1,  s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    StoreBaseIdx d s1 s2 -> zip [d,s1, s2] [qd anEntry, qj anEntry, qk anEntry]
                                                                    MoveI d i     -> zip [d] [qd anEntry]
                                                                    Move d s     -> zip [d,s] [qd anEntry, qj anEntry]
                                                                    LoadIdx d s i -> zip [d, s] [qd anEntry, qj anEntry]
                                                                    StoreIdx s1 s2 i -> zip [s1, s2] [qj anEntry, qk anEntry]
                                                                    Ret -> []
                                                                    Not d s  -> zip [d, s] [qd anEntry, qj anEntry]
                                                                    BT s i ->  zip [s] [qj anEntry]
                                                                    B i -> []
                                                                    BF s i -> zip [s] [qj anEntry]
                                                                    End -> []
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
                                                                                                                  Nothing   -> (cpu { executionUnits = units { intUnit2 = unit { instruction = Just instructionAndPC, 
                                                                                                                                                                                 rs_id = rsId, 
                                                                                                                                                                                 rs_cycle = rsCycle,
                                                                                                                                                                                 cycles = 2  }}}, True )
                                                                        Nothing ->  let units = executionUnits cpu
                                                                                        unit = intUnit1 units
                                                                                    in   case instruction unit of Just _    -> (cpu, False)
                                                                                                                  Nothing   -> (cpu { executionUnits = units { intUnit1 = unit { instruction = Just instructionAndPC, 
                                                                                                                                                                                 rs_id = rsId, 
                                                                                                                                                                                 rs_cycle = rsCycle,
                                                                                                                                                                                 cycles = 2  }}}, True )
                                                    MemUnit ->      let units = executionUnits cpu
                                                                        unit  = memUnit units
                                                                        cycs  = case instrct of LoadIdx s1 s2 i ->  let  base   = readRegister (registers cpu) s2
                                                                                                                         offset = i 
                                                                                                                    in   if Map.member (base + i) (l1_cache cpu) then 2 else 5
                                                                                                StoreIdx s1 s2 i -> let  base   = readRegister (registers cpu) s2
                                                                                                                         offset = i 
                                                                                                                    in   if Map.member (base + i) (l1_cache cpu) then 2 else 4
                                                                                                _                -> 4

                                                                    in  case instruction unit of Just _ ->  (cpu, False)
                                                                                                 Nothing -> (cpu { executionUnits = units { memUnit = unit { instruction = Just instructionAndPC, 
                                                                                                                                                             rs_id = rsId, 
                                                                                                                                                             rs_cycle = rsCycle,
                                                                                                                                                             cycles = cycs }}}, True )
                                                    BranchUnit ->   let units = executionUnits cpu
                                                                        unit  = branchUnit units
                                                                    in  case instruction unit of Just _ ->  (cpu, False)
                                                                                                 Nothing -> (cpu { executionUnits = units { branchUnit = unit { instruction = Just instructionAndPC, 
                                                                                                                                                                rs_id = rsId, 
                                                                                                                                                                rs_cycle = rsCycle,
                                                                                                                                                                cycles = 3 }}}, True )
                        in (cpu', issueAgain)


getFreeRSEntries :: ReservationStation -> [Int]
getFreeRSEntries rsstation = let rsentries = rs_entries rsstation
                                in  map fst $ filter (\(k,(cycle, x)) -> case x of Nothing -> True
                                                                                   Just _  -> False) (Map.toList rsentries)