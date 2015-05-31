{-# LANGUAGE RecordWildCards #-}
module System where

import System.IO
import Data.Bits
import Data.Char
import Debug.Trace
import Data.Maybe
import Data.List
import TypesEtc
import Sprockell
import PseudoRandom

-- Constants
bufferSize  = 2 -- bufferSize >  0
memorySize  = 6 -- memorySize >= 0
randomStart = 0 -- Change for different random behaviour


-- execS prevents executing Sprockells which have active=false set.
execS spr inp = (Sprockell ident instrs sprState', outp)
            where
                (Sprockell ident instrs sprState) = spr
                (sprState',outp) = sprockell instrs sprState inp

-- We handle exactly one request at each clock tick, so we basically need to concatenate all
-- incoming requests and pop the head. This has the unfortunate side-effect of prioritising
-- certain Sprockells. To mitigate this behaviour *and* to "randomise" the behaviour of the
-- shared memory, this function chooses a random head.
randomiseQueue :: [(Int, SprockellOut)] -> Int -> [(Int, SprockellOut)]
randomiseQueue queue seq  = spr : (left ++ right)
        where
                available = nub $ map fst queue
                chosen = available !! ((randomInts !! seq) `mod` (length available))
                chosenIndex = fst $ head $ filter (\(i, (sprNr, sprOut)) -> sprNr ==  chosen) (zip [0..] queue)
                (left, spr, right) = slice chosenIndex queue

newToQueue :: [Request] -> [(Int, SprockellOut)]
newToQueue inps = map (\(i,r) -> (i, fromJust r))  $  filter ((/=Nothing).snd)  $  zip [0..] inps


-- ===========================================================================================
-- ===========================================================================================
-- 
-- shMem is the Mealy machine modelling the shared memory:
--        - its state conists of the input queue and its actual memory
--        - it handles exactly one request at a time
--        - it has a sequence number to simulate randomness
ifReadyDo :: IO Char -> IO (Maybe Int)
ifReadyDo x = hReady stdin >>= f
   where f True  = x >>= return . (Just . ord)
         f False = return Nothing

shMem ([]   ,mem,seq) inps = return ((newToQueue inps,mem,seq+1), (replicate (length inps) Nothing))
shMem (queue,mem,seq) inps = do
          let defaultOuts  = replicate (length inps) Nothing
          let queue'       = (randomiseQueue queue seq) ++ (newToQueue inps)
          inputChar       <- ifReadyDo (hLookAhead stdin)
          (mem',outps)    <- case head queue' of
                      -- Memory
                      (i, ReadReq  a)     -> return (mem, defaultOuts <~ (i, Just (mem!!a)) )
                      (i, WriteReq a val) -> return (mem <~ (a, val), defaultOuts)
                      -- Test and set 
                      (i, TestReq  a)     -> return (mem', outs')
                        where
                           test  = tobit $ testBit (mem !! a) 0
                           mem'  = mem         <~ (a, test)
                           outs' = defaultOuts <~ (i, Just test)
                      -- Stdout
                      (i, PutIntReq v)    -> putChar (intToDigit v) >> return (mem, defaultOuts)
                      (i, PutCharReq v)   -> putChar (chr v) >> return (mem, defaultOuts)
                      (i, GetReq)         -> return (mem, defaultOuts <~ (i, Just $ fromMaybe (-1) inputChar))
          return ((tail queue',mem',seq+1), outps)


-- ===========================================================================================
-- ===========================================================================================
-- SystemState contains:
--        - a list of sprockells
--        - a list of buffers from the sprockells to shared memory
--        - a list of buffers from shared memory to the sprockells
--        - the shared memory
system :: SystemState -> IO SystemState
system (sprs, buffersS2M, buffersM2S, ShMem st) = do 
                  (shmem',replies)                  <- shMem st $ map head buffersS2M
                  let (sprs' ,sprOutps)             = unzip $ zipWith execS sprs (map head buffersM2S) 
                  let buffersS2M'                   = zipWith (<+) buffersS2M sprOutps
                  let buffersM2S'                   = zipWith (<+) buffersM2S replies
                  return (sprs', buffersS2M',buffersM2S', ShMem shmem')

-- ===========================================================================================
-- ===========================================================================================
-- Determine if sprockells have reached "EndProg". (Of course, this couldn't exist in hardware
-- but we need to somehow stop the simulation if all sprockells are 'done'.)
halted' :: Sprockell -> Bool
halted' (Sprockell _ _ SprState{..}) = halted

-- ===========================================================================================
-- ===========================================================================================
-- "Simulates" sprockells by recursively calling them over and over again
simulate :: (SystemState -> String) -> SystemState -> IO ()
simulate debugFunc sysState@(sprs, _, _, _) 
    | all halted' sprs = return ()
    | otherwise   = do
       	sysState' <- system sysState
        putStr (debugFunc sysState')
        simulate debugFunc sysState'

-- ===========================================================================================
-- ===========================================================================================
-- Initialise SystemState for N sprockells
initSystemState :: Int -> [Instruction] -> SystemState
initSystemState n instrs = (sprockells, buffer, buffer, sharedMemory)
     where
        sprockells   = [Sprockell n instrs (initstate n) | n <- [0..n]]
        buffer       = replicate n (replicate bufferSize Nothing)
        sharedMemory = ShMem ([], replicate memorySize 0 :: [Int], randomStart)
 
run :: Int -> [Instruction] -> IO ()
run = runDebug (const "")

runDebug :: (SystemState -> String) -> Int -> [Instruction] -> IO ()
runDebug debugFunc n instrs = simulate debugFunc (initSystemState n instrs)
