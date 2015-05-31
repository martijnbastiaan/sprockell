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

-- execS prevents executing Sprockells which have active=false set.
execS spr inp
       | halted spr = (spr, Nothing, Nothing)
       | otherwise  = (Sprockell ident instrs sprState', outp, powerreq)
            where
                (Sprockell ident instrs sprState) = spr
                (sprState',outp,powerreq) = sprockell instrs sprState inp

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
power' :: Sprockell -> Bool -> Int -> Sprockell
power' (Sprockell ident instrs spr) active pc = Sprockell ident instrs spr{active=active}

power :: [Sprockell] -> Maybe PowerOut -> [Sprockell]
power sprs Nothing = sprs
power sprs (Just (StartReq spr pc)) = sprs <~ (spr, power' (sprs !! spr) True pc)
power sprs (Just (StopReq spr))     = sprs <~ (spr, power' (sprs !! spr) False 0)

-- ===========================================================================================
-- ===========================================================================================
-- SystemState contains:
--        - a list of sprockells
--        - a list of buffers from the sprockells to shared memory
--        - a list of buffers from shared memory to the sprockells
--        - the shared memory

system :: SystemState -> IO SystemState
system (sprs, buffersS2M,buffersM2S,shmem) = do 
                  let (ShMem st)                    = shmem
                  (shmem',replies)                  <- shMem st $ map head' buffersS2M
                  let (sprs' ,sprOutps, powerReqs)  = unzip3 $ zipWith execS sprs (map head' buffersM2S) 
                  let sprs''                        = foldl power sprs' powerReqs
                  let buffersS2M'                   = zipWith (<+) buffersS2M sprOutps
                  let buffersM2S'                   = zipWith (<+) buffersM2S replies
                  return (sprs'', buffersS2M',buffersM2S', ShMem shmem')

-- ===========================================================================================
-- ===========================================================================================
-- Determine if sprockells have reached "EndProg"
halted :: Sprockell -> Bool
halted (Sprockell _ instrs SprState{regbank=regbank, active=active}) =
    case (instrs !! (regbank !! fromEnum PC)) of
        EndProg -> True
        _       -> not active


-- ===========================================================================================
-- ===========================================================================================
sysSim :: (SystemState -> String) -> SystemState -> IO ()
sysSim debugFunc sysState@(sprs, _, _, _) 
    | all halted sprs = return ()
    | otherwise   = do
       	sysState' <- system sysState
        putStr (debugFunc sysState')
        sysSim debugFunc sysState'
        
head'  []       = Nothing
head' (x:xs)    = x

-- ===========================================================================================
-- ===========================================================================================
spr :: [Instruction] -> Int -> Sprockell
spr instrs ident = Sprockell ident instrs (initstate ident)

run :: Int -> [Instruction] -> IO ()
run = runDebug (const "")

runDebug :: (SystemState -> String) -> Int -> [Instruction] -> IO ()
runDebug debugFunc n instrs = sysSim debugFunc (sprockells, spr2Mems0, mem2Sprs0, shMem0)
    where
        sprockells = map (spr instrs) [0..n]
        spr2Mems0  = replicate n (replicate 2 Nothing)
        mem2Sprs0  = replicate n (replicate 2 Nothing)
        mem0       = replicate 6 0 :: [Int]
        queue0     = []
        shMem0     = ShMem (queue0,mem0,0)

