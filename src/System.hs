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
execS (Sprockell ident instrs sprState) inp = (Sprockell ident instrs sprState', outp)
            where
                (sprState',outp) = sprockell instrs sprState inp

-- ===========================================================================================
-- IO Devices
-- ===========================================================================================
ifReadyDo :: IO Char -> IO (Maybe Int)
ifReadyDo x = hReady stdin >>= f
   where f True  = x >>= return . (Just . ord)
         f False = return Nothing

memDevice :: IODevice
memDevice mem (ReadReq addr)        = return (mem, Just $ mem !! addr)
memDevice mem (WriteReq addr value) = return (mem <~ (addr, value), Nothing)
memDevice mem (TestReq addr)        = return (mem <~ (addr, test),  Just test)
    where
       test  = tobit $ testBit (mem !! addr) 0

stdDevice :: IODevice
stdDevice mem (WriteReq _ value) = putChar (chr value) >> return (mem, Nothing)
stdDevice mem (ReadReq _) = do
      inputChar  <- ifReadyDo (hLookAhead stdin) 
      inputChar' <- case inputChar of
                        Nothing -> return (Just (-1))
                        Just c  -> getChar >> return (Just c)
      return (mem, inputChar')
        
-- ===========================================================================================
-- ===========================================================================================
addr :: SprockellOut -> Int
addr (ReadReq  a)   = a
addr (WriteReq a _) = a
addr (TestReq  a)   = a

mapAddress :: Int -> IODevice
mapAddress addr | addr <= 0xFF = memDevice
                | otherwise    = stdDevice

catRequests :: [(Int, Maybe SprockellOut)] -> [(Int, SprockellOut)]
catRequests [] = []
catRequests ((_, Nothing):reqs)  =          catRequests reqs
catRequests ((n, Just s):reqs)   = (n, s) : catRequests reqs

processRequest :: [(Int, SprockellOut)] -> [Int] -> IO ([Int], (Int, Maybe Int))
processRequest [] mem    = return (mem, (0, Nothing))
processRequest ((spr, out):queue) mem = do
            let ioDevice   = mapAddress (addr out)
            (mem', reply)  <- ioDevice mem out
            return (mem', (spr, reply))

system :: SystemState -> IO SystemState
system (sprs, buffersS2M, buffersM2S, queue, mem, cycle) = do 
                  let newToQueue        = shuffle cycle $ zip [0..length sprs] (map head buffersS2M)
                  let queue'            = queue ++ (catRequests $ newToQueue)
                  (mem', reply)         <- processRequest queue' mem
                  let replies           = (replicate (length sprs) Nothing) <~ reply
                  let (sprs', sprOutps) = unzip $ zipWith execS sprs (map head buffersM2S) 

                  -- Update delay queues
                  let buffersM2S'       = zipWith (<+) buffersM2S replies
                  let buffersS2M'       = zipWith (<+) buffersS2M sprOutps

                  return (sprs', buffersS2M',buffersM2S', drop 1 queue', mem', succ cycle)

-- ===========================================================================================
-- ===========================================================================================
-- Determine if sprockells have reached "EndProg". (Of course, this couldn't exist in hardware
-- but we need to somehow stop the simulation if all sprockells are 'done'.)
halted' :: Sprockell -> Bool
halted' (Sprockell _ _ SprState{..}) = halted

-- ===========================================================================================
-- ===========================================================================================
-- "Simulates" sprockells by recursively calling them over and over again
simulate :: (SystemState -> String) -> SystemState -> IO SystemState
simulate debugFunc sysState@(sprs, _, _, _, _, _) 
    | all halted' sprs = return sysState
    | otherwise   = do
       	sysState' <- system sysState
        putStr (debugFunc sysState')
        simulate debugFunc sysState'

-- ===========================================================================================
-- ===========================================================================================
-- Initialise SystemState for N sprockells
initSystemState :: Int -> [Instruction] -> SystemState
initSystemState n instrs = (sprockells, buffer, buffer, [], sharedMemory, randomStart)
     where
        sprockells   = [Sprockell n instrs (initstate n) | n <- [0..n]]
        buffer       = replicate n (replicate bufferSize Nothing)
        sharedMemory = replicate memorySize 0 :: [Int]
 
run :: Int -> [Instruction] -> IO SystemState
run = runDebug (const "")

runDebug :: (SystemState -> String) -> Int -> [Instruction] -> IO SystemState
runDebug debugFunc n instrs = simulate debugFunc (initSystemState n instrs)
