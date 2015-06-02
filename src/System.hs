{-# LANGUAGE RecordWildCards #-}
module System where

import Control.Monad
import System.IO
import System.Random
import Data.Bits
import Data.Char
import Debug.Trace
import TypesEtc
import Sprockell
import PseudoRandom

-- Constants
bufferSize  = 2 -- bufferSize >  0
memorySize  = 6 -- memorySize >= 0


-- ===========================================================================================
-- IO Devices
-- ===========================================================================================
--              Shared memory    Request         Memory, reply to sprockell
type IODevice = Memory        -> SprockellOut -> IO (Memory, Reply)

memDevice :: IODevice
memDevice mem (addr, ReadReq)        = return (mem, Just $ mem !! addr)
memDevice mem (addr, WriteReq value) = return (mem <~ (addr, value), Nothing)
memDevice mem (addr, TestReq)        = return (mem <~ (addr, test),  Just test)
    where
       test  = tobit $ testBit (mem !! addr) 0

stdDevice :: IODevice
stdDevice mem (_, WriteReq value) = putChar (chr value) >> return (mem, Nothing)
stdDevice _   (a, TestReq) = error ("TestAndSet not supported on address: " ++ show a)
stdDevice mem (_, ReadReq) = fmap ((,) mem) $ do
    rdy <- hReady stdin
    if rdy
        then fmap (Just . ord) getChar        
        else return (Just (-1))
        
-- ===========================================================================================
-- ===========================================================================================
mapAddress :: Int -> IODevice
mapAddress addr | addr <= 0xFFFFFF = memDevice
                | otherwise        = stdDevice

catRequests :: [(SprockellID, Maybe SprockellOut)] -> [(SprockellID, SprockellOut)]
catRequests [] = []
catRequests ((_, Nothing):reqs)  =          catRequests reqs
catRequests ((n, Just s):reqs)   = (n, s) : catRequests reqs

processRequest :: [(SprockellID, SprockellOut)] -> [Value] -> IO ([Value], (Int, Maybe Value))
processRequest []                   mem = return (mem, (0, Nothing))
processRequest ((SprID spr, out):_) mem = do
        let ioDevice   = mapAddress (fst out)
        (mem', reply)  <- ioDevice mem out
        return (mem', (spr, reply))

system :: SystemState -> IO SystemState
system SysState{..} = do 
        let (r,rngState')     = randomInt rngState
        let newToQueue        = shuffle r $ zip [0..] (map head buffersS2M)
        let queue'            = queue ++ (catRequests $ newToQueue)
        (mem', reply)         <- processRequest queue' sharedMem
        let replies           = (replicate (length sprs) Nothing) <~ reply
        let (sprs', sprOutps) = unzip $ zipWith (sprockell instrs) sprs (map head buffersM2S) 

        -- Update delay queues
        let buffersM2S'       = zipWith (<+) buffersM2S replies
        let buffersS2M'       = zipWith (<+) buffersS2M sprOutps

        return (SysState instrs sprs' buffersS2M' buffersM2S' (drop 1 queue') mem' (succ cycleCount) rngState')

xs <+ x = drop 1 xs ++ [x]

-- ===========================================================================================
-- ===========================================================================================
-- "Simulates" sprockells by recursively calling them over and over again
simulate :: (SystemState -> String) -> SystemState -> IO SystemState
simulate debugFunc sysState
    | all halted (sprs sysState) = return sysState
    | otherwise   = do
        sysState' <- system sysState
        putStr (debugFunc sysState')
        simulate debugFunc sysState'

-- ===========================================================================================
-- ===========================================================================================
-- Initialise SystemState for N sprockells
initSystemState :: Int -> [Instruction] -> Seed -> SystemState
initSystemState n is seed = SysState
        { instrs     = padWithErrors is
        , sprs       = map initSprockell [0..n]
        , buffersS2M = replicate n (replicate bufferSize Nothing)
        , buffersM2S = replicate n (replicate bufferSize Nothing)
        , queue      = []
        , sharedMem  = replicate memorySize 0
        , cycleCount = 0
        , rngState   = dec2bin seed
        }
    where
        padWithErrors is = is ++ [error ("trying to execute from undefined address: " ++ show (length is + n))| n <- [0..]]
 
pickSeed :: IO (Int)
pickSeed = getStdRandom random

run :: Int -> [Instruction] -> IO SystemState
run n instrs = do
    seed <- pickSeed
    runWithSeed seed n instrs

runDebug :: (SystemState -> String) -> Int -> [Instruction] -> IO SystemState
runDebug debugFunc n instrs = do
    seed <- pickSeed
    runDebugWithSeed seed debugFunc n instrs


runWithSeed :: Seed -> Int -> [Instruction] -> IO SystemState
runWithSeed seed = runDebugWithSeed seed (const "")

runDebugWithSeed :: Seed -> (SystemState -> String) -> Int -> [Instruction] -> IO SystemState
runDebugWithSeed seed debugFunc n instrs = printSeed >> simulate debugFunc (initSystemState n instrs seed)
    where
        printSeed = hPutStrLn stderr $ "Starting with random seed: " ++ show seed
