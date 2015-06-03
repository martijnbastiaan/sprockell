{-# LANGUAGE RecordWildCards #-}
module System where

import Control.Monad
import System.IO
import System.Random
import Data.Maybe
import Data.Bits
import Data.Char
import Debug.Trace
import Components
import TypesEtc
import Sprockell
import PseudoRandom

-- Constants
bufferSize  = 2 -- bufferSize >  0
memorySize  = 6 -- memorySize >= 0

-- ===========================================================================================
-- IO Devices
-- ===========================================================================================
type IODevice = SharedMem -> SprockellOut -> IO (SharedMem, Reply)

memDevice :: IODevice
memDevice mem (addr, ReadReq)        = return (mem, Just $ load addr mem)
memDevice mem (addr, WriteReq value) = return (store addr value mem, Nothing)
memDevice mem (addr, TestReq)        = return (store addr test mem, Just test)
    where test = intBool $ testBit (load addr mem) 0

stdDevice :: IODevice
stdDevice mem (_, WriteReq value) = putChar (chr value) >> return (mem, Nothing)
stdDevice _   (a, TestReq) = error ("TestAndSet not supported on address: " ++ show a)
stdDevice mem (_, ReadReq) = fmap ((,) mem . Just) $ do
    rdy <- hReady stdin
    if rdy
        then fmap ord getChar        
        else return (-1)

-- ===========================================================================================
-- ===========================================================================================
withDevice :: Int -> IODevice
withDevice addr | addr <= 0xFFFFFF = memDevice
                | otherwise        = stdDevice


labelRequests :: [Maybe SprockellOut] -> [Maybe (SprockellID, SprockellOut)]
labelRequests = zipWith (\i -> fmap ((,) i)) [0..]

processRequest :: Maybe (SprockellID, SprockellOut) -> SharedMem -> IO (SharedMem, (SprockellID, Maybe Value))
processRequest Nothing           mem = return (mem, (0, Nothing))
processRequest (Just (spr, out)) mem = fmap (fmap ((,) spr)) $ withDevice (fst out) mem out

system :: SystemState -> IO SystemState
system SysState{..} = do 
        let (r,rngState')     = randomInt rngState
        let newToQueue        = catMaybes $ labelRequests $ map peek buffersS2M
        let (queue', xreq)    = deQueue $ catQueue queue $ shuffle r newToQueue
        (mem', (sid, reply)) <- processRequest xreq sharedMem
        let replies           = map (\i -> if i == sid then reply else Nothing) [0..]
        let (sprs', sprOutps) = unzip $ zipWith (sprockell instrs) sprs $ map peek buffersM2S

        -- Update delay queues
        let buffersM2S'       = zipWith (<+) buffersM2S replies
        let buffersS2M'       = zipWith (<+) buffersS2M sprOutps

        return (SysState instrs sprs' buffersS2M' buffersM2S' queue' mem' rngState')

-- ===========================================================================================
-- ===========================================================================================
-- "Simulates" sprockells by recursively calling them over and over again
simulate :: (SystemState -> String) -> SystemState -> IO SystemState
simulate debugFunc sysState
    | all halted (sprs sysState) = return sysState
    | otherwise = do
        sysState' <- system sysState
        putStr (debugFunc sysState')
        simulate debugFunc sysState'

-- ===========================================================================================
-- ===========================================================================================
-- Initialise SystemState for N sprockells
initSystemState :: Int -> [Instruction] -> Seed -> SystemState
initSystemState n is seed = SysState
        { instrs     = initLookupTable "InstructionMemory" is
        , sprs       = map initSprockell [0..n]
        , buffersS2M = replicate n (initBuffer bufferSize Nothing) 
        , buffersM2S = replicate n (initBuffer bufferSize Nothing)
        , queue      = initFifo
        , sharedMem  = initMemory
        , rngState   = dec2bin seed
        }
 
pickSeed :: IO (Int)
pickSeed = getStdRandom $ randomR (0, maxBound)

run :: Int -> [Instruction] -> IO SystemState
run n instrs = runDebug (const "") n instrs 

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
