{-# LANGUAGE RecordWildCards #-}
module System where

import Control.Monad
import System.IO
import Data.Bits
import Data.Char
import Debug.Trace
import Components
import TypesEtc
import Sprockell
import PseudoRandom

-- Constants
bufferSize  = 2 -- bufferSize >  0
randomStart = 0 -- Change for different random behaviour

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

catRequests :: [(SprockellID, Maybe SprockellOut)] -> [(SprockellID, SprockellOut)]
catRequests [] = []
catRequests ((_, Nothing):reqs)  =          catRequests reqs
catRequests ((n, Just s):reqs)   = (n, s) : catRequests reqs

processRequest :: Maybe (SprockellID, SprockellOut) -> SharedMem -> IO (SharedMem, (Int, Maybe Value))
processRequest Nothing                  mem = return (mem, (0, Nothing))
processRequest (Just  (SprID spr, out)) mem = fmap (fmap ((,) spr)) $ withDevice (fst out) mem out

system :: SystemState -> IO SystemState
system SysState{..} = do 
        let newToQueue        = shuffle cycleCount $ zip [0..] $ map peek buffersS2M
        let (queue', xreq)    = deQueue $ catQueue queue $ catRequests newToQueue
        (mem', (sid, reply)) <- processRequest xreq sharedMem
        let replies           = map (\i -> if i == sid then reply else Nothing) [0..]
        let (sprs', sprOutps) = unzip $ zipWith (sprockell instrs) sprs $ map peek buffersM2S
        let buffersM2S'       = zipWith (<+) buffersM2S replies
        let buffersS2M'       = zipWith (<+) buffersS2M sprOutps
        return (SysState instrs sprs' buffersS2M' buffersM2S' queue' mem' (succ cycleCount))

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
initSystem :: Int -> [Instruction] -> SystemState
initSystem n is = SysState
        { instrs     = initLookupTable is
        , sprs       = map initSprockell [0..n]
        , buffersS2M = replicate n (initBuffer bufferSize Nothing)
        , buffersM2S = replicate n (initBuffer bufferSize Nothing)
        , queue      = initFifo
        , sharedMem  = initMemory
        , cycleCount = randomStart
        }

run :: Int -> [Instruction] -> IO SystemState
run = runDebug (const "")

runDebug :: (SystemState -> String) -> Int -> [Instruction] -> IO SystemState
runDebug debugFunc n instrs = simulate debugFunc (initSystem n instrs)
