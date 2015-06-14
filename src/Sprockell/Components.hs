module Sprockell.Components where

-- This module contains a collection of basic hardware structures such as memories and buffers
-- Goal of this version is supporting (memory) efficient simulation (don't mind the implementation details).
-- An alternative implementation of this module could support synthesis to a FPGA (using Clash).

import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Array (Ix)
import qualified Data.Array.IArray as IA
import System.Random
import qualified Data.Char as Char

class MemoryStructure m where
    (!) :: (Ord i, Ix i, Show i) => m i a -> i -> a
    (<~) :: (Ord i, Ix i) => m i a -> (i, a) -> m i a

(<<~~) :: (Ord i, Ix i, MemoryStructure m) => m i a -> [(i, a)] -> m i a
(<<~~) = foldl' (<~)

newtype Memory a = Memory (IM.IntMap a)

initMemory :: Memory a
initMemory = Memory IM.empty

(!!!) :: Num a => Memory a -> Int32 -> a
(Memory m) !!! i = fromMaybe 0 (IM.lookup (fromIntegral i) m)

(<~=) :: Memory a -> (Int32, a) -> Memory a
(Memory m) <~= (i,x) = Memory (IM.insert (fromIntegral i) x m)

newtype RegFile r a = RegFile (M.Map r a)

initRegFile :: (Ord r, Enum r, Bounded r) => a -> RegFile r a
initRegFile = RegFile . M.fromList . zip [minBound..maxBound] . repeat

instance MemoryStructure RegFile where
    (RegFile r) ! i = r M.! i
    (RegFile r) <~ (i,x) = RegFile (M.insert i x r)

data LookupTable i e = LookupTable String !(IA.Array i e)

initLookupTable :: (Num i, IA.Ix i) => String -> [e] -> LookupTable i e
initLookupTable n xs = LookupTable n $ IA.listArray (0, fromIntegral (length xs) - 1) xs

instance MemoryStructure LookupTable where
    (<~) = error "read only lookup table"
    (LookupTable n xs) ! i 
        | i < lo || i > hi = error ("index " ++ show i ++ " out of bounds for " ++ n) 
        | otherwise = xs IA.! i
        where (lo, hi) = IA.bounds xs

newtype Buffer a = Buffer [a]

initBuffer :: Int -> a -> Buffer a
initBuffer n x = Buffer (replicate n x)

(<+) :: Buffer a -> a -> Buffer a
(Buffer xs) <+ x = Buffer (drop 1 xs ++ [x])

peek :: Buffer a -> a
peek (Buffer (x:xs)) = x

newtype Fifo a = Fifo [a]

initFifo :: Fifo a
initFifo = Fifo []

enQueue :: a -> Fifo a -> Fifo a
enQueue x (Fifo xs) = Fifo (xs ++ [x])

catQueue :: Fifo a -> [a] -> Fifo a
catQueue (Fifo xs) ys = Fifo (xs ++ ys)

deQueue :: Fifo a -> (Fifo a, Maybe a)
deQueue (Fifo []    ) = (Fifo [], Nothing)
deQueue (Fifo (x:xs)) = (Fifo xs, Just x)

type Seed = Int
newtype RngState = RngState StdGen

initRng :: Seed -> RngState
initRng seed = RngState (mkStdGen seed)

nextRandom :: Random a => RngState -> (a, RngState)
nextRandom (RngState rs) = fmap RngState $ random rs

pickSeed :: IO Seed
pickSeed = getStdRandom $ randomR (0, maxBound)


ord :: Integral i => Char -> i
ord = fromIntegral . Char.ord
chr :: Integral i => i -> Char
chr = Char.chr . fromIntegral
