module Components where

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as IA

type Memory a = IM.IntMap a

initMemory :: Memory a
initMemory = IM.empty

load :: Num a => Int -> Memory a -> a
load i m = fromMaybe 0 (IM.lookup i m)

store :: Int -> a -> Memory a -> Memory a
store = IM.insert

type RegFile r a = M.Map r a

initRegFile :: (Ord r, Enum r, Bounded r) => a -> RegFile r a
initRegFile = M.fromList . zip [minBound..maxBound] . repeat

(#) :: Ord r => RegFile r a -> r -> a
(#) = (M.!)

(.=) :: Ord r => r -> a -> RegFile r a -> RegFile r a
(.=) = M.insert

setRegList :: Ord r => RegFile r a -> [(r,a)] -> RegFile r a
setRegList = foldl' (flip (uncurry (.=)))

data LookupTable i e = LookupTable String !(IA.Array i e)

initLookupTable :: (Num i, IA.Ix i) => String -> [e] -> LookupTable i e
initLookupTable n xs = LookupTable n $ IA.listArray (0, fromIntegral (length xs) - 1) xs

(!) :: (IA.Ix i, Show i) => LookupTable i e -> i -> e
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
