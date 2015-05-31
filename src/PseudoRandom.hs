module PseudoRandom where

import Data.List
import Data.Bits

-- slicing
slice' from to xs = take (to - from) (drop from xs)
slice i xs = (left, element, right)
    where
        left = slice' 0 i xs
        element = xs !! i
        right = drop (i+1) xs

-- indexing
xs !!! is = map (xs!!) is

-- shift-in
x +>> xs = x : init xs

-- binary-to-decimal
bin2dec   []   = 0
bin2dec (x:xs) = x*2^n + bin2dec xs
        where
          n = length xs

-- logarithm of base 2
log2 x = log x / log 2

-- linear feedback shift register ==> pseudo random numbers
-- rs: registers
-- is: indexes (from right to left) chosen for feedback
lfsr  key rs = y +>> rs
    where
      y   = foldr1 xor (rs!!!key)


-- reasonable choices. Repetition factor in randoms: 254
width = 29
seed0 = [0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,0]
key0  = [1,7,11,13]

-- infinite sequence of random (binary) numbers.
-- With choices for seed and key: repetition rate: 254
randoms = iterate (lfsr key0) seed0    
randomInts = map bin2dec randoms


-- Given a seed, shuffle a list
shuffle :: Int -> [a] -> [a]
shuffle _ [] = []
shuffle n xs = el : shuffle n (left ++ right)
    where
        chosenIndex        = (randomInts !! n) `mod` (length xs)
        (left, el, right) = slice chosenIndex xs 

