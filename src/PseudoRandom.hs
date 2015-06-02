module PseudoRandom where

import Data.List
import Data.Bits
import TypesEtc

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

dec2bin x = [if testBit x n then 1 else 0 | n <- [width,width-1..0]]

-- logarithm of base 2
log2 x = log x / log 2

-- linear feedback shift register ==> pseudo random numbers
-- rs: registers
-- is: indexes (from right to left) chosen for feedback
lfsr  key rs = y +>> rs
    where
      y   = foldr1 xor (rs!!!key)


-- reasonable choices. Repetition factor in randoms: 254
width = 16
seed0 = [0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,0] :: RngState
key0  = [1,7,11,13]

-- Given a (random) number, shuffle a list
shuffle :: Int -> [a] -> [a]
shuffle _ [] = []
shuffle n xs = el : shuffle n (left ++ right)
    where
        chosenIndex        = n `mod` (length xs)
        (left, el, right) = slice chosenIndex xs 

updateRandom = lfsr key0

randomInt :: RngState -> (Int, RngState)
randomInt g = (bin2dec g, updateRandom g)
