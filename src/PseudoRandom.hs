module PseudoRandom where

import Data.List


-- xor: #
0 # 0 = 0
0 # 1 = 1
1 # 0 = 1
1 # 1 = 0


-- slicing
slice' from to xs = take (to - from) (drop from xs)
slice i xs = (left, element, right)
	where
		left = slice' 0 i xs
		element = xs !! i
		right = drop (i+1) xs

-- indexing
xs !!! is = map (xs!!) is

-- choosing every n'th element from xs
choose n xs = xs !!! [0,n..]

-- shift-in
x +>> xs = x : init xs

-- split list xs into sublists of length n
split n xs = as : split n bs
	where
	  (as,bs) = splitAt n xs

-- extend (numerical) list xs to length n such that all elements are different
extend n xs = sort $ adds ++ xs
	where
	  m    = length seed0
	  adds = take (n-length xs) $ [0..m-1] \\ xs



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
	  y   = foldr1 (#) (rs!!!key)


-- reasonable choices. Repetition factor in randoms: 254
width = 29
seed0 = [0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,0]
key0  = [1,7,11,13]


randoms = iterate (lfsr key0) seed0	-- infinite sequence of random (binary) numbers.
					-- With choices for seed and key: repetition rate: 254

randomInts = map bin2dec randoms


-- infinite sequence of keys so that every communication channel may have its own key for randomization
keys = choose (width+2) $ map (extend m') $ map nub $ split m' $ map bin2dec $ map (take nn) randoms
	where
	  m' = length key0
	  m  = length seed0
	  nn = round $ log2 $ fromIntegral m


