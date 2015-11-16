module Pairs where

pairs :: [(Integer,Integer)]
pairs = concat [[(h, x - h) | h <- [2..(x `div` 2)], gcd h (x - h) == 1] | x <- [5..]]
