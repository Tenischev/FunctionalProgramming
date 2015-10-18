module Factorial where

factorial :: Integer -> Integer
factorial n = factorial' n 1

factorial' :: Integer -> Integer -> Integer
factorial' n fn
 | n == 0 = fn
 | otherwise = factorial' (n-1) (fn*n)