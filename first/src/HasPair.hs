module HasPair where

hasPair :: Integer -> Bool
hasPair n
 | n == 0 = False
 | n < 0 = hasPair (n * (-1))
 | otherwise = hasPair' (div n 10) (mod n 10)

hasPair' :: Integer -> Integer -> Bool
hasPair' n cur
 | n == 0 = False
 | otherwise = hasPair'' (div n 10) (mod n 10) cur

hasPair'' :: Integer -> Integer -> Integer -> Bool
hasPair'' n cur prev
 | cur == prev = True
 | n == 0 = False
 | otherwise = hasPair'' (div n 10) (mod n 10) cur