module IsNet where

type Graph = (Int, Int->Int->Integer)

isNet :: Graph -> Bool
isNet (v, f) = (length flow == 2) && (length badNodes == 0) where
    n = v - 1
    flow = filter (\(i,o) -> i /= o) [calcVert n x (flip f x) (f x) | x <- [0..n]]
    badNodes = filter (\(i,o) -> (i /= 0 && o /= 0)) flow

calcVert n v inFlow outFlow = ((sum [inFlow x | x <- [0..n]]),(sum [outFlow x | x <- [0..n]]))

--для тестирования
gg :: Int -> Int -> Integer
gg x y = case (x, y) of
    (0, 0) -> 0
    (0, 1) -> 2
    (0, 2) -> 1
    (1, 0) -> 0
    (1, 1) -> 0
    (1, 2) -> 2
    (2, 0) -> 0
    (2, 1) -> 0
    (2, 2) -> 0
