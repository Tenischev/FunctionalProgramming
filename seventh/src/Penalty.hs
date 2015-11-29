module Penalty where
import Data.List

penalty :: [[Integer]] -> [(Int,Int)]
penalty m = reverse $ nextWave 0 (sort [((m !! 0) !! x, [(0, x)]) | x <- [0..length (m !! 0) - 1]]) m

nextWave :: Int -> [(Integer, [(Int, Int)])] -> [[Integer]] -> [(Int, Int)]
nextWave i front matr
    | i + 1 == length matr = snd (head front)
    | otherwise = nextWave (i + 1) newFront matr
    where
        newFront = sort (wave (i + 1) front [[]] matr)

wave :: Int -> [(Integer, [(Int, Int)])] -> [[(Integer, [(Int, Int)])]] -> [[Integer]] -> [(Integer, [(Int, Int)])]
wave i front newFront matr
    | length front == 0 = concat $ newFront
    | otherwise = wave i front' newFront' matr
    where
        (cost, path) = head front
        curJ = snd (head path)
        newFront' = (nextStep i curJ cost path matr):newFront
        front' = filter (\(c, p) -> snd (head p) /= curJ) front

nextStep :: Int -> Int -> Integer -> [(Int, Int)] -> [[Integer]] -> [(Integer, [(Int, Int)])]
nextStep i j cost path matr = map (\(c, p) -> (c + cost, p:path)) steps where
    up = (checkNeig i (j - 1) ((matr !! i) !! (j - 1)) matr)
    centr = (checkNeig i j ((matr !! i) !! j) matr)
    down = (checkNeig i (j + 1) ((matr !! i) !! (j + 1)) matr)
    steps = concat $ up:centr:down:[]

checkNeig :: Int -> Int -> Integer -> [[Integer]] -> [(Integer, (Int, Int))]
checkNeig i j cost matr = if (j >= 0 && j < (length (matr !! 0))) then ([(cost, (i, j))]) else ([])