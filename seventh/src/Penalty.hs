module Penalty where
import Data.List

penalty :: [[Integer]] -> [(Int,Int)]
penalty m = reverse $ nextWave 0 (sort [((m !! 0) !! x, [(0, x)]) | x <- [0..length (m !! 0) - 1]]) m
-- (m !! 0) !! x - вес в клетке, [(0, x)] - путь до нее

-- первое - текущая строка, второе - список пар (длина, путь), третье - сама матрица, результат - путь
-- важно, что каждый раз второй аргумент отсортирован
nextWave :: Int -> [(Integer, [(Int, Int)])] -> [[Integer]] -> [(Int, Int)]
nextWave i front matr
    | i + 1 == length matr = snd (head front)
    | otherwise = nextWave (i + 1) newFront matr
    where
        newFront = sort (wave (i + 1) front [[]] matr)

-- первое - текущая строка, второе - список пар (длина, путь) отсортирован, третье - список, списков пар (длина, путь)
-- для следующей строки, далее сама матрица, результат - новый список пар для следующей строки
-- изначально во втором агрументе front, может быть несколько путей заканчивающихся в одной вершине
wave :: Int -> [(Integer, [(Int, Int)])] -> [[(Integer, [(Int, Int)])]] -> [[Integer]] -> [(Integer, [(Int, Int)])]
wave i front newFront matr
    | length front == 0 = concat $ newFront
    | otherwise = wave i front' newFront' matr
    where
        (cost, path) = head front -- текущий минимум по стоимости
        curJ = snd (head path) -- извлекаем j координату, таким образом (i, curJ) - клетка которую сейчас обрабатываем
        newFront' = (nextStep i curJ cost path matr):newFront -- вычисляем новые пути из текущей вершине
        front' = filter (\(c, p) -> snd (head p) /= curJ) front -- отсекаем остальные пути, которые закончились в обработанной вершине

nextStep :: Int -> Int -> Integer -> [(Int, Int)] -> [[Integer]] -> [(Integer, [(Int, Int)])]
nextStep i j cost path matr = map (\(c, p) -> (c + cost, p:path)) steps where
    up = (checkNeig i (j - 1) ((matr !! i) !! (j - 1)) matr) -- сосед "сверху"
    centr = (checkNeig i j ((matr !! i) !! j) matr) -- сосед "по центру"
    down = (checkNeig i (j + 1) ((matr !! i) !! (j + 1)) matr) -- сосед "снизу"
    steps = concat $ up:centr:down:[] -- собираем пути к соседям

-- проверяем что j ушлел за границы матрицы
checkNeig :: Int -> Int -> Integer -> [[Integer]] -> [(Integer, (Int, Int))]
checkNeig i j cost matr = if (j >= 0 && j < (length (matr !! 0))) then ([(cost, (i, j))]) else ([])