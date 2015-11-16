module Diag where

-- Из условия не совсем понял как размещать элементы в списки, сделал 2 реализации
diag :: [[Integer]]
diag = diagh

-- Каждый список - это столбец матрицы
diagv :: [[Integer]]
diagv = [column (((1 + (x - 1)) * (x - 1)) `div` 2 + 1) x | x <- [1..]]

column el n = el:column (el + n + 1) (n + 1)

-- Каждый список - это строка матрицы
diagh :: [[Integer]]
diagh = [row (((1 + x) * x) `div` 2) x | x <- [1..]]

row el n = el:row (el + n) (n + 1)