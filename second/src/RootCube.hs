module RootCube where

-- Функция заданная в условии
defFunc = \x c -> ((x * x * x) - c)

-- Производная от функции заданной в условии
derDefFunc = \x -> (2 * x * x)

rootCube :: Double -> Double
rootCube c = doRooting (defFunc 0.001 c) 0 0.001 c

doRooting :: Double -> Double -> Double -> Double -> Double
doRooting curent last x c
    | abs (curent - last) < 0.001 = x
    | otherwise = doRooting (defFunc (newx x c) c) curent (newx x c) c

-- Следующий x
newx = \x c -> x - ((defFunc x c) / (derDefFunc x))
