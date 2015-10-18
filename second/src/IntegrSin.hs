module IntegrSin where

-- Вычисление площади трапеции
square = \a b h -> ((a + b) * h / 2)

-- Функция заданная в условии
defFunc = \x -> (x * (sin x))

integrSin :: Double -> Double
integrSin thresold = doIntegrate (integral 1) (-thresold) thresold 1

-- Интегрирование до заданной точности
doIntegrate :: Double -> Double -> Double -> Double -> Double
doIntegrate curent last thresold n
    | abs (curent - last) < thresold = curent
    | otherwise = doIntegrate (integral (n * 2)) curent thresold (n * 2)

integral :: Double -> Double
integral steps = integral' steps 0 0

integral' :: Double -> Double -> Double -> Double
integral' steps ind s
        | ind == steps = s
        | otherwise = integral' steps (ind + 1) (s + (square (defFunc (pi / steps * ind)) (defFunc (pi / steps * (ind + 1))) (pi / steps)))
