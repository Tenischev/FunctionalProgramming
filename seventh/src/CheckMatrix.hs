module CheckMatrix where
import Data.List(transpose, nub)
import Data.Bits((.|.))

checkMatrix :: [[Int]] -> Bool
checkMatrix m = (zeroAccesseble m) && (onesCover m)

-- Поварачиваю 4 раза матрицу, затем побитовое или по матрицам, затем суммирую значение элементов матрицы, в полностью покрытой сумма элементов равна их количеству
onesCover :: [[Int]] -> Bool
onesCover matrix = (length matrix) ^ 2 == sum (map sum (foldl1 orMatrix $ take 4 $ iterate rotate matrix))

-- Запускаюсь из угла, использую то что если у матрицы в двух углах стоят единицы, тест на покрытие единицами вернет False
zeroAccesseble :: [[Int]] -> Bool
zeroAccesseble m = ((m !! 0) !! 0 == 0 && zeroAccesseble' m [(0,0)]) || ((m' !! 0) !! 0 == 0 && zeroAccesseble' m' [(0,0)]) where
    m' = rotate m

zeroAccesseble' :: [[Int]] -> [(Int, Int)] -> Bool
zeroAccesseble' m front = zeroAccesseble'' m [] front
zeroAccesseble'' m oldFront front
    | oldFront == front = (((length m) ^ 2) `div` 4) * 3 == length front
    | otherwise = zeroAccesseble'' m front $ nub (concat $ map (getNeighbor m) front)

getNeighbor :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNeighbor m (x,y) = (x,y):(concat $ (checkElement m (x - 1, y)):(checkElement m (x, y - 1)):(checkElement m (x + 1, y)):(checkElement m (x, y + 1)):[[]])

checkElement m (x,y) = if (x >= 0 && y >= 0 && x < length m && y < length m && (m !! x) !! y == 0) then ([(x,y)]) else ([])

orMatrix  :: [[Int]] -> [[Int]] -> [[Int]]
orMatrix = zipWith (zipWith (.|.))

rotate :: [[Int]] -> [[Int]]
rotate = map reverse . transpose

