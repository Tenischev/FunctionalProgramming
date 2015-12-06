module Operations where
import Control.Monad
import Data.List

-- среди всех возможных значений для строки "123456789" ищем то что потребовали
operations :: Integer -> Maybe String
operations val = (find (\(v, str) -> v == val) (possibleVal "123456789")) >>= Just . snd

-- возвращает список пар (значение выражения, выражение с расстановкой операций в нем)
possibleVal :: String -> [(Integer, String)]
possibleVal str@[c] = [(read str, str)]
possibleVal str = concat $ map (\(f, s) -> calc (possibleVal f) s) (map (\ind -> splitAt ind str) [1..length str - 1])
-- (map (\ind -> splitAt ind str) [1..length str - 1]) все разбиения строки на две части
-- вторую часть принимаем за число, для певрой части ищем возможные значения выражения

-- служебная функция, скалдывает и вычитает второй аргумент из всех возможных значений левой части и записывает
-- результат в той же форме возможных значений - список пар (значение выражения, выражение с расстановкой операций в нем)
calc :: [(Integer, String)] -> String -> [(Integer, String)]
calc vals s = concat $ (map (\(val, str) -> (val + (read s), str ++ '+':s)) vals):(map (\(val, str) -> (val - (read s), str ++ '-':s)) vals):[]
