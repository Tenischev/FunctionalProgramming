module Operations where
import Control.Monad
import Data.List

operations :: Integer -> Maybe String
operations val = (find (\(v, str) -> v == val) (possibleVal "123456789")) >>= Just . snd

possibleVal :: String -> [(Integer, String)]
possibleVal str@[c] = [(read str, str)]
possibleVal str = concat $ map (\(f, s) -> calc (possibleVal f) s) (map (\ind -> splitAt ind str) [1..length str - 1])

calc :: [(Integer, String)] -> String -> [(Integer, String)]
calc vals s = concat $ (map (\(val, str) -> (val + (read s), str ++ '+':s)) vals):(map (\(val, str) -> (val - (read s), str ++ '-':s)) vals):[]


-- (map (\ind -> splitAt ind str) [1..length str]) все разбиения на две строки