module MaxNumber where
import Data.Char(isDigit)

maxNumber :: String -> Integer
maxNumber = maximum . strToInt where
    strToInt :: String -> [Integer]
    strToInt str = map (read) $ guard $ wordsWhen (not . isDigit) str where
        guard :: [String] -> [String]
        guard a = if (a == []) then (error "Invalid argument") else a

-- aka words
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where
        (w, s'') = break p s'