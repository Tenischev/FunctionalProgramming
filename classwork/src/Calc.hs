module Calc where
import Data.Char(isDigit)

calc :: String -> Int
calc "" = 0
calc s = f (read num) $ calc rest where
    (num, rest') = span isDigit s
    getFunc c
        | c == '-' = (-)
        | otherwise = (+)
    (f, rest) = case rest' of
        "" -> ((+), rest')
        (a:tail) -> (getFunc a,tail)