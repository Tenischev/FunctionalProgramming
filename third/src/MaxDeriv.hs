module MaxDeriv where
import Data.List(elemIndex)
import Data.Maybe(fromMaybe)

maxDeriv :: Real a => [a] -> Int
maxDeriv list = fromMaybe (-1) (elemIndex (maximum list1) list1) where
    list1 = (tail $ zipWith (-) list (0:list)) ++ [0]