module PreHigher where
import Data.List(findIndices)

preHigher :: Ord a => [a] -> [Int]
preHigher [] = []
preHigher list@(first:rest) = findIndices id list1 where
    list1 = tail $ zipWith (<) (first:list) list