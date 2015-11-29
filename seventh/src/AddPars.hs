module AddPars where
import Data.Char(isDigit, intToDigit)
import Data.List(groupBy, partition)

-- "1+2+3+4+5+6+7+8+9+10+11+12+13+14+15" считает меньше минуты, осторожнее с оперативной памятью
data Tree b = Empty | Node (Tree b) Int (Tree b) deriving (Eq)

instance Ord (Tree b) where
    compare (Node l1 e1 r1) (Node l2 e2 r2) = compare e1 e2

instance Show (Tree b) where
    show Empty = ""
    show (Node Empty x Empty) = show x
    show (Node t1 x t2) = '(':(show t1) ++ (whatYouDid t1 x t2):(show t2) ++ ")"

addPars :: String -> String
addPars = show . findMax . interpret . splitter

findMax :: ([Int], [(Int -> Int -> Int)]) -> Tree b
findMax (values, signs) = maximum $ findMax' values signs 0 (length signs - 1)

findMax' :: [Int] -> [(Int -> Int -> Int)] -> Int -> Int -> [Tree b]
findMax' values signs l r
    | l > r = [Node Empty (values !! l) Empty]
    | otherwise = concat $ map (calc values signs l r) [l..r]

calc :: [Int] -> [(Int -> Int -> Int)] -> Int -> Int -> Int -> [Tree b]
calc val sign l r pos = concat $ map (mergeTree (sign !! pos) rT) lT where
    lT = findMax' val sign l (pos - 1)
    rT = findMax' val sign (pos + 1) r

mergeTree :: (Int -> Int -> Int) -> [Tree b] -> Tree b -> [Tree b]
mergeTree op rTL lT@(Node l1 e1 r1) = map (\rT@(Node l2 e2 r2) -> (Node lT (op e1 e2) rT)) rTL

splitter :: String -> ([String], [String])
splitter str = (numbers, sign) where
    split = groupBy (\x1 x2 -> (isDigit x1) && (isDigit x2)) str
    (numbers, sign) = partition (foldl (flip $ (||) . isDigit) False) split

interpret :: ([String], [String]) -> ([Int], [(Int -> Int -> Int)])
interpret (numb, sign) = (map (read) numb, map (getOperation) sign)

getOperation :: String -> (Int -> Int -> Int)
getOperation op = case op of
                    "+" -> (+)
                    "-" -> (-)
                    "*" -> (*)

whatYouDid (Node l1 e1 r1) res (Node l2 e2 r2)
    | res == e1 + e2 = '+'
    | res == e1 - e2 = '-'
    | otherwise = '*'