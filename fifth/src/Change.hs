module Change where

-- это предельно не удобная реализация в функциональном программировании
data BinHeap e = BinHeap [e] deriving (Show)

-- подъем элемента в куче
lift :: Ord e => Int -> BinHeap e -> BinHeap e
lift ind (BinHeap heap)
    | ind == 0 = BinHeap heap
    | child < parent = lift pInd $ BinHeap (l ++ (child:m) ++ (parent:r))
    | otherwise = BinHeap heap
    where
       (m'',r') = splitAt ind heap
       (l,m') = splitAt pInd m''
       m = tail m'
       r = tail r'
       child = heap !! ind
       pInd = (ind - 1) `div` 2
       parent = heap !! pInd

change :: Ord e => Int -> e -> BinHeap e -> BinHeap e
change ind elem (BinHeap heap) = lift ind (BinHeap (l ++ (elem:r))) where
    (l,r') = splitAt ind heap
    r = tail r'