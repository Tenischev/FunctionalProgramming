module RemoveBy where
import Data.Maybe

data Map key value = Empty | Node (key,value) (Map key value) -- deriving (Show)

instance (Show key, Show value) => Show (Map key value) where
    showsPrec d m  = showParen (d > 10) $ shows (toList m)

get :: Eq k => k -> Map k v -> Maybe v
get k m = case (getEntry k m) of
    Nothing -> Nothing
    Just v -> Just (snd v)

getEntry :: Eq k => k -> Map k v -> Maybe (k, v)
getEntry k Empty = Nothing
getEntry k (Node tup m)
    | k == (fst tup) = Just tup
    | otherwise = getEntry k m

put :: Eq k => (k, v) -> Map k v -> Map k v
put el m = Node el (remove (fst el) m)

remove :: Eq k => k -> Map k v -> Map k v
remove k m = removeBy (== k) m

keys :: Map k v -> [k]
keys Empty = []
keys (Node tup m) = (fst tup):keys m

values :: Map k v -> [v]
values Empty = []
values (Node tup m) = (snd tup):values m

toList :: Map k v -> [(k,v)]
toList Empty = []
toList (Node tup m) = tup:(toList m)

removeBy :: (k -> Bool) -> Map k v -> Map k v
removeBy f Empty = Empty
removeBy f (Node tup m)
    | f (fst tup) = removeBy f m
    | otherwise = Node tup (removeBy f m)