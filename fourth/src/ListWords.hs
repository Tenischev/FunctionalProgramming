module ListWords where
import Control.Monad(join)

data Trie = Empty | Node Char [Trie]
type Dictionary = [Trie]

foldTrie :: Trie -> [String]
foldTrie Empty = [""]
foldTrie (Node c lt) = map (c:) (listWords lt)

listWords :: Dictionary -> [String]
listWords d = join $ map (foldTrie) d