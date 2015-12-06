module HasCycle where

type Graph = [[Int]]

-- идея решения состоит в том, что если цикл есть, то мы сможем пройти путь длиной равный количеству вершин в графе
-- запускаю canTravel из каждой вершины графа, а затем ищи хотя бы один True
hasCycle :: Graph -> Bool
hasCycle graph = foldl (||) False ([canTravel x graph | x <- [0..length graph - 1]])

-- проверяет можем ли мы из вершины v проделать путь длиной (length graph)
canTravel v graph = canTravel' v (length graph) graph
canTravel' v n graph
    | n == 0 = True
    | length (graph !! v) == 0 = False
    | otherwise = foldl (||) False (map (\x -> canTravel' x (n - 1) graph) (graph !! v))