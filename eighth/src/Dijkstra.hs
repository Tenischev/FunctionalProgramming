module Dijkstra where
import Data.List(sort)

type Graph = [[(Int,Integer)]]

dijkstra :: Int -> Int -> Graph -> Maybe [Int]
dijkstra s = nextStep (distF (== s) (0,[s]) (\_ -> (-1, []))) [] s

nextStep dist front current end graph
    | current == end = Just ((reverse . snd) $ dist current)
    | otherwise = relaxFront dist newFront end graph
    where
        path = fst $ dist current
        -- просто добавляю в front все возможные пути из текущей вершины
        newFront = foldl (\list (ver, cost) -> (cost + path, ver, current):list) front (graph !! current)

relaxFront dist front end graph
    | length front' == 0 = Nothing
    | otherwise = nextStep (distF (== v) (path, v:(snd $ dist parent)) dist) newFront v end graph
    where -- перед сортировкой убираю вершины до которых уже посчитано расстояние
        front' = sort $ filter (\(cost, x, p) -> fst (dist x) == -1) front
        newFront = tail front'
        (path, v, parent) = head front'

-- шаблон для функции, которая по номеру вершины будет возвращать путь и его стоимость до нее или -1 если путь еще не найден
distF = \p x y -> (\s -> if (p s) then (x) else (y s))