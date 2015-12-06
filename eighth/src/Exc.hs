module Exc where
import Data.List(find)

type Graph = (Int, Int -> Int -> Bool)

exc :: Int -> Graph -> Int
exc v graph = nextStep 0 (avalibles [v] graph) [v] graph

-- парадируем bfs, первое - число итераций и есть эксцентриситет
nextStep :: Int -> [Int] -> [Int] -> Graph -> Int
nextStep step [] visit graph = step
nextStep step front visit graph = nextStep (step + 1) newFront (front ++ visit) graph where
    newFront = filter (\v -> not (elem v visit)) (avalibles front graph) -- выкидываем посещенные

-- выдет список вершин с повторениями доступных из вершин front
avalibles front graph = concat $ map (\v -> filter ((snd graph) v) [0..(fst graph) - 1]) front
