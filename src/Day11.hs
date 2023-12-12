module Day11(day11a)
    where 

import Common(addCoordinates)
import Data.List(foldl', transpose)


-- Part 1 
day11a :: IO ()
day11a = do
    input<- readFile "input/Day11.txt"

    let expanded = (expandHorizontal . expandVertical) $lines input 
    let galaxyCoords = (map fst . filterGalaxies . addCoordinates) expanded
    let dists = allDists galaxyCoords
    
    print $ sum dists
  

filterGalaxies :: [(a, Char)] -> [(a, Char)]
filterGalaxies = filter((=='#').snd)

dist2one :: Num a => (a, a) -> (a, a) -> a
dist2one (x,y) (x2,y2) = abs(x-x2) + abs(y-y2)
dist2all :: Num b => (b, b) -> [(b, b)] -> [b]
dist2all node = map (dist2one node)

allDists :: Num a => [(a, a)] -> [a]
allDists [] = []
allDists (x:xs) = dist2all x xs ++ allDists xs

expandVertical ::  [[Char]] -> [[Char]]
expandVertical  = foldl' (\acc line -> if all(=='.') line then line: line:acc else line:acc)[] 
expandHorizontal :: [[Char]] -> [[Char]]
expandHorizontal =  reverse .transpose . reverse .expandVertical . transpose