module Day11 (day11a, day11b) where

import Common (addCoordinates)
import Data.Bifunctor (bimap)
import Data.List (foldl', transpose)

-- Part 1
day11a :: IO ()
day11a = do
  input <- readFile "input/Day11.txt"

  let expanded = (expandHorizontal . expandVertical) $ lines input
  let galaxyCoords = galaxyCoordinates expanded
  let dists = allDists galaxyCoords

  print $ sum dists

galaxyCoordinates :: [String] -> [(Int, Int)]
galaxyCoordinates = map fst . filterGalaxies . addCoordinates

filterGalaxies :: [(a, Char)] -> [(a, Char)]
filterGalaxies = filter ((== '#') . snd)

dist2one :: Num a => (a, a) -> (a, a) -> a
dist2one (x, y) (x2, y2) = abs (x - x2) + abs (y - y2)

dist2all :: Num b => (b, b) -> [(b, b)] -> [b]
dist2all node = map (dist2one node)

allDists :: Num a => [(a, a)] -> [a]
allDists [] = []
allDists (x : xs) = dist2all x xs ++ allDists xs

expandVertical :: [[Char]] -> [[Char]]
expandVertical = foldl' (\acc line -> if all (== '.') line then line : line : acc else line : acc) []

expandHorizontal :: [[Char]] -> [[Char]]
expandHorizontal = reverse . transpose . reverse . expandVertical . transpose

-- part 2

day11b :: IO ()
day11b = do
  input <- readFile "input/Day11.txt"
  let inplines = lines input
  let galaxyCoords = galaxyCoordinates inplines
  let updatedCoords = updateCoords (x2expand inplines) (y2expand inplines) (map (bimap toInteger toInteger) galaxyCoords)
  let dists = allDists updatedCoords

  print $ sum dists

y2expand :: [[Char]] -> [Integer]
y2expand ls = map fst . filter (all (== '.') . snd) $ zip [1 ..] ls

x2expand :: [[Char]] -> [Integer]
x2expand = y2expand . transpose

updateCoord :: [Integer] -> [Integer] -> (Integer, Integer) -> (Integer, Integer)
updateCoord xExpand yExpand (x, y) = (newx, newy)
  where
    newx = 999999 * numXExpand + x
    newy = 999999 * numYExpand + y
    numXExpand = fromIntegral $ (length . filter (< x)) xExpand
    numYExpand = fromIntegral $ (length . filter (< y)) yExpand

updateCoords :: [Integer] -> [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
updateCoords xExpand yExpand = map (updateCoord xExpand yExpand)
