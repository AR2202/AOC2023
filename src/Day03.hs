module Day03 (day3a) where

import Common (addCoordinates, addCoordinatest, capitalize, loadApplyPrint, lstrip, neighbors, rstrip)
import qualified Data.List as L
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map as M

---Types
type SymbolMap = M.Map (Int, Int) Char

-- Part 1
extractNums :: [(a, Char)] -> [[(a, Char)]]
extractNums = splitWhen ((`notElem` "1234567890") . snd)

extractSnd :: [[(a, b)]] -> [[b]]
extractSnd = (map . map) snd

-- reading input

--day3 :: ([String] -> Integer) -> IO ()
day3 = loadApplyPrint "input/Day03.txt"

day3a :: IO ()
day3a = day3 numsWithSymbNeighbors

withCoords :: [[Char]] -> [((Int, Int), Char)]
withCoords = addCoordinatest

makeMap :: [[Char]] -> M.Map (Int, Int) Char
makeMap = M.fromList . withCoords

hasSymbolNeighbor :: SymbolMap -> ((Int, Int), Char) -> Bool
hasSymbolNeighbor symmap charWcoord = any (\x -> M.findWithDefault '.' x symmap `notElem` "1234567890.") ((neighbors . fst) charWcoord)

hasSymbolNeighborF :: SymbolMap -> ((Int, Int), Char) -> Bool
hasSymbolNeighborF symmap charWcoord = any (\x -> M.findWithDefault '.' x symmap `notElem` "1234567890.") ((frontNeighbors . fst) charWcoord)

hasSymbolNeighborB :: SymbolMap -> ((Int, Int), Char) -> Bool
hasSymbolNeighborB symmap charWcoord = any (\x -> M.findWithDefault '.' x symmap `notElem` "1234567890.") ((backNeighbors . fst) charWcoord)

--numsWithSymbNeighbors :: (Num c, Read c) => [[Char]] -> c
numsWithSymbNeighbors s = (sum .  map read . extractSnd . filter (any (hasSymbolNeighbor symmap)) . extractNums . withCoords) s
  where
    symmap = makeMap s


--numsWithSymbNeighbors'' ::  [[Char]] -> [Int]
numsWithSymbNeighbors'' s = (  filter (any (hasSymbolNeighbor symmap)) . extractNums . withCoords) s
  where
    symmap = makeMap s


numsWithSymbNeighbors' :: (Num c, Read c) => [[Char]] -> c
numsWithSymbNeighbors' s = (sum .  map read . extractSnd . filter ((\list -> ((hasSymbolNeighborF symmap) (head list) || (hasSymbolNeighborB symmap) (last list)))) . filter (/= []) . extractNums . withCoords) s
  where
    symmap = makeMap s

frontNeighbors :: (Num a1, Num a2) => (a2, a1) -> [(a2, a1)]
frontNeighbors (y, x) = [(y, x - 1), (y - 1, x), (y + 1, x), (y - 1, x - 1), (y + 1, x - 1)]

backNeighbors :: (Num a1, Num a2) => (a2, a1) -> [(a2, a1)]
backNeighbors (y, x) = [(y, x + 1), (y - 1, x), (y + 1, x), (y - 1, x + 1), (y + 1, x + 1)]
