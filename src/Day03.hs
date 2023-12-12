module Day03 (day3a) where

import Common (addCoordinates, loadApplyPrint, neighbors)
import qualified Data.List as L
import qualified Data.Map as M

---Types
type SymbolMap = M.Map (Int, Int) Char

-- Part 1
extractNums :: [(a, Char)] -> [[(a, Char)]]
extractNums = splitWhen ((`notElem` "1234567890") . snd)

extractSnd :: [[(a, b)]] -> [[b]]
extractSnd = (map . map) snd

-- reading input

day3 :: ([String] -> Integer) -> IO ()
day3 = loadApplyPrint "input/Day03.txt"

day3a :: IO ()
day3a = day3 numsWithSymbNeighbors

withCoords :: [[Char]] -> [((Int, Int), Char)]
withCoords = L.reverse . addCoordinates . L.reverse . L.transpose

makeMap :: [[Char]] -> M.Map (Int, Int) Char
makeMap = M.fromList . withCoords

hasSymbolNeighbor :: SymbolMap -> ((Int, Int), Char) -> Bool
hasSymbolNeighbor symmap charWcoord = any (\x -> M.findWithDefault '.' x symmap `notElem` "1234567890.") ((neighbors . fst) charWcoord)

numsWithSymbNeighbors :: (Num c, Read c) => [[Char]] -> c
numsWithSymbNeighbors s = (sum . map read . extractSnd . filter (any (hasSymbolNeighbor symmap)) . extractNums . withCoords) s
  where
    symmap = makeMap s
