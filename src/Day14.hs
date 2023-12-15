module Day14 (day14a) where

import Data.List (transpose)
import qualified Data.List as L
import Data.List.Split (splitOn, splitWhen)

day14a :: IO ()
day14a = do
  input <- readFile "input/Day14.txt"
  let columns = map reverse $ transpose $ lines input

  print $ sum $ findLoad columns

rownums :: (Num b, Enum b) => [a] -> [(a, b)]
rownums str = zip str [1 ..]

splitCols :: [[(Char, b)]] -> [[[(Char, b)]]]
splitCols columns = map (splitWhen ((== '#') . fst)) columns

splitCol :: [(Char, b)] -> [[(Char, b)]]
splitCol = (splitWhen ((== '#') . fst))

numOs :: [(Char, b)] -> Int
numOs = length . filter ((== 'O') . fst)

o2front :: [(Char, Int)] -> Int
o2front ls = sum $ take (numOs ls) $ L.reverse $ L.sort $ map snd ls

findLoad :: [[Char]] -> [Int]
findLoad = map (sum . map o2front . filter (/= []) . splitCol . rownums)
