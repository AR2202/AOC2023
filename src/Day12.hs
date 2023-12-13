module Day12 (day12a, day12b) where

import Data.Bifunctor (bimap)
import Data.List (foldl1')
import Data.List.Split (splitOn, splitWhen)

-- Part 1
day12a :: IO ()
day12a = do
  input <- readFile "input/Day12.txt"
  let inputs = separateInputs input
  let numPossibilities = map numValidFromList inputs
  print $ sum numPossibilities

day12atest :: Int
day12atest = numValid [1, 1, 3] $ allPossibilities ".??..??...?##."

allPossibilities :: String -> [String]
allPossibilities s = allPossibilities' (reverse s) [""]
  where
    allPossibilities' "" possibilities = possibilities
    allPossibilities' ('?' : cs) possibilities = allPossibilities' cs (appendBoth possibilities)
    allPossibilities' (x : xs) possibilities = allPossibilities' xs (map (x :) possibilities)
    appendBoth possibilities = map ('#' :) possibilities ++ map ('.' :) possibilities

filterValid :: [[Char]] -> [Int] -> [[Int]]
filterValid strs list = filter (== list) $ (map . map) length $ map (filter (/= "")) $ map (splitOn (".")) strs

filterValidFromList l = filterValid ((allPossibilities . head) l) ((parseNumList . last) l)

numValid :: [Int] -> [[Char]] -> Int
numValid ls = length . (`filterValid` ls)

numValidFromList = length . filterValidFromList

-- reading input
separateInputs :: String -> [[String]]
separateInputs = map words . lines

parseNumList :: [Char] -> [Int]
parseNumList = map read . splitOn ","

-- Part2
day12b :: IO ()
day12b = do
  input <- readFile "input/Day12.txt"
  let inputs = separateInputs input
  let numPossibilities = map numValidFromListPart2 inputs
  print $ sum numPossibilities

unfoldInput s = foldl1' (\acc l -> acc ++ ('?' : l)) $ replicate 5 s

unfoldNums :: [a] -> [a]
unfoldNums = concat . replicate 5

unfoldBoth :: String -> ([Char], [Int])
unfoldBoth s = ((unfoldInput . head . words) s, (unfoldNums . parseNumList . last . words) s)

mapAllPossibilities :: (String, d) -> ([String], d)
mapAllPossibilities = bimap allPossibilities id

numValidFromListPart2 ls = fromIntegral (numValidFromList ls) ** 5 * 2 ** 4
