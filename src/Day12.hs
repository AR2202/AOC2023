module Day12(day12a, print12a)
    where
import Data.List.Split (splitOn, splitWhen)
import GHC.IO.Handle (NewlineMode(inputNL))
-- Part 1
day12a :: IO ()
day12a = do
    input <- readFile "input/Day12.txt"
    let inputs = separateInputs input
    let numPossibilities = map numValidFromList inputs
    print $ sum numPossibilities
day12atest :: Int
day12atest = numValid [1,1,3]$allPossibilities ".??..??...?##."
print12a :: [[[Char]]]
print12a = map (splitOn ("."))$allPossibilities".??..??...?##."

allPossibilities :: String-> [String]
allPossibilities s = allPossibilities' (reverse s) [""] 
    where allPossibilities' "" possibilities = possibilities
          allPossibilities' ('?':cs) possibilities = allPossibilities' cs (appendBoth possibilities) 
          allPossibilities' (x:xs) possibilities = allPossibilities' xs (map (x:) possibilities )
          appendBoth possibilities =   map ('#' :) possibilities ++ map ('.':) possibilities 

filterValid :: [Int] -> [[Char]] -> [[Int]]
filterValid list strs = filter (==list)$(map .map) length $ map (filter (/="")) $ map (splitOn (".")) strs 

filterValidFromList l = filterValid ((parseNumList . last) l) ((allPossibilities . head) l)

numValid :: [Int] -> [[Char]] -> Int
numValid list = length . filterValid list

numValidFromList = length. filterValidFromList
-- reading input
separateInputs :: String -> [[String]]
separateInputs = map words . lines 

parseNumList :: [Char] -> [Int]
parseNumList = map read . splitOn ","