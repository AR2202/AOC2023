{-# LANGUAGE ScopedTypeVariables #-}

module Day05 (day5a, day5b) where

import Common (loadApplyPrint, loadInput, splitOnBlankLine)
import Data.List (foldl')
import qualified Data.List as L
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (uncurry3)

-- read input
day5a :: IO ()
day5a = do
  inp <- splitOnBlankLine "Day05.txt"
  let seeds :: [Int] = map read $ splitOn (" ") $ drop 2 $ L.dropWhile (/= ':') $ head inp

  let maps :: [[(Int, Int, Int)]] = map (map (list2triple . map read . splitOn (" ")) . tail . lines) $ tail inp
  let targets = foldl' allTargets seeds maps
  let locmin = minimum targets

  print seeds

  print locmin

-- partial function, but assuming input is valid
list2triple :: [c] -> (c, c, c)
list2triple (x : y : z : zs) = (y, x, z)

list2triplep2 :: [c] -> (c, c, c)
list2triplep2 (x : y : z : zs) = (x, y, z)

-- part 1
inRange :: (Ord a, Num a) => a -> a -> a -> a -> Maybe a
inRange seed source target nums
  | seed >= source && seed < (source + nums) = Just (target + seed - source)
  | otherwise = Nothing

inAnyRange :: (Ord t, Num t) => [(t, t, t)] -> t -> t
inAnyRange [] seed = seed
inAnyRange (x : xs) seed = case uncurry3 (inRange seed) x of
  Just t -> t
  Nothing -> inAnyRange xs seed

allTargets :: (Ord b, Num b) => [b] -> [(b, b, b)] -> [b]
allTargets seeds l = map (inAnyRange l) seeds

-- part 2

day5b :: IO ()
day5b = do
  inp <- splitOnBlankLine "Day05.txt"
  let seeds :: [Int] = map read $ splitOn (" ") $ drop 2 $ L.dropWhile (/= ':') $ head inp
  let ranges = toRanges seeds
  let seedRanges = findRanges ranges
  --let allSeeds = concatMap range2list seedRanges

  let maps :: [[(Int, Int, Int)]] = reverse $ map (map (list2triplep2 . map read . splitOn (" ")) . tail . lines) $ tail inp

  let locmin = findSeeds 0 maps seedRanges

  print locmin

toRanges :: [b] -> [(b, b)]
toRanges [] = []
toRanges (x : y : zs) = (x, y) : toRanges zs

findRanges :: (Ord b, Num b) => [(b, b)] -> [(b, b)]
findRanges [] = []
findRanges (x : xs) = (fst x, fst x + (maximum . map snd . filter inXRange) (x : xs)) : findRanges (filter (not . inXRange) xs)
  where
    inXRange x1 = (fst x1 >= fst x) && fst x1 < (fst x + snd x)

range2list :: (Num a, Enum a) => (a, a) -> [a]
range2list (x, y) = [x .. y - 1]

isBetween :: Ord a => a -> (a, a) -> Bool
isBetween x (y, z) = (x >= y) && (x<z)


findSeeds location maps seedRanges
  | any (isBetween (foldl' (flip inAnyRange) location maps) ) seedRanges = location
  | otherwise = findSeeds (location + 1) maps seedRanges
