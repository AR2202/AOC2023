module Day09 (day9a, day9b, day9ex2) where

import qualified Data.List as L

nextRow :: Num a => [a] -> [a]
nextRow row = zipWith (-) (tail row) row

allRows row = go row []
  where
    go row lasts
      | all (== 0) row = sum lasts
      | otherwise = go (nextRow row) (last row : lasts)



day9ex2 :: [Integer]
day9ex2 = previous [10, 13, 16, 21, 30, 45]

-- read Input
readDay9 :: IO [[Int]]
readDay9 = do
  input <- readFile "input/Day09.txt"
  let ls = lines input
  let nums = map (map read . words) ls
  return nums

day9a :: IO ()
day9a = do
  rows <- readDay9
  print $ sum $ map allRows rows

-- part 2
previous :: (Eq a, Num a) => [a] -> [a]
previous row = go row []
  where
    go row firsts
      | all (== 0) row = firsts 
      | otherwise = go (nextRow row) (head row : firsts)



findPrevious :: [Int] -> Int
findPrevious = L.foldl' (flip (-)) 0

day9b :: IO ()
day9b = do
  rows <- readDay9
  let nums = map (findPrevious . previous) rows

  print $ sum nums
