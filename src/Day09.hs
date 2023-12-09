module Day09 (day9a) where

nextRow :: Num a => [a] -> [a]
nextRow row = zipWith (-) (tail row) row

allRows row = go row []
  where
    go row lasts
      | all (== 0) row = sum lasts
      | otherwise = go (nextRow row) (last row : lasts)

day9ex = allRows [0, 3, 6, 9, 12, 15]

-- read Input
readDay9 :: IO [[Int]]
readDay9 = do
  input <- readFile "input/Day09.txt"
  let ls = lines input
  let nums = map (map read .  words) ls
  return nums

day9a :: IO ()
day9a = do
  rows <- readDay9
  print $ sum $ map allRows rows
