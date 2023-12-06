{-# LANGUAGE ScopedTypeVariables #-}

module Day06 (day6a) where

testtimes = [7, 15, 30]

testdists = [9, 40, 200]

inptimes = [62, 73, 75, 65]

inpdists = [644, 1023, 1240, 1023]

chargetimeMin t d = t / 2 - sqrt ((t / 2) ** 2 - d)

chargetimeMax t d = t / 2 + sqrt ((t / 2) ** 2 - d)

day6a = do
  let minbounds :: [Int] = map (+ 1) $ map floor $ zipWith chargetimeMin inptimes inpdists
  let maxbounds :: [Int] = map ceiling $ zipWith chargetimeMax inptimes inpdists
  print minbounds
  print maxbounds

  let nums = zipWith (-) maxbounds minbounds
  print $ product nums
