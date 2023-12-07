{-# LANGUAGE ScopedTypeVariables #-}

module Day06 (day6a,day6b) where


-- Part 1

-- Inputs 
testtimes = [7, 15, 30]


testdists = [9, 40, 200]

inptimes :: [Double]
inptimes = [62, 73, 75, 65]

inpdists :: [Double]
inpdists = [644, 1023, 1240, 1023]

chargetimeMin :: Floating a => a -> a -> a
chargetimeMin t d = t / 2 - sqrt ((t / 2) ** 2 - d)

chargetimeMax :: Floating a => a -> a -> a
chargetimeMax t d = t / 2 + sqrt ((t / 2) ** 2 - d)

day6a :: IO ()
day6a = do
  let minbounds :: [Int] = map (+ 1) $ map floor $ zipWith chargetimeMin inptimes inpdists
  let maxbounds :: [Int] = map ceiling $ zipWith chargetimeMax inptimes inpdists
  print minbounds
  print maxbounds

  let nums = zipWith (-) maxbounds minbounds
  print $ product nums

-- Part 2
-- Inputs
ex6btime = 71530
ex6bdist = 940200
time6b = 62737565
dist6b = 644102312401023

day6b :: IO ()
day6b = do
  let minbound = (+) 1 $ floor $  chargetimeMin time6b dist6b
  let maxbound  = ceiling $ chargetimeMax time6b dist6b
  print $ maxbound - minbound
