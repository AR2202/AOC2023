module Day13 (day13a) where

import qualified Data.IntMap as IM
import qualified Data.List as L
import Common(splitOnBlankLine)

-- Part 1
day13a :: IO ()
day13a = do
  patterns <- splitOnBlankLine "Day13.txt"
  let nums = map reflectionNum patterns
  
  

  print $ sum nums

makeLineMap ls = IM.fromList $ zip [1 ..] ls
rowMap = makeLineMap . L.transpose . lines 
lineMap = makeLineMap . lines 

reflectionNum pat = 100 * reflectionH + reflectionV 
    where reflectionH = findReflection (lineMap pat) 1 2 1
          reflectionV = findReflection (rowMap pat) 1 2 1
lineMatches [] [] = True
lineMatches _ [] = False
lineMatches [] _ = False
lineMatches (x : xs) (y : ys)
  | x /= y = False
  | otherwise = lineMatches xs ys

findReflection linemap currInd1 currInd2 currTest =
  case IM.lookup currInd1 linemap of
    Nothing -> if currTest == IM.size linemap then 0 else currTest
    Just x -> case IM.lookup currInd2 linemap of
      Nothing ->  if currTest == IM.size linemap then 0 else currTest
      Just y -> if lineMatches x y then findReflection linemap (currInd1 - 1) (currInd2 + 1) currTest else findReflection linemap (currTest + 1) (currTest + 2) (currTest + 1)
