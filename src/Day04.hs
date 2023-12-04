module Day04 (day4a) where

import Common (addCoordinates, addCoordinatest, capitalize, loadApplyPrint, lstrip, neighbors, rstrip)
import qualified Data.List as L
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics ((:+:) (R1))

---Types
type Scratchcard = (S.Set Int, S.Set Int)

-- reading input

day4 = loadApplyPrint "input/Day04.txt"

day4a :: IO ()
day4a = day4 solve4a

str2SC :: [[Char]] -> [Scratchcard]
str2SC = map (toScratchCard . map (map read . filter (/= "") . splitOn " " . rstrip " " . lstrip " ") . sepNums . stripCardNum)

stripCardNum :: [Char] -> [Char]
stripCardNum = drop 2 . dropWhile (/= ':')

sepNums :: [Char] -> [[Char]]
sepNums = splitOn ("|")

toScratchCard :: Ord a => [[a]] -> (S.Set a, S.Set a)
toScratchCard l = (S.fromList $ head l, S.fromList $ last l)

-- Part 1
numberIntersection :: Scratchcard -> S.Set Int
numberIntersection (winn, act) = S.intersection winn act

numWinning :: Scratchcard -> Int
numWinning = S.size .numberIntersection

cardWorth 0 = 0
cardWorth x = 2^(x-1)

solve4a :: [[Char]] -> Integer
solve4a = sum . (map (cardWorth .numWinning) .str2SC)