module Day04 (day4a, day4b) where

import Common (addCoordinates, addCoordinatest, capitalize, loadApplyPrint, lstrip, neighbors, rstrip)
import qualified Data.List as L
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics ((:+:) (R1))

---Types
type Scratchcard = (S.Set Int, S.Set Int)

type CardCopies = M.Map Int Integer

-- reading input

day4 :: ([String] -> Int) -> IO ()
day4 = loadApplyPrint "input/Day04.txt"

day4a :: IO ()
day4a = day4 solve4a

str2SC :: [[Char]] -> [Scratchcard]
str2SC = map (toScratchCard . map (map read . filter (/= "") . splitOn " " . rstrip " " . lstrip " ") . sepNums . stripCardNum)

stripCardNum :: [Char] -> [Char]
stripCardNum = drop 2 . dropWhile (/= ':')

sepNums :: [Char] -> [[Char]]
sepNums = splitOn "|"

toScratchCard :: Ord a => [[a]] -> (S.Set a, S.Set a)
toScratchCard l = (S.fromList $ head l, S.fromList $ last l)

-- Part 1
numberIntersection :: Scratchcard -> S.Set Int
numberIntersection (winn, act) = S.intersection winn act

numWinning :: Scratchcard -> Int
numWinning = S.size . numberIntersection

cardWorth :: (Num p, Integral b) => b -> p
cardWorth 0 = 0
cardWorth x = 2 ^ (x - 1)

points :: [[Char]] -> [Int]
points = map (cardWorth . numWinning) . str2SC

solve4a :: [[Char]] -> Int
solve4a = sum . points

-- Part 2
day4b :: IO ()
day4b = day4 solve4b

copies :: [[Char]] -> [Int]
copies = map numWinning . str2SC

initialCards inp = M.fromList $ zip [1 .. length inp] (repeat 1)

addCopies1 m (i, pnts) = go (i + 1) pnts m (M.findWithDefault 0 i m)
  where
    go _ 0 mp _ = mp
    go ind p mp mult = go (ind + 1) (p - 1) (M.adjust (+ mult) ind mp) mult

addCopiesAll :: [[Char]] -> M.Map Int Int
addCopiesAll inp = L.foldl' addCopies1 (initialCards inp) (zip [1 ..] (copies inp))

solve4b :: [[Char]] -> Int
solve4b = (M.foldr (+) 0) . addCopiesAll
