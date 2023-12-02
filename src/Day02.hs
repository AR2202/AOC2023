module Day02 (day2a, day2b) where

import Common (capitalize, loadApplyPrint)
import Data.List
import Data.List.Split (splitOn, splitWhen)

---Types

data CubeGame = CubeGame {gameId :: Int, alldraws :: [Draw]} deriving (Show, Eq)

data Draw = Draw {red :: Int, blue :: Int, green :: Int} deriving (Show, Eq)

data Color = Red Int | Blue Int | Green Int deriving (Show, Read, Eq)

data PossibleGame = Possible | Impossible deriving (Show, Eq)

-- Part 1

toColor :: String -> Color
toColor s = read $ init $ color ++ number
  where
    color = capitalize $ dropWhile (`notElem` "redgreenblue") s
    number = takeWhile (`notElem` "redgreenblue") s

toDraw :: [Color] -> Draw
toDraw = foldl' addcolor (Draw 0 0 0)
  where
    addcolor draw (Red x) = draw {red = x}
    addcolor draw (Blue x) = draw {blue = x}
    addcolor draw (Green x) = draw {green = x}

-- used for debugging only
toGame :: (Int, [Draw]) -> CubeGame
toGame (i, drawlist) = CubeGame i drawlist

-- used for debugging only
toMaxgreen :: (a, [Draw]) -> (a, Int)
toMaxgreen (i, drawlist) = (i, maxGreen drawlist)

toPossible :: (a, [Draw]) -> (a, PossibleGame)
toPossible (i, drawlist)
  | maxGreen drawlist > 13 = (i, Impossible)
  | maxRed drawlist > 12 = (i, Impossible)
  | maxBlue drawlist > 14 = (i, Impossible)
  | otherwise = (i, Possible)

filterPossible :: [(a, [Draw])] -> [(a, PossibleGame)]
filterPossible l = filter (\x -> snd x == Possible) $ map toPossible l

addPossible :: Num a => [(a, [Draw])] -> a
addPossible l = sum $ map fst $ filterPossible l

toGameTuple :: String -> (Int, [Draw])
toGameTuple s = (read $ findId s, map str2draw . draws2colors . draws2draw . draws $ s)

findId :: [Char] -> [Char]
findId = dropWhile (`notElem` "1234567890") . head . splitWhen (== ':')

draws :: [Char] -> [Char]
draws = last . splitWhen (== ':')

draws2draw :: [Char] -> [[Char]]
draws2draw = splitOn (";")

draws2colors :: [[Char]] -> [[[Char]]]
draws2colors = map (splitOn (","))

str2draw :: [String] -> Draw
str2draw = toDraw . map toColor

maxGreen :: [Draw] -> Int
maxGreen draws = maximum $ map green draws

maxRed :: [Draw] -> Int
maxRed draws = maximum $ map red draws

maxBlue :: [Draw] -> Int
maxBlue draws = maximum $ map blue draws

-- reading input

day2 :: ([String] -> Int) -> IO ()
day2 = loadApplyPrint "input/Day02.txt"

day2a :: IO ()
day2a = day2 (addPossible . map toGameTuple)

-- Part 2
gamePower :: [Draw] -> Int
gamePower d = maxGreen d * maxRed d * maxBlue d

day2b :: IO ()
day2b = day2 (sum . map (gamePower . snd . toGameTuple))
