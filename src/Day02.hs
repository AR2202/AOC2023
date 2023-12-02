module Day02 (day2a) where

import Data.List
import Data.List.Split(splitWhen, splitOn)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Common (capitalize)

---Types

data CubeGame = CubeGame {gameId :: Int, alldraws :: [Draw]} deriving (Show, Eq)

data Draw = Draw {red :: Int, blue :: Int, green :: Int} deriving (Show, Eq)

data Color = Red Int | Blue Int |Green Int deriving(Show,Read,  Eq)

data PossibleGame = Possible | Impossible deriving (Show, Eq)

toColor :: String -> Color
toColor s = read $ init $ color  ++ number
    where color = capitalize $ dropWhile (`notElem`"redgreenblue")s  
          number = takeWhile (`notElem`"redgreenblue") s  

toDraw :: [Color] -> Draw
toDraw colorList= foldl'(\draws color ->addcolor draws color) (Draw 0 0 0 )colorList
    where   addcolor draw (Red x) = draw{red = x}
            addcolor draw (Blue x) = draw{blue = x}
            addcolor draw (Green x) = draw{green = x}

toGame (i, drawlist) = CubeGame i drawlist
toMaxgreen (i, drawlist) = (i, maxGreen drawlist)
toPossible (i, drawlist) 
    | maxGreen drawlist > 13 = (i, Impossible)
    | maxRed drawlist > 12 = (i, Impossible)
    | maxBlue drawlist > 14 = (i, Impossible)
    | otherwise = (i, Possible)
filterPossible l = filter (\x-> snd x == Possible) $ map toPossible l 
addPossible l = sum $ map fst $ filterPossible l
toGameTuple :: String -> (Int, [Draw])
toGameTuple s = (read $ findId s, map str2draw . draws2colors.draws2draw.draws $s)
findId =  dropWhile (`notElem`"1234567890") . head. splitWhen (==':')
draws = last . splitWhen (==':')
draws2draw = splitOn (";")
draws2colors =  map (splitOn (","))
str2draw = toDraw . map toColor

maxGreen draws = maximum $ map green draws
maxRed draws = maximum $ map red draws
maxBlue draws =  maximum $ map blue draws
-- reading input
day2a = do
  input <- readFile "input/Day02.txt"
  let inputlines = lines input
 

  print $   addPossible $ map   toGameTuple inputlines
  
