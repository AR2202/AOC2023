module Day01 (day1a, day1b) where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

-- reading input
day1 f = do
  input <- readFile "input/Day01.txt"
  let inputlines = lines input
  let calibrationVals = map f inputlines

  print $ sum calibrationVals

-- Part 1

day1a = day1 calibrationVal

firstDigit :: String -> String
firstDigit s = take 1 $ dropWhile (`notElem` "1234567890") s

lastDigit = firstDigit . reverse

calibrationVal :: String -> Int
calibrationVal s = read $ firstd ++ lastd
  where
    firstd = firstDigit s
    lastd = lastDigit s

-- Part2

day1b = day1 calibrationValP2

calibrationValP2 :: String -> Int
calibrationValP2 s = 10 * firstd + lastd
  where
    firstd = lookUpDigit findFirstDigit s
    lastd = lookUpDigit findLastDigit s

digitMap :: M.Map String Int
digitMap = M.fromList $ zip validDigits (cycle [0 .. 9])

validDigits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ++ map show [0 .. 9]

findPrefix pre s = filter (`isPrefixOf` s) pre

findDigits s = map (findPrefix validDigits) (tails s)

findFirstDigit = head . head . filter (/= []) . findDigits

findLastDigit = head . last . filter (/= []) . findDigits

lookUpDigit f s = M.findWithDefault 0 (f s) digitMap