{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day24 (testmParser, testLineParser, testIntersept, day24a) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import GHC.IO.Handle (NewlineMode(inputNL))
import qualified Data.Text as T

data Line = Line {start_x :: Float, start_y :: Float, xvel :: Float, m :: Float, b :: Float} deriving (Show, Eq)
-- PArt 1
day24a :: IO ()
day24a = do 
    input<- readFile "input/Day24.txt"
    let ls = traverse (parse lineParser "test") $ map T.pack $lines input 
    let valid = interseptsValid 200000000000000 400000000000000 <$> ls 
    print $length <$> valid 


mparser :: Parser Float
mparser = flip (/) <$> (many1 (space <|> digit <|> char ',') *> xvelParser <* spaces) <*> (yvelParser <* many1 (space <|> digit <|> char ','))

xvelParser :: Parser Float
xvelParser = read @Float <$> between (string "@ ") (string ", ") (spaces *>many1 (digit <|> char '-'))

yvelParser :: Parser Float
yvelParser = read @Float <$> (spaces *>many1 (digit <|> char '-')) <* char (',')

testmParser = parse mparser "test" "19, 13, 30 @ -2,  1, -2"

testLineParser = parse lineParser "test" "19, 13, 30 @ -2,  1, -2"

lineParser = toLine <$> (yvelParser <* spaces) <*> yvelParser <*> (many1 (space <|> digit <|> char ',') *> xvelParser <* spaces) <*> (yvelParser <* many1 (space <|> digit <|> char ','))

toM = flip (/)

toB x y slope = y - (slope * x)

toLine x y xvel yvel = Line x y xvel slope (toB x y slope)
  where
    slope = toM xvel yvel

findXIntersept line1 line2 = (b line1 - b line2) / (m line2 - m line1)

findYIntersept xintersept line = m line * xintersept + b line

inRange rangemin rangemax x y = x >= rangemin && x <= rangemax && y >= rangemin && y <= rangemax

interseptInRange rangemin rangemax line1 line2 = inRange rangemin rangemax xintersept yintersept
  where
    xintersept = findXIntersept line1 line2
    yintersept = findYIntersept xintersept line1

forwardInTime line xintersept yintersept = (xintersept - start_x line) * xvel line >= 0 && (yintersept - start_y line) * m line * xvel line >= 0

testIntersept = findXIntersept <$> parse lineParser "test" "19, 13, 30 @ -2, 1, -2" <*> parse lineParser "test" "18, 19, 22 @ -1, -1, -2"


validIntersept rangemin rangemax line1 line2 = inRange rangemin rangemax xintersept yintersept && forwardInTime line1 xintersept yintersept && forwardInTime line2 xintersept yintersept
  where
    xintersept = findXIntersept line1 line2
    yintersept = findYIntersept xintersept line1

interseptsValid r1 r2 [] = []
interseptsValid r1 r2 (line1: ls) = filter (validIntersept r1 r2 line1) ls ++  interseptsValid r1 r2 ls 