module Day07 (day7a, day7b, Hand (..), Card (..), Face (..)) where

import qualified Data.List as L
import Data.Ord (comparing)

-- Types
data Face = T | J | Q | K | A deriving (Show, Read, Eq, Ord)

data Card = Num Int | Court Face deriving (Show, Eq, Ord)

data Hand = High [Card] | Pair [Card] | TwoPair [Card] | Three [Card] | FullHouse [Card] | Four [Card] | Five [Card] deriving (Show, Eq, Ord)

type CardBid = (Hand, Int)

data Face2 = T2 | Q2 | K2 | A2 deriving (Show, Read, Eq, Ord)

data Card2 = Joker | Num2 Int | Court2 Face2 deriving (Show, Eq, Ord)

data Hand2 = High2 [Card2] | Pair2 [Card2] | TwoPair2 [Card2] | Three2 [Card2] | FullHouse2 [Card2] | Four2 [Card2] | Five2 [Card2] deriving (Show, Eq, Ord)

type CardBid2 = (Hand2, Int)

day7a :: IO ()
day7a = do
  input <- readFile "input/Day07.txt"
  let cardbids = day7inp input
  let rankedHands = rankHands cardbids
  let winnings = zipWith (*) (map snd rankedHands) [1 ..]

  print $ sum winnings

readCard :: Char -> Card
readCard c
  | c `elem` "TJQKA" = Court (read [c])
  | otherwise = Num (read [c])

groupCards :: Ord a => [a] -> [[a]]
groupCards = L.group . L.sort

toHand :: [Card] -> Hand
toHand cardlist =
  case (maximum . map length . groupCards) cardlist of
    5 -> Five cardlist
    4 -> Four cardlist
    3 -> case (length . map length . groupCards) cardlist of
      2 -> FullHouse cardlist
      _ -> Three cardlist
    2 -> case length $ head $ tail $ reverse $ L.sortBy (comparing length) $ groupCards cardlist of
      2 -> TwoPair cardlist
      _ -> Pair cardlist
    1 -> High cardlist
    _ -> High cardlist

toCardBid :: [Char] -> CardBid
toCardBid s = (toHand $ map readCard $ L.takeWhile (/= ' ') s, read $ drop 1 $ dropWhile (/= ' ') s)

rankHands :: Ord a => [(a, b)] -> [(a, b)]
rankHands = L.sortBy (comparing fst)

-- read input
day7inp :: String -> [CardBid]
day7inp = map toCardBid . lines

-- Part 2
day7binp :: String -> [CardBid2]
day7binp = map toCardBidWJ . lines

day7b :: IO ()
day7b = do
  input <- readFile "input/Day07.txt"
  let cardbids = day7binp input
  let rankedHands = rankHands cardbids
  let winnings = zipWith (*) (map snd rankedHands) [1 ..]

  print $ sum winnings

readCard2 :: Char -> Card2
readCard2 c
  | c == 'J' = Joker
  | c `elem` "TQKA" = Court2 (read (c : "2"))
  | otherwise = Num2 (read [c])

toHandWJ :: [Card2] -> Hand2
toHandWJ cardlist =
  case (maximum . map length . groupCards) cardlist of
    5 -> Five2 cardlist
    4 -> if Joker `elem` cardlist then Five2 cardlist else Four2 cardlist
    3 -> case (length . map length . groupCards) cardlist of
      2 -> if Joker `elem` cardlist then Five2 cardlist else FullHouse2 cardlist
      _ -> if Joker `elem` cardlist then Four2 cardlist else Three2 cardlist
    2 -> case length $ head $ tail $ reverse $ L.sortBy (comparing length) $ groupCards cardlist of
      2 -> if Joker `elem` (last $ reverse $ L.sortBy (comparing length) $ groupCards cardlist) then FullHouse2 cardlist else if Joker `elem` cardlist then Four2 cardlist else TwoPair2 cardlist
      _ ->
        if Joker `elem` cardlist
          then Three2 cardlist
          else Pair2 cardlist
    1 -> if Joker `elem` cardlist then Pair2 cardlist else High2 cardlist
    _ -> High2 cardlist

toCardBidWJ :: [Char] -> CardBid2
toCardBidWJ s = (toHandWJ $ map readCard2 $ L.takeWhile (/= ' ') s, read $ drop 1 $ dropWhile (/= ' ') s)
