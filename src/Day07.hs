module Day07 (day7a, Hand (..), Card (..), Face (..)) where

import qualified Data.List as L
import Data.Ord (comparing)

-- Types
data Face = T | J | Q | K | A deriving (Show, Read, Eq, Ord)

data Card = Num Int | Court Face deriving (Show, Eq, Ord)


data Hand = High [Card] | Pair [Card] | TwoPair [Card] | Three [Card] | FullHouse [Card] | Four [Card] | Five [Card] deriving (Show, Eq, Ord)

type CardBid = (Hand, Int)

day7a = do
    input <- readFile "input/Day07.txt"
    let cardbids = day7inp input
    let rankedHands = rankHands cardbids
    let winnings = zipWith (*) (map snd rankedHands) [1..]

    print $ sum winnings

readCard :: Char -> Card
readCard c
  | c `elem` "TJQKA" = Court (read [c])
  | otherwise = Num (read [c])

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

rankHands :: [CardBid] -> [CardBid]
rankHands = L.sortBy (comparing fst)

-- read input
day7inp :: String -> [CardBid]
day7inp = map toCardBid . lines
