module Day10 (day10a) where

import Common (addCoordinatesswap)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Set as S

-- Types
-- using H for horizontal pipe, V for Vertical pipe, T for 7
data Connector = L | J | S | V | H | F | T deriving (Show, Eq, Read)

data Direction = West | North | East | South deriving (Show, Eq)

type LoopMap = M.Map (Int, Int) Connector

enterFrom :: Direction -> Direction
enterFrom North = South
enterFrom South = North
enterFrom East = West
enterFrom West = East

connections :: Connector -> [Direction]
connections L = [North, East]
connections J = [North, West]
connections S = [North, East, South, West]
connections V = [North, South]
connections H = [East, West]
connections F = [South, East]
connections T = [South, West]

-- Prepare  input
toConnector :: Char -> Connector
toConnector '7' = T
toConnector '|' = V
toConnector '-' = H
toConnector c = read $ return c

makeMap :: [String] -> LoopMap
makeMap = fmap toConnector . M.fromList . filter ((/= '.') . snd) . addCoordinatesswap

-- Part 1
day10a :: IO ()
day10a = do
  input <- readFile "input/Day10.txt"
  let locmap = makeMap $ lines input
  let sloc = locateS locmap
  let steps = findFromS locmap sloc (connections S)
  print steps

moveDirection :: (Int, Int) -> Direction -> (Int, Int)
moveDirection (x, y) North = (x, y - 1)
moveDirection (x, y) South = (x, y + 1)
moveDirection (x, y) West = (x - 1, y)
moveDirection (x, y) East = (x + 1, y)

findLoop :: LoopMap -> S.Set (Int, Int) -> Int -> (Int, Int) -> Direction -> Maybe Int
findLoop locationmap visited steps location direction
  | isNothing (M.lookup location locationmap) = Nothing
  | direction `notElem` entrances = Nothing
  | M.lookup location locationmap == Just S = Just (ceiling (fromIntegral steps / 2))
  | S.member location visited = Nothing
  | otherwise = findLoop locationmap (S.insert location visited) (steps + 1) newLocation newDirection
  where
    entrances = (map enterFrom . connections) (M.findWithDefault S location locationmap)
    newDirection = head $ filter (/= enterFrom direction) $ connections $ M.findWithDefault S location locationmap
    newLocation = moveDirection location newDirection

findFromS :: LoopMap -> (Int, Int) -> [Direction] -> Int
findFromS locationmap sloc [] = 0
findFromS locationmap sloc (x : xs) = case findLoop locationmap S.empty 1 (moveDirection sloc x) x of
  Just steps -> steps
  Nothing -> findFromS locationmap sloc xs

locateS m = (fst . head) $ filter ((== S) . snd) $ M.toList m
