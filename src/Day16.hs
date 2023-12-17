module Day16 (day16a) where

import Common (addCoordinates)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace(trace)

-- Types

data Direction = Up | Down | L | R deriving (Show, Eq, Ord)

day16a :: IO ()
day16a = do
  input <- readFile "input/Day16.txt"
  let withCoords = makeCoordMap input
  let energized = allBeamsMove withCoords [((1, 1), Down)] S.empty
  -- print energized
  print $ length energized

move :: (Num a1, Num a2) => Direction -> (a1, a2) -> (a1, a2)
move R (x, y) = (x + 1, y)
move L (x, y) = (x - 1, y)
move Up (x, y) = (x, y - 1)
move Down (x, y) = (x, y + 1)

makeCoordMap :: String -> M.Map (Int, Int) Char
makeCoordMap input = M.fromList $ addCoordinates $ lines input

splitBeam :: Char -> Direction -> [Direction]
splitBeam '|' Up = [Up]
splitBeam '|' Down = [Down]
splitBeam '|' _ = [Up, Down]
splitBeam '-' L = [L]
splitBeam '-' R = [R]
splitBeam '-' _ = [L, R]
splitBeam '/' R = [Up]
splitBeam '/' L = [Down]
splitBeam '/' Up = [R]
splitBeam '/' Down = [L]
splitBeam '\\' R = [Down]
splitBeam '\\' L = [Up]
splitBeam '\\' Up = [L]
splitBeam '\\' Down = [R]
splitBeam '.' x = [x]

oneMove :: M.Map (Int, Int) Char -> ((Int, Int), Direction) -> [((Int, Int), Direction)]
oneMove m (coord, direction) =  zip (repeat newCoord) newDirections
  where
    newCoord = move direction coord
    newDirections = case M.lookup newCoord m of
      Nothing -> []
      Just symb -> splitBeam symb direction

allBeamsMove :: M.Map (Int, Int) Char -> [((Int, Int), Direction)] -> S.Set ((Int, Int), Direction) -> S.Set (Int, Int)
allBeamsMove m [] energized = S.map fst energized
allBeamsMove m beams energized = allBeamsMove m (concatMap (oneMove m) (filter (not . (`S.member` energized)) beams)) (S.union energized $ S.fromList beams)
