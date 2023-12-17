module Day16 (day16a, day16b) where

import Common (addCoordinates)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace(trace)

-- Types

data Direction = Up | Down | L | R deriving (Show, Eq, Ord)

day16b :: IO ()
day16b = do
    let startsL = zip (zip (repeat 0)[1..110]) (repeat R)
    let startsR = zip (zip (repeat 111)[1..110]) (repeat L)
    let startsU = zip (zip [1..110](repeat 0)) (repeat Down)
    let startsD = zip (zip [1..110](repeat 111)) (repeat Up)
    let starts = startsL ++ startsR ++ startsU ++ startsD
    day16 starts >>= print

day16a :: IO ()
day16a = day16 [((0,1),R)]>>= print



day16 ::[ ((Int, Int), Direction)] -> IO Int
day16 starts = do
  input <- readFile "input/Day16.txt"
  let withCoords = makeCoordMap input
  let enter = map (oneMove withCoords) starts
  let energized = map (allBeamsMove withCoords  S.empty )enter
  -- print energized
  return $maximum$ map length energized

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


allBeamsMove m energized []  = S.map fst energized
allBeamsMove m  energized beams = allBeamsMove m (S.union energized $ S.fromList beams)(concatMap (oneMove m) (filter (not . (`S.member` energized)) beams)) 
