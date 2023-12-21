module Day21 (day21a) where

import Common (addCoordinates)
import qualified Data.Map as M
import qualified Data.Set as S

-- Part 1
day21a :: IO ()
day21a = do
  input <- readFile "input/Day21.txt"
  let withCoord = addCoordinates $ lines input
  let start = (fst . head . filter ((== 'S') . snd)) withCoord
  let coordmap = M.fromList withCoord
  let steps = nSteps 64 coordmap (S.singleton start)
  print $ length steps

emptyNeighbours :: (Int, Int) -> M.Map (Int, Int) Char -> [(Int, Int)]
emptyNeighbours (x, y) coordmap = filter (\coord -> M.lookup coord coordmap `elem` [Just '.', Just 'S']) [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]

nextFields :: M.Map (Int, Int) Char -> S.Set (Int, Int) -> S.Set (Int, Int)
nextFields coordmap coordset = S.unions $ S.map (S.fromList . (`emptyNeighbours` coordmap)) coordset

nSteps :: Int -> M.Map (Int, Int) Char -> S.Set (Int, Int) -> S.Set (Int, Int)
nSteps n coordmap start = iterate (nextFields coordmap) start !! n
