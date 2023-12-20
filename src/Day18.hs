module Day18 (day18aTest, day18a) where

import Common (lstrip, rstrip)
import qualified Data.IntMap as IM
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (uncurry3)

-- Types
type Color = String

type Coord = (Int, Int)

data Direction = R | L | D | U deriving (Show, Read, Eq)

type Colormap = M.Map Coord Color

type DigMap = IM.IntMap [Int]

data DigState = DigState {_currPos :: Coord, _colorMap :: Colormap, _digMap :: DigMap, _yMap :: DigMap} deriving (Show, Read, Eq)

day18a :: IO ()
day18a = do
  input <- readFile "input/Day18.txt"
  let digInst = parseDigInstructions input
  let digstate = followDigMap digInst

  print $length $allInternal(_digMap digstate) (_yMap digstate)

initialDigState :: DigState
initialDigState = DigState (0, 0) M.empty (IM.singleton 0 [0]) (IM.singleton 0 [0])

day18aTest :: DigState
day18aTest = dig R 6 "#70c710" initialDigState

dig :: Direction -> Int -> String -> DigState -> DigState
dig direction steps color digstate = digstate {_currPos = updatePosition (_currPos digstate) direction steps, _colorMap = addColor (_currPos digstate) direction steps color (_colorMap digstate), _digMap = addCoords (_currPos digstate) direction steps (_digMap digstate), _yMap = addCoordsY (_currPos digstate) direction steps (_yMap digstate)}

addCoords (currX, currY) U steps digmap = IM.insertWith (++) currX [currY + 1 .. currY + steps] digmap
addCoords (currX, currY) D steps digmap = IM.insertWith (++) currX [currY - steps .. currY - 1] digmap
addCoords (currX, currY) R steps digmap = IM.unionWith (++) (IM.fromList [(x, [currY]) | x <- [currX + 1 .. currX + steps]]) digmap
addCoords (currX, currY) L steps digmap = IM.unionWith (++) (IM.fromList [(x, [currY]) | x <- [currX - steps .. currX - 1]]) digmap

addCoordsY :: (IM.Key, IM.Key) -> Direction -> IM.Key -> IM.IntMap [IM.Key] -> IM.IntMap [IM.Key]
addCoordsY (x, y) D = addCoords (y, x) L
addCoordsY (x, y) L = addCoords (y, x) D
addCoordsY (x, y) R = addCoords (y, x) U
addCoordsY (x, y) U = addCoords (y, x) R

addColor :: Coord -> Direction -> Int -> String -> Colormap -> Colormap
addColor (currX, currY) U steps color colormap = M.union (M.fromList [((currX, y), color) | y <- [currY + 1 .. currY + steps]]) colormap
addColor (currX, currY) D steps color colormap = M.union (M.fromList [((currX, y), color) | y <- [currY - 1 .. currY - steps]]) colormap
addColor (currX, currY) R steps color colormap = M.union (M.fromList [((x, currY), color) | x <- [currX + 1 .. currX + steps]]) colormap
addColor (currX, currY) L steps color colormap = M.union (M.fromList [((x, currY), color) | x <- [currX - 1 .. currX - steps]]) colormap

updatePosition :: Coord -> Direction -> Int -> Coord
updatePosition (currX, currY) U steps = (currX, currY + steps)
updatePosition (currX, currY) D steps = (currX, currY - steps)
updatePosition (currX, currY) R steps = (currX + steps, currY)
updatePosition (currX, currY) L steps = (currX - steps, currY)

followDigMap diglist = foldl' (flip (uncurry3 dig)) initialDigState diglist

toDigInstructions :: [String] -> (Direction, Int, [Char])
toDigInstructions (x : y : z : zs) = (read x, read y, (rstrip ")" . lstrip "(") y)

parseDigInstructions :: String -> [(Direction, Int, [Char])]
parseDigInstructions = map (toDigInstructions . words) . lines

isInside :: (IM.Key, IM.Key) -> IM.IntMap [IM.Key] -> IM.IntMap [IM.Key] -> Bool
isInside (x, y) xmap ymap = x <= xmax && x >= xmin && y <= ymax && y >= ymin
  where
    xmax = maximum $ IM.findWithDefault [] y ymap
    xmin = minimum $ IM.findWithDefault [] y ymap
    ymax = maximum $ IM.findWithDefault [] x xmap
    ymin = minimum $ IM.findWithDefault [] x xmap

minKey intmap = head $ IM.keys intmap

maxKey intmap = last $ IM.keys intmap

allInternal xmap ymap = filter (\coord -> isInside coord xmap ymap) $ [(x, y) | x <- [minKey xmap .. maxKey xmap], y <- [minKey ymap .. maxKey ymap]]
