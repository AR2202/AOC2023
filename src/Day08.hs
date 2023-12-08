module Day08 (day8a, day8b) where

import Common (splitOnBlankLine)
import qualified Data.Map as M
import qualified Data.List as L

-- Types

type Network = M.Map String (String, String)

data Direction = L | R deriving (Show, Read, Eq)

type Instructions = [Direction]


-- reading Input
readInstructions :: [Char] -> Instructions
readInstructions = map (read . return)

readNetwork :: [String] -> Network
readNetwork strs = M.fromList $ map (\s -> (readSource s, (readTargetL s, readTargetR s))) strs
  where
    readSource = take 3
    readTargetL = take 3 . drop 7
    readTargetR = take 3 . drop 12

readDay8 :: IO (Instructions, Network)
readDay8  = do
  input <- splitOnBlankLine "Day08.txt"
  let instructions = readInstructions $ head input
  let network = readNetwork $ lines $ last input
  return (instructions, network)

exampleGraph :: Network
exampleGraph = M.fromList [("AAA", ("BBB", "CCC")), ("BBB", ("DDD", "EEE")), ("CCC", ("ZZZ", "GGG")), ("DDD", ("DDD", "DDD")), ("EEE", ("EEE", "EEE")), ("GGG", ("GGG", "GGG")), ("ZZZ", ("ZZZ", "ZZZ"))]

-- Part 1

day8a :: IO ()
day8a = do
  (instructions, network) <- readDay8
  let numsteps = findZZZ network instructions
  print numsteps



oneStep :: String -> Direction -> Network -> Maybe String
oneStep current L network = fst <$> M.lookup current network
oneStep current R network = snd <$> M.lookup current network

findZZZ :: Network -> Instructions -> Int
findZZZ network instructions = go 1 (Just "AAA") network (cycle instructions)
  where
    go _ Nothing _ _ = 0
    go _ _ _ [] = 0
    go steps (Just current) nw (x : xs) = case oneStep current x nw of
      Just "ZZZ" -> steps
      newNode -> go (steps + 1) newNode nw xs



-- Part2

day8b :: IO ()
day8b = do 
    (instructions, network) <- readDay8
    let nodesA = filter (L.isSuffixOf "A") $ M.keys network
    let nodesZ = filter (L.isSuffixOf "Z") $ M.keys network
    print nodesA
    print nodesZ
    let steps = map (a2z  network instructions) nodesA
    print $ L.foldl1' lcm  steps
    
    
a2z :: Num a => Network -> [Direction] -> String -> a
a2z network instructions startnode = go 1 (Just startnode) network (cycle instructions)
  where
    go _ Nothing _ _ = 0
    go _ _ _ [] = 0
    go steps (Just current) nw (x : xs) = case oneStep current x nw of
      Just node -> if "Z" `L.isSuffixOf` node then steps else go (steps + 1) (Just node) nw xs
      newNode -> go (steps + 1) newNode nw xs