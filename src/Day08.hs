module Day08 (day8a) where

import qualified Data.Map as M
import Common (splitOnBlankLine)

-- Types

type Network = M.Map String (String, String)

data Direction = L | R deriving (Show, Read, Eq)

type Instructions = [Direction]



-- Part 1
exampleGraph :: Network
exampleGraph = M.fromList [("AAA", ("BBB", "CCC")), ("BBB", ("DDD", "EEE")), ("CCC", ("ZZZ", "GGG")), ("DDD", ("DDD", "DDD")), ("EEE", ("EEE", "EEE")), ("GGG", ("GGG", "GGG")), ("ZZZ", ("ZZZ", "ZZZ"))]

oneStep :: String-> Direction -> Network-> Maybe String
oneStep current L network =  fst <$>M.lookup current network
oneStep current R network =  snd <$>M.lookup current network

findZZZ :: Network -> Instructions -> Int
findZZZ network instructions = go 1 (Just "AAA") network (cycle instructions  ) 
    where 
        go _ Nothing _ _ = 0
        go _ _ _ [] =0        
        go steps (Just current) nw (x:xs) = case oneStep current x nw of 
                Just "ZZZ" -> steps 
                newNode -> go (steps + 1) newNode nw xs

day8a = do
    input<- splitOnBlankLine "Day08.txt"
    let instructions = readInstructions $ head input
    let network = readNetwork $ lines $ last input
    let numsteps = findZZZ network instructions
    print numsteps

-- reading Input 
readInstructions :: [Char] -> Instructions
readInstructions = map (read . return)

readNetwork lines = M.fromList $ map (\s->(readSource s, (readTargetL s, readTargetR s) )) lines
    where readSource s = take 3 s 
          readTargetL s = take 3$drop 7 s
          readTargetR s = take 3 $ drop 12 s 