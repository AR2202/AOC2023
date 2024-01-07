{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Day20 (pushNEx1, pushNEx2, testParseFlipFlop, testParseConjunction, testParseBroadcaster, day20a) where

import Control.Lens
import Data.Bifunctor (bimap)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.IO.Handle (NewlineMode (inputNL))
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

data PulseType = High | Low deriving (Show, Read, Eq)

data Switch = On | Off | Push deriving (Show, Read, Eq)

type Connections = [String]

type Module = Either FlipFlop Conjunction

type PulseMap = M.Map String PulseType

type ModuleMap = M.Map String Module

data FlipFlop = FlipFlop {_nameF :: String, _state :: Switch, _connections :: Connections} deriving (Show, Read, Eq)

data Conjunction = Conjunction {_nameC :: String, _inputs :: PulseMap, _outputs :: Connections} deriving (Show, Read, Eq)

makeLenses ''Conjunction
makeLenses ''FlipFlop

-- Part 1

day20a :: IO ()
day20a = do
  input <- readFile "input/Day20.txt"
  let inputlines = map T.pack $ lines input
  let modules = moduleListParser inputlines
  let modulemap = insertOutputModules . insertAllInputs . makeModuleMap <$> modules
  print modulemap
  print $ pushNTimes 1000 <$> modulemap
  let result = hightimeslow . pushNTimes 1000 <$> modulemap
  print result

hightimeslow (x, y, z) = y * z

switch :: Switch -> Switch
switch On = Off
switch Off = On
switch Push = Push

switchPulse :: Switch -> PulseType
switchPulse On = High
switchPulse Off = Low
switchPulse Push = Low

switchState :: FlipFlop -> FlipFlop
-- switchState ff = ff {_state = switch (_state ff)}
switchState = over state switch

receive :: Module -> PulseType -> String -> (Module, Maybe PulseType)
receive (Left flipFlop) High _ = (Left flipFlop, Nothing)
receive (Left flipFlop) Low _ = (Left (switchState flipFlop), Just (send (Left (switchState flipFlop))))
receive (Right conjunction) pulsetype str = (Right (over inputs (M.insert str pulsetype) conjunction), Just (send (Right (over inputs (M.insert str pulsetype) conjunction))))

send :: Module -> PulseType
send (Left flipflop) = switchPulse (_state flipflop)
send (Right conjunction) = if all (== High) (M.elems (_inputs conjunction)) then Low else High

extractOutputs :: Module -> Connections
extractOutputs (Left flipflop) = _connections flipflop
extractOutputs (Right conjunction) = _outputs conjunction

extractName :: Module -> String
extractName (Left flipflop) = _nameF flipflop
extractName (Right conjunction) = _nameC conjunction

insertModule :: M.Map String Module -> Module -> M.Map String Module
insertModule modulemap m = M.insert (extractName m) m modulemap

handlePulses :: [(Module, Maybe PulseType)] -> (M.Map String Module, Int, Int) -> (M.Map String Module, Int, Int)
handlePulses [] modulemapcnts = modulemapcnts
handlePulses ((m, Nothing) : pulses) modulemapcnts = handlePulses pulses modulemapcnts
handlePulses ((m, Just pulse) : pulses) (modulemap, highs, lows) = handlePulses (pulses ++ receivers) (addPulseType (foldl' insertModule modulemap (map fst receivers), highs, lows) pulse (length (extractOutputs m)))
  where
    receivers = map (\receiver -> receive (fromJust (M.lookup receiver modulemap)) pulse (extractName m)) (extractOutputs m)

addPulseType (x, highs, lows) High n = (x, highs + n, lows)
addPulseType (x, highs, lows) Low n = (x, highs, lows + n)

broadcaster :: Connections -> FlipFlop
broadcaster = FlipFlop "broadcaster" Push

bcdefault :: Module
bcdefault = Left $ broadcaster []

-- pushButton :: M.Map String Module -> M.Map String Module
pushButton (modulemap, h, l) = handlePulses [(M.findWithDefault bcdefault "broadcaster" modulemap, Just Low)] (modulemap, h, l)

pushNTimes :: Int -> M.Map String Module -> (M.Map String Module, Int, Int)
pushNTimes n modulemap = iterate pushButton (modulemap, 0, n) !! n

---Examples----
example1Map :: ModuleMap
example1Map = M.fromList [("broadcaster", Left $ broadcaster ["a", "b", "c"]), ("a", Left $ FlipFlop "a" Off ["b"]), ("b", Left $ FlipFlop "b" Off ["c"]), ("c", Left $ FlipFlop "c" Off ["inv"]), ("inv", Right $Conjunction "inv" (M.singleton "c" Low) ["a"])]

example2Map :: ModuleMap
example2Map = M.fromList [("broadcaster", Left $ broadcaster ["a"]), ("a", Left $ FlipFlop "a" Off ["inv", "con"]), ("b", Left $ FlipFlop "b" Off ["con"]), ("inv", Right $Conjunction "inv" (M.singleton "a" Low) ["b"]), ("con", Right $Conjunction "con" (M.fromList [("a", Low), ("b", Low)]) ["output"]), ("output", Left $ FlipFlop "output" Push [])]

pushNEx1 n = pushNTimes n example1Map

pushNEx2 n = pushNTimes n example2Map

-- Parsing
makeModuleMap modules = M.fromList [(extractName m, m) | m <- modules]

nameParser :: Parser String
nameParser = spaces *> many1 alphaNum

connectionParser :: Parser String
connectionParser = spaces *> many1 alphaNum <* char ','

parseAsFlipFlop :: String -> Connections -> Module
parseAsFlipFlop name connections = Left $ FlipFlop name Off connections

-- inputs need to be inserted separately
parseAsConjunction :: String -> Connections -> Module
parseAsConjunction name connections = Right $ Conjunction name M.empty connections

flipFlopParser :: Parser Module
flipFlopParser = moduleParser parseAsFlipFlop '%'

conjunctionParser :: Parser Module
conjunctionParser = moduleParser parseAsConjunction '&'

moduleParser f c = f <$> (char c *> nameParser <* spaces) <*> (string "->" *> spaces *> many1 (try connectionParser <|> nameParser))

broadcasterParser = Left . broadcaster <$> (string "broadcaster ->" *> spaces *> many1 (try connectionParser <|> nameParser))

anyModuleParser = try conjunctionParser <|> try flipFlopParser <|> broadcasterParser

moduleListParser :: [T.Text] -> Either ParseError [Module]
moduleListParser = traverse (parse anyModuleParser "input")

testParseFlipFlop = parse flipFlopParser "test" "%a -> inv, con"

testParseConjunction = parse conjunctionParser "test" "&con -> output"

testParseBroadcaster = parse broadcasterParser "test" "broadcaster -> a, b, c"

insertInput inp = over inputs (M.insert inp Low)

insertInputInMap inp key = M.adjust (bimap id (insertInput inp)) key

inputsFromOutputs m = [(extractName m, out) | out <- extractOutputs m]

inputOutputList modulemap = concatMap inputsFromOutputs $ M.elems modulemap

insertAllInputs :: M.Map String Module -> M.Map String Module
insertAllInputs modulemap = foldl' (flip (uncurry insertInputInMap)) modulemap (inputOutputList modulemap)

insertOutputModules modulemap = foldl' (flip (uncurry (M.insertWith (const id)))) modulemap (concatMap (map createOutputModule . extractOutputs) $ M.elems modulemap)

createOutputModule s = (s, Left $ FlipFlop s Push [])
