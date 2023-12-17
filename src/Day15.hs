module Day15 (day15a) where

import qualified Data.Map as M
import Data.List(foldl')
import Data.List.Split(splitOn)
import Debug.Trace(trace)

day15a :: IO ()
day15a = do
  input <- readFile "input/Day15.txt"
  let hashes = map hashStr $ parseStrs input
  
  print $  sum <$> sequenceA hashes

asciimap :: M.Map Char Integer
asciimap = M.fromList $ zip " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" [32 ..]

asciiCode :: Char -> Maybe Integer
asciiCode c = M.lookup c asciimap

hashChar :: Maybe Integer -> Char -> Maybe Integer
hashChar startval c = fmap ((`mod` 256) . (* 17) ) $(+) <$> startval<*>asciiCode c

hashStr :: String -> Maybe Integer
hashStr = foldl' hashChar (Just 0)

-- loading and parsing input
parseStrs :: [Char] -> [[Char]]
parseStrs = splitOn (",")