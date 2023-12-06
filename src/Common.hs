{-# LANGUAGE OverloadedStrings #-}

module Common
  ( splitOnBlankLine,
    splitOnBlankSplitAndRead,
    dir,
    filepath,
    loadInput,
    splitLines,
    splitLinesAndWords,
    list2tuple,
    readTuple,
    splitCommas,
    capitalize,
    list2triple,
    makeCoordinates,
    addCoordinates,
    loadAndAddCoords,
    readUserName',
    main2,
    loadApplyPrint,
    neighbors,
    rstrip,
    lstrip,
    addCoordinatest,
    addCoordinatesswap
  )
where

import Data.Char (toUpper)
import Data.List (transpose, isSuffixOf, isPrefixOf)
import Data.List.Split
import Control.Monad.Trans.Maybe
import GHC.IO.Exception (IOException(ioe_filename))

main2 :: IO ()
main2 = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName'
    
    return usr
  case maybeCreds of
    Nothing -> print "Couldn't login!"
    Just u -> print u

readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  putStrLn "Please enter your Username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

dir :: String
dir = "input/"

filepath :: String -> String
filepath filename = dir ++ filename

loadInput :: String -> IO String
loadInput filename = readFile $ filepath filename

splitOnBlankLine :: String -> IO [String]
splitOnBlankLine filename = splitOn "\n\n" <$> loadInput filename

splitOnBlankSplitAndRead :: String -> IO [[Int]]
splitOnBlankSplitAndRead filename = do
  textblocks <- splitOnBlankLine filename
  let linesInBlocks = map lines textblocks
  let converted = (map . map) read linesInBlocks
  return converted

splitLines :: String -> IO [String]
splitLines filename = lines <$> loadInput filename

splitLinesAndWords :: String -> IO [[String]]
splitLinesAndWords filename = map words <$> splitLines filename

-- partial function - will fail on lists with less than 2 elements
list2tuple :: [a] -> (a, a)
list2tuple (x : y : zs) = (x, y)

list2triple :: [a] -> (a, a, a)
list2triple (x : y : z : zs) = (x, y, z)

readTuple :: (Read a, Read b) => (String, String) -> (a, b)
readTuple (x, y) = (read x, read y)

splitCommas :: String -> IO [[String]]
splitCommas filename = map (splitOn ",") . lines <$> loadInput filename

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)
makeCoordinatesSwap :: Int -> Int -> [(Int, Int)]
makeCoordinatesSwap x y  = map swap $ makeCoordinates x y 
addCoordinates :: [String] -> [((Int, Int), Char)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

addCoordinatest :: [String] -> [((Int, Int), Char)]
addCoordinatest ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = concat  ls
addCoordinatesswap :: [String] -> [((Int, Int), Char)]
addCoordinatesswap ls = zip coordlist chars
  where
    coordlist = makeCoordinatesSwap ((length . head) ls) (length ls)
    chars = concat  ls
loadAndAddCoords :: String -> IO [((Int, Int), Char)]
loadAndAddCoords filename = addCoordinates <$> splitLines filename



loadApplyPrint :: Show a => FilePath -> ([String] -> a) -> IO ()
loadApplyPrint filep f = do
  input <- readFile filep
  let linesInput = lines input 
  print $ f linesInput

neighbors :: (Num a, Num b, Enum a, Enum b) => (a, b) -> [(a, b)]
neighbors (x,y) = [(x1,y1)|x1<-[x-1..x+1],y1<-[y-1..y+1]]

rstrip :: Eq a => [a] -> [a] -> [a]
rstrip c s =
  if c `isSuffixOf` s then init s else s 

lstrip :: Eq a => [a] -> [a] -> [a]
lstrip c s =
  if c `isPrefixOf` s then tail s else s 