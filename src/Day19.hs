{-# LANGUAGE OverloadedStrings #-}

module Day19 (test19a, testParsingLT, testParsingGT, testParsingField, testParsingRule, testPArsingRules, testParsingWorkflow, testParsingPart, parseWorkflows, day19a) where

import Common (splitOnBlankLine)
import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

data XMAS = X | M | A | S deriving (Show, Read, Eq)

data Action = Accept | Reject | PassTo String deriving (Show, Eq)

data Rule = Rule {_partProperty :: Part -> Int, _condition :: Int -> Bool, _ontrue :: Action}

type Workflow = [Rule]

type Workflows = M.Map String Workflow

data Part = Part {_x :: Int, _m :: Int, _a :: Int, _s :: Int} deriving (Show, Read, Eq)

data PartConstraints = PartConstraints {_xmin :: Int, _xmax :: Int, _mmin:: Int, _mmax :: Int, _amin:: Int, _amax :: Int, _smin :: Int, _smax :: Int}deriving (Show, Read, Eq)

instance Show Rule where
  show r = "Rule { " ++ "_partProperty:" ++ show1 (_partProperty r) ++ ",_condition:" ++ show2 (_condition r) ++ ",_ontrue:" ++ show (_ontrue r) ++ "}"
    where
      show2 _ = "Int->Bool"
      show1 _ = "Part->Int"

-- Part 1

day19a = do
  inputs <- splitOnBlankLine "Day19.txt"
  let workflows = parseWorkflows $ head inputs
  let parts = parseParts $ last inputs
  let accepted = filterAccepted <$> workflows <*> parts
  let ratingNumbers = traverse (map ratingNumber) accepted
  let sumRatings = sum <$> sequenceA ratingNumbers
  print sumRatings

applyWorkflow :: Part -> Workflow -> Action
applyWorkflow _ [] = Reject
applyWorkflow part (rule : workflow) = if (_condition rule . _partProperty rule) part then _ontrue rule else applyWorkflow part workflow

applyAllWorkflows :: Part -> Workflows -> Action
applyAllWorkflows part workflows = go part workflows "in"
  where
    go p wfs s = case M.lookup s wfs of
      Nothing -> Reject
      Just wf -> case applyWorkflow p wf of
        PassTo news -> go p wfs news
        x -> x

ratingNumber :: Part -> Int
ratingNumber p = _x p + _m p + _a p + _s p

filterAccepted workflows = filter (\part -> applyAllWorkflows part workflows == Accept)

-- Parsing

ruleParser :: Parser Rule
ruleParser = Rule <$> fieldParser <*> conditionParser <*> actionParser <* (char ',')

lessThanParser :: Parser (Int -> Bool)
lessThanParser = (>) . read <$> ((char '<') *> many1 digit)

greaterThanParser :: Parser (Int -> Bool)
greaterThanParser = (<) . read <$> ((char '>') *> many1 digit)

conditionParser :: Parser (Int -> Bool)
conditionParser = try lessThanParser <|> greaterThanParser

fieldParser :: Parser (Part -> Int)
fieldParser = determineField <$> oneOf "xmas"

determineField 'x' = _x
determineField 'm' = _m
determineField 'a' = _a
determineField 's' = _s

actionParser :: Parser Action
actionParser = determineAction <$> ((char ':') *> many1 alphaNum)

unconditionalParser :: Parser Rule
unconditionalParser = (Rule _x (const True)) . determineAction <$> many1 alphaNum

determineAction "A" = Accept
determineAction "R" = Reject
determineAction str = PassTo str

rulesParser :: Parser Workflow
rulesParser = between (char '{') (char '}') (many (try ruleParser <|> unconditionalParser))

nameParser :: Parser String
nameParser = many1 alphaNum

workflowParser :: Parser (String, Workflow)
workflowParser = (,) <$> nameParser <*> rulesParser

xParser :: Parser Int
xParser = read <$> between (string "{x=") (char ',') (many1 digit)

mParser :: Parser Int
mParser = read <$> between (string "m=") (char ',') (many1 digit)

aParser :: Parser Int
aParser = read <$> between (string "a=") (char ',') (many1 digit)

sParser :: Parser Int
sParser = read <$> between (string "s=") (char '}') (many1 digit)

partParser :: Parser Part
partParser = Part <$> xParser <*> mParser <*> aParser <*> sParser

parseWorkflows str = M.fromList <$> traverse (parse workflowParser "text" . T.pack) (lines str)

parseParts str = traverse (parse partParser "text" . T.pack) (lines str)
-- Part 2
sortPartsOneWorkflow :: Workflow -> PartConstraints -> [(Action, PartConstraints)]
sortPartsOneWorkflow (rule:rules) parts = undefined
-- Test Cases
test19a = applyAllWorkflows testPart1 testmap

testRule1 = Rule _s (< 1351) (PassTo "px")

testRule2 = Rule _x (const True) (PassTo "qqz")

testRule3 = Rule _s (> 2270) (PassTo "qs")

testRule4 = Rule _m (< 1801) (PassTo "hdj")

testRule5 = Rule _x (const True) Reject

testRule6 = Rule _s (> 3448) Accept

testRule7 = Rule _x (const True) (PassTo "lnx")

testRule8 = Rule _m (const True) Accept

testWorkflowIn = [testRule1, testRule2]

testWorkFlowqqz = [testRule3, testRule4, testRule5]

testWorkFlowqs = [testRule6, testRule7]

testWorkFlowlnx = [testRule8]

testPart1 = Part 787 2655 1222 2876

testmap = M.fromList [("in", testWorkflowIn), ("qqz", testWorkFlowqqz), ("qs", testWorkFlowqs), ("lnx", testWorkFlowlnx)]

testParsingLT = fmap ($ 400) (parse conditionParser "text" "<500")

testParsingGT = fmap ($ 400) (parse conditionParser "text" ">500")

testParsingField = fmap ($ testPart1) (parse fieldParser "text" "x")

testParsingRule = parse ruleParser "text" "s<1351:px,"

testPArsingRules = parse rulesParser "text" "{a<2006:qkq,m>2090:A,rfg}"

testParsingWorkflow = parse workflowParser "text" "px{a<2006:qkq,m>2090:A,rfg}"

testParsingPart = parse partParser "test" "{x=787,m=2655,a=1222,s=2876}"
