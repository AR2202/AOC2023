{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Day19 (test19a, testParsingLT, testParsingGT, testParsingField, testParsingRule, testPArsingRules, testParsingWorkflow, testParsingPart, parseWorkflows, day19a, day19b) where

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

data Rule2 = Rule2 {_partProperty2 :: Char, _condition2 :: Int -> Int -> Bool, _testnum :: Int, _ontrue2 :: Action}

type Workflow = [Rule]

type Workflow2 = [Rule2]

type Workflows = M.Map String Workflow

type Workflows2 = M.Map String Workflow2

data Part = Part {_x :: Int, _m :: Int, _a :: Int, _s :: Int} deriving (Show, Read, Eq)

data PartConstraints = PartConstraints {_xmin :: Int, _xmax :: Int, _mmin :: Int, _mmax :: Int, _amin :: Int, _amax :: Int, _smin :: Int, _smax :: Int} deriving (Show, Read, Eq)

instance Show Rule where
  show :: Rule -> String
  show r = "Rule { " ++ "_partProperty:" ++ show1 (_partProperty r) ++ ",_condition:" ++ show2 (_condition r) ++ ",_ontrue:" ++ show (_ontrue r) ++ "}"
    where
      show2 _ = "Int->Bool"
      show1 _ = "Part->Int"

instance Show Rule2 where
  show :: Rule2 -> String
  show r = "Rule2 { " ++ "_partProperty2:" ++ show1 (_partProperty2 r) ++ ",_condition2:" ++ show2 (_condition2 r) ++ ", _testnum:" ++ show (_testnum r) ++ ",_ontrue2:" ++ show (_ontrue2 r) ++ "}"
    where
      show2 _ = "Int->Int->Bool"
      show1 _ = "Part->Int"

-- Part 1

day19a :: IO ()
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

filterAccepted :: Workflows -> [Part] -> [Part]
filterAccepted workflows = filter (\part -> applyAllWorkflows part workflows == Accept)

-- Parsing

ruleParser :: Parser Rule
ruleParser = Rule <$> fieldParser <*> conditionParser <*> actionParser <* (char ',')

ruleParser2 :: Parser Rule2
ruleParser2 = Rule2 <$> fieldNameParser <*> conditionParser2 <*> numParser <*> actionParser <* (char ',')

lessThanParser :: Parser (Int -> Bool)
lessThanParser = (>) . read <$> ((char '<') *> many1 digit)

greaterThanParser :: Parser (Int -> Bool)
greaterThanParser = (<) . read <$> ((char '>') *> many1 digit)

conditionParser :: Parser (Int -> Bool)
conditionParser = try lessThanParser <|> greaterThanParser

lessThanParser2 :: Parser (Int -> Int -> Bool)
lessThanParser2 = (>) <$ (char '<')

greaterThanParser2 :: Parser (Int -> Int -> Bool)
greaterThanParser2 = (<) <$ (char '>')

conditionParser2 :: Parser (Int -> Int -> Bool)
conditionParser2 = try lessThanParser2 <|> greaterThanParser2

numParser :: Parser Int
numParser = read <$> many1 digit

fieldParser :: Parser (Part -> Int)
fieldParser = determineField <$> oneOf "xmas"

fieldNameParser :: Parser Char
fieldNameParser = oneOf "xmas"

determineField :: Char -> Part -> Int
determineField 'x' = _x
determineField 'm' = _m
determineField 'a' = _a
determineField 's' = _s

actionParser :: Parser Action
actionParser = determineAction <$> ((char ':') *> many1 alphaNum)

unconditionalParser :: Parser Rule
unconditionalParser = (Rule _x (const True)) . determineAction <$> many1 alphaNum

unconditionalParser2 :: Parser Rule2
unconditionalParser2 = (Rule2 'x' ((const . const) True) 0) . determineAction <$> many1 alphaNum

determineAction :: [Char] -> Action
determineAction "A" = Accept
determineAction "R" = Reject
determineAction str = PassTo str

rulesParser :: Parser Workflow
rulesParser = between (char '{') (char '}') (many (try ruleParser <|> unconditionalParser))

rulesParser2 :: Parser Workflow2
rulesParser2 = between (char '{') (char '}') (many (try ruleParser2 <|> unconditionalParser2))

nameParser :: Parser String
nameParser = many1 alphaNum

workflowParser :: Parser (String, Workflow)
workflowParser = (,) <$> nameParser <*> rulesParser

workflowParser2 :: Parser (String, Workflow2)
workflowParser2 = (,) <$> nameParser <*> rulesParser2

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

parseWorkflows :: String -> Either ParseError (M.Map String Workflow)
parseWorkflows str = M.fromList <$> traverse (parse workflowParser "text" . T.pack) (lines str)

parseWorkflows2 :: String -> Either ParseError (M.Map String Workflow2)
parseWorkflows2 str = M.fromList <$> traverse (parse workflowParser2 "text" . T.pack) (lines str)

parseParts :: String -> Either ParseError [Part]
parseParts str = traverse (parse partParser "text" . T.pack) (lines str)

-- Part 2
day19b :: IO ()
day19b = do
  inputs <- splitOnBlankLine "Day19.txt"
  let workflows = parseWorkflows2 $ head inputs
  let sorted = sortParts <$> workflows
  print $ sum <$> sorted

sortPartsOneWorkflow :: Workflow2 -> PartConstraints -> [(Action, PartConstraints)]
sortPartsOneWorkflow (rule : rules) parts
  | (_condition2 rule) (_testnum rule) (testmin parts) && (_condition2 rule) (_testnum rule) (testmax parts) = [(_ontrue2 rule, parts)]
  | not ((_condition2 rule) (_testnum rule) (testmin parts)) && not ((_condition2 rule) (_testnum rule) (testmax parts)) = sortPartsOneWorkflow rules parts
  | (_condition2 rule) (_testnum rule) (testmin parts) && not ((_condition2 rule) (_testnum rule) (testmax parts)) = (_ontrue2 rule, setFieldMax (_testnum rule - 1) parts) : sortPartsOneWorkflow rules (setFieldMin (_testnum rule) parts)
  | not ((_condition2 rule) (_testnum rule) (testmin parts)) && (_condition2 rule) (_testnum rule) (testmax parts) = (_ontrue2 rule, setFieldMin (_testnum rule + 1) parts) : sortPartsOneWorkflow rules (setFieldMax (_testnum rule) parts)
  where
    testmin
      | _partProperty2 rule == 'x' = _xmin
      | _partProperty2 rule == 'm' = _mmin
      | _partProperty2 rule == 'a' = _amin
      | _partProperty2 rule == 's' = _smin
    testmax
      | _partProperty2 rule == 'x' = _xmax
      | _partProperty2 rule == 'm' = _mmax
      | _partProperty2 rule == 'a' = _amax
      | _partProperty2 rule == 's' = _smax
    setFieldMin
      | _partProperty2 rule == 'x' = setXmin
      | _partProperty2 rule == 'm' = setMmin
      | _partProperty2 rule == 'a' = setAmin
      | _partProperty2 rule == 's' = setSmin
    setFieldMax
      | _partProperty2 rule == 'x' = setXmax
      | _partProperty2 rule == 'm' = setMmax
      | _partProperty2 rule == 'a' = setAmax
      | _partProperty2 rule == 's' = setSmax
    setXmin x rule' = rule' {_xmin = x}
    setXmax x rule' = rule' {_xmax = x}
    setMmin x rule' = rule' {_mmin = x}
    setMmax x rule' = rule' {_mmax = x}
    setAmin x rule' = rule' {_amin = x}
    setAmax x rule' = rule' {_amax = x}
    setSmin x rule' = rule' {_smin = x}
    setSmax x rule' = rule' {_smax = x}

performAction :: Workflows2 -> (Action, PartConstraints) -> [(Action, PartConstraints)]
performAction _ (Accept, x) = [(Accept, x)]
performAction _ (Reject, _) = []
performAction workflows (PassTo w, x) = case M.lookup w workflows of
  Nothing -> []
  Just workflow -> sortPartsOneWorkflow workflow x

initialRanges :: PartConstraints
initialRanges = PartConstraints 1 4000 1 4000 1 4000 1 4000

sortParts :: M.Map String Workflow2 -> [Int]
sortParts workflowmap = go workflowmap [(PassTo "in", initialRanges)] []
  where
    go _ [] accepted = map (countParts . snd) accepted
    go workflows unsorted accepted = go workflows (newunsorted unsorted) (newaccepted unsorted ++ accepted)
    newunsorted unsorted' = filter ((/= Accept) . fst) (newactions unsorted')
    newaccepted unsorted' = filter ((== Accept) . fst) (newactions unsorted')
    newactions = concatMap (performAction workflowmap)

countParts :: PartConstraints -> Int
countParts parts = xvals * mvals * avals * svals
  where
    xvals = _xmax parts - _xmin parts + 1
    mvals = _mmax parts - _mmin parts + 1
    avals = _amax parts - _amin parts + 1
    svals = _smax parts - _smin parts + 1

-- Test Cases
test19a :: Action
test19a = applyAllWorkflows testPart1 testmap

testRule1 :: Rule
testRule1 = Rule _s (< 1351) (PassTo "px")

testRule2 :: Rule
testRule2 = Rule _x (const True) (PassTo "qqz")

testRule3 :: Rule
testRule3 = Rule _s (> 2270) (PassTo "qs")

testRule4 :: Rule
testRule4 = Rule _m (< 1801) (PassTo "hdj")

testRule5 :: Rule
testRule5 = Rule _x (const True) Reject

testRule6 :: Rule
testRule6 = Rule _s (> 3448) Accept

testRule7 :: Rule
testRule7 = Rule _x (const True) (PassTo "lnx")

testRule8 :: Rule
testRule8 = Rule _m (const True) Accept

testWorkflowIn :: [Rule]
testWorkflowIn = [testRule1, testRule2]

testWorkFlowqqz :: [Rule]
testWorkFlowqqz = [testRule3, testRule4, testRule5]

testWorkFlowqs :: [Rule]
testWorkFlowqs = [testRule6, testRule7]

testWorkFlowlnx :: [Rule]
testWorkFlowlnx = [testRule8]

testPart1 :: Part
testPart1 = Part 787 2655 1222 2876

testmap :: M.Map String [Rule]
testmap = M.fromList [("in", testWorkflowIn), ("qqz", testWorkFlowqqz), ("qs", testWorkFlowqs), ("lnx", testWorkFlowlnx)]

testParsingLT :: Either ParseError Bool
testParsingLT = fmap ($ 400) (parse conditionParser "text" "<500")

testParsingGT :: Either ParseError Bool
testParsingGT = fmap ($ 400) (parse conditionParser "text" ">500")

testParsingField :: Either ParseError Int
testParsingField = fmap ($ testPart1) (parse fieldParser "text" "x")

testParsingRule :: Either ParseError Rule
testParsingRule = parse ruleParser "text" "s<1351:px,"

testPArsingRules :: Either ParseError Workflow
testPArsingRules = parse rulesParser "text" "{a<2006:qkq,m>2090:A,rfg}"

testParsingWorkflow :: Either ParseError (String, Workflow)
testParsingWorkflow = parse workflowParser "text" "px{a<2006:qkq,m>2090:A,rfg}"

testParsingPart :: Either ParseError Part
testParsingPart = parse partParser "test" "{x=787,m=2655,a=1222,s=2876}"
