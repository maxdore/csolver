module PathParser where


import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (runP)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Data

import Debug.Trace


data PState = PState Int (Map String Int)



-- parseVar :: GenParser Char st String
parseVar = many1 $ letter <|> char '_'


parseAbs :: GenParser Char st ()
parseAbs = do
  string "\955"
  spaces
  var <- parseVar
  spaces
  -- TODO UPDATE STATE
  trace "Wh" $ string "\8594"
  spaces
  return ()


parseFace :: GenParser Char st Term
parseFace = do
  name <- many1 letter
  return $ Face name

parseApp :: GenParser Char st Term
parseApp = do
  -- t <- parseTerm
  many1 $ letter
  char ' '
  many1 $ letter
  return $ App (Face "temp") [[1]]


parseTerm :: GenParser Char st Term
parseTerm = try parseApp <|> (do {parseAbs ; t <- parseTerm ; return $ Abs t}) <|> try parseFace


parsePathP :: GenParser Char st Cube
parsePathP = do
  string "PathP "
  r <- between (char '(') (char ')') (parseAbs >> parsePath)
  char ' '
  u <- between (char '(') (char ')') parseTerm
  char ' '
  v <- between (char '(') (char ')') parseTerm
  return $ Path r u v


parseEq :: GenParser Char st Cube
parseEq = do
  u <- parseTerm
  string " \8801 "
  v <- parseTerm
  return $ Path Point u v


parsePath :: GenParser Char st Cube
parsePath = parsePathP <|> parseEq

parseCube :: String -> Either ParseError Cube
-- parseCube input = parse parsePath "" input
parseCube = runP parsePath (PState 1 Map.empty) ""


-- parseCube "PathP (\955 i \8594 seg i \8801 seg i) (\955 _ \8594 zero) (\955 _ \8594 one)"
