module PathParser where


import Data.Map (Map , (!))
import qualified Data.Map as Map
import Text.Parsec (runP , updateState)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Data

import Debug.Trace


data PState = PState Int (Map String Int)



-- parseVar :: GenParser Char PState String
ident = many1 alphaNum

parseVar = many1 $ alphaNum <|> char '_'


bindVar :: String -> PState -> PState
bindVar v (PState i f) = PState (i+1) (Map.insert v i f)

getBind :: PState -> String -> Int
getBind (PState _ f) = (!) f


parseAbs :: GenParser Char PState ()
parseAbs = do
  char '\955'
  spaces
  var <- parseVar
  spaces
  updateState (bindVar var)
  char '\8594'
  spaces
  return ()


parseDim :: GenParser Char PState [[Int]]
parseDim = undefined

parseBase :: GenParser Char PState Term
parseBase = do
  face <- ident
  apps <- many (try (char ' ' >> ident))
  st <- getState
  return $ foldl (\t i -> App t [[getBind st i]]) (Face face) apps
  -- return $ Face face


parseTerm :: GenParser Char PState Term
parseTerm = (do {parseAbs ; t <- parseTerm ; return $ Abs t}) <|> parseBase
-- parseTerm = try parseApp <|> (do {parseAbs ; t <- parseTerm ; return $ Abs t}) <|> try parseFace



parsePathP :: GenParser Char PState Cube
parsePathP = do
  string "PathP "
  r <- between (char '(') (char ')') (parseAbs >> parsePath)
  char ' '
  u <- (between (char '(') (char ')') parseTerm) <|> parseTerm
  char ' '
  v <- (between (char '(') (char ')') parseTerm) <|> parseTerm
  return $ Path r u v


parseEq :: GenParser Char PState Cube
parseEq = do
  u <- parseTerm
  spaces
  char '\8801'
  spaces
  v <- parseTerm
  return $ Path Point u v

parsePoint :: GenParser Char PState Cube
parsePoint = do
  parseVar
  return Point

parsePath :: GenParser Char PState Cube
parsePath = try parsePathP <|> try parseEq <|> parsePoint

parseCube :: String -> Either ParseError Cube
-- parseCube input = parse parsePath "" input
parseCube = runP parsePath (PState 1 Map.empty) ""


-- parseCube "PathP (\955 i \8594 seg i \8801 seg i) (\955 _ \8594 zero) (\955 _ \8594 one)"
