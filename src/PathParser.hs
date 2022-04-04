module PathParser where



import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Data


parseTerm :: GenParser Char st Term
parseTerm = do
  return $ Face "a"

parsePath :: GenParser Char st Cube
parsePath = do
  string "PathP "
  r <- between (char '(') (char ')') parsePath
  u <- parseTerm
  v <- parseTerm
  return $ Path r u v


parseCube :: String -> Either ParseError Cube
parseCube input = parse parsePath "PathP (\955 i \8594 seg i \8801 seg i) (\955 _ \8594 zero) (\955 _ \8594 one)" input
