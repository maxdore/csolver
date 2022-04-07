{-# LANGUAGE OverloadedStrings #-}
module AgdaInteractive where

import System.Process.Typed
import Control.Monad.Except
import qualified Data.ByteString.Lazy as Byte
import qualified Data.ByteString.Lazy.Search as ByteS

import Data.String
import Data.List
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy.IO          as TL

import Text.ParserCombinators.Parsec.Error

import Data
import PathParser





readGoals :: String -> IO [Cube]
readGoals file = do
  let cmd = "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])"
  (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
  print out

  let goalInd = ByteS.indices "\"type\":\"" out
  let goalsRaw = map (\i -> ByteS.split "\"" (Byte.drop (i + 8) out) !! 0) goalInd

  print $ (TL.unpack . TL.decodeUtf8) (goalsRaw !! 0)

  let goals = sequence (map  (parseCube . TL.unpack . TL.decodeUtf8) goalsRaw)
  print goals
  case goals of
    Right gs -> return gs
    -- Left parserr -> print "Could not parse goals in file" >> return [Point]


inferType :: String -> Id -> IO Cube
inferType file name = do
  let cmd = "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_infer_toplevel Simplified \"" ++ name ++ "\")"
  (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
  print out
  let tyRaw = ByteS.split "\"" (Byte.drop ((ByteS.indices "\"expr\":\"" out) !! 0 + 8) out) !! 0
  print tyRaw
  let ty = (parseCube . TL.unpack . TL.decodeUtf8) tyRaw
  print ty
  case ty of
    Right t -> return t

searchLemmas :: String -> Id -> IO [Decl]
searchLemmas file name = do
  let cmd = "IOTCM \"" ++ file ++ "\" NonInteractive Indirect (Cmd_search_about_toplevel Simplified \"" ++ name ++ "\")"
  (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"

  let lemmasInd = ByteS.indices "\"name\":\"" out
  let lemmasName = map (\i -> ByteS.split "\"" (Byte.drop (i + 8) out) !! 0) lemmasInd
  let lemmasRaw = map (\k -> ByteS.split "\"" (Byte.drop ((lemmasInd !! k) + (Byte.length (lemmasName !! k)) + 18) out) !! 0) [0 .. (length lemmasInd)-1]

  let lemmas = (map  (parseCube . TL.unpack . TL.decodeUtf8) lemmasRaw)
  print lemmasRaw
  print lemmas
  let decls = (zipWith (,) lemmasName lemmas)
  -- case lemmas of
  --   Right ls -> return (zipWith (,) lemmasName ls)
  return $ filter (\(n,c) -> n /= "UNDEFINED") $ map (\(n,ty) -> case ty of
                      (Right c) -> ((TL.unpack . TL.decodeUtf8) n,c)
                      (Left _) -> ("UNDEFINED", Point)) decls



buildContext :: String -> Cube -> IO Tele
buildContext file goal = do
  decls <- mapM (\t -> inferType file t >>= (\ty -> return (t,ty))) (getCFaces goal)
  lemmasAll <- mapM (\t -> searchLemmas file t) (getCFaces goal)
  let lemmas = filter (\(n,t) -> t /= goal) $ nub $ concat lemmasAll
  return $ decls ++ lemmas



varNames = ["i","j","k","l","m","n"]

agdaDim :: Dim -> Int -> Byte.ByteString
agdaDim (c:[]) d = agdaClause c d
agdaDim (c:c':cs) d = "(" <> (agdaClause c d) <> ") OR " <> agdaDim (c' : cs) d

agdaClause :: [Int] -> Int -> Byte.ByteString
agdaClause (i:[]) d = varNames !! (i-d)
agdaClause (i:j:is) d = (varNames !! (i-d)) <> " AND " <> agdaClause (j:is) d

agdaTerm :: Term -> Byte.ByteString
agdaTerm t = agdaTerm' t 0
  where
  agdaTerm' (Face name) d = fromString name
  agdaTerm' (Abs t) d = "\206\187 " <> (varNames !! d) <> " \226\134\146 " <> agdaTerm' t (d + 1)
  -- agdaTerm' (Abs t) d = "\955 " <> (varNames !! d) <> " \8594 " <> agdaTerm' t (d + 1)
  agdaTerm' (App t r) d = agdaTerm' t d <> " " <> agdaDim r d


agdaResult :: Result -> Byte.ByteString
agdaResult (Dir t) = agdaTerm t


-- readGoal :: String -> IO Cube
-- readGoal file = do
--   let cmd = "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])"

--   (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
--   let json = (((Byte.drop 6) . (Byte.take (Byte.length out - 7))) out)
--   print json
--   -- let test = "{\"visibleGoals\":[{\"constraintObj\":{\"range\":[{\"start\":{\"line\":38,\"pos\":940,\"col\":8},\"end\":{\"line\":38,\"pos\":941,\"col\":9}}],\"id\":0},\"kind\":\"OfType\",\"type\":\"PathP (\206\187 i \226\134\146 seg i \226\137\161 seg i) (\206\187 _ \226\134\146 zero) (\206\187 _ \226\134\146 one)\"}]}"


--   let start = (Byte.indices "{\"kind\":\"DisplayInfo\"" json !! 0)
--   let end = (Byte.indices "{\"kind\":\"InteractionPoints\"" json !! 0)
--   -- print end
--   let goals = ((Byte.take (end - start)) . (Byte.drop start) ) json

--   -- print goals
--   let mmh = decode goals :: Maybe Object
--   case mmh of
--     (Just res) -> mapM print (toList res)

--   print "asd"

--   return Point

  -- let parsed = decode goals :: Maybe AgdaGoal
  -- print parsed

-- ("info",Object (fromList [("errors",Array []),("invisibleGoals",Array []),("kind",String "AllGoalsWarnings"),

--                           ("visibleGoals",
--                              Array [
--                               Object (fromList
--                                        [
--                                          ("constraintObj",Object (fromList [("id",Number 0.0),("range",Array [Object (fromList [("end",Object (fromList [("col",Number 15.0),("line",Number 38.0),("pos",Number 947.0)])),("start",Object (fromList [("col",Number 8.0),("line",Number 38.0),("pos",Number 940.0)]))])])])),
--                                          ("kind",String "OfType"),
--                                          ("type",String "PathP (\955 i \8594 seg i \8801 seg i) (\955 _ \8594 zero) (\955 _ \8594 one)")
--                                        ])])

--                          ,("warnings",Array [])]))

    -- {\"kind\":\"Status\",\"status\":{\"checked\":false,\"showIrrelevantArguments\":false,\"showImplicitArguments\":false}}

    -- {\"kind\":\"ClearRunningInfo\"}

    -- {\"kind\":\"ClearHighlighting\",\"tokenBased\":\"NotOnlyTokenBased\"}

    -- {\"message\":\"Checking Cauto.Examples.Interval (/home/scratch/maxore/Dropbox/Uni/experiments/Cauto/Examples/Interval.agda).\\n\",\"debugLevel\":1,\"kind\":\"RunningInfo\"}

    -- {\"kind\":\"Status\",\"status\":{\"checked\":false,\"showIrrelevantArguments\":false,\"showImplicitArguments\":false}}

    -- {\"kind\":\"DisplayInfo\",\"info\":{\"visibleGoals\":[{\"type\":\"PathP (\206\187 i \226\134\146 seg i \226\137\161 seg i) (\206\187 _ \226\134\146 zero) (\206\187 _ \226\134\146 one)\",\"kind\":\"OfType\",\"constraintObj\":{\"id\":0,\"range\":[{\"start\":{\"col\":8,\"line\":38,\"pos\":940},\"end\":{\"col\":15,\"line\":38,\"pos\":947}}]}}],\"kind\":\"AllGoalsWarnings\",\"errors\":[],\"warnings\":[],\"invisibleGoals\":[]}}

    -- {\"kind\":\"InteractionPoints\",\"interactionPoints\":[{\"id\":0,\"range\":[{\"start\":{\"col\":8,\"line\":38,\"pos\":940},\"end\":{\"col\":15,\"line\":38,\"pos\":947}}]}]}

