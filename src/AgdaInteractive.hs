{-# LANGUAGE OverloadedStrings #-}
module AgdaInteractive where

import System.Process.Typed
import Control.Monad.Except
import qualified Data.ByteString.Lazy as Byte
import qualified Data.ByteString.Lazy.Search as ByteS

import Data.String
import Data.List
import Data.Ord
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy.IO          as TL

import Text.ParserCombinators.Parsec.Error

import Data
import PathParser


-- parseEmacs :: String -> Byte.ByteString -> Byte.ByteString -> IO [String]
parseEmacs cmd pre post = do
  (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction"
  print out

  let goalInd = ByteS.indices pre out
  let goalsRaw = map (\i -> ByteS.split post (Byte.drop (i + (Byte.length (Byte.fromStrict pre))) out) !! 0) goalInd

  return $ map (TL.unpack . TL.decodeUtf8) goalsRaw


readGoals :: String -> IO [Cube]
readGoals file = do
  goalsRaw <- parseEmacs ("IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])") "\"?0 : " "\\n"
  let goals = sequence (map parseCube goalsRaw)
  print goals
  case goals of
    Right gs -> return gs
    -- Left parserr -> print "Could not parse goals in file" >> return [Point]


inferType :: String -> Id -> IO Cube
inferType file name = do
  typesRaw <- parseEmacs ("IOTCM \"" ++ file ++ "\" None Indirect (Cmd_infer_toplevel Simplified \"" ++ name ++ "\")") "*Inferred Type*\" \"" "\" nil"
  let ty = parseCube (typesRaw !! 0)
  print ty
  case ty of
    Right t -> return t

  -- let cmd = 
  -- (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction"
  -- print out
  -- let tyRaw = ByteS.split "\"" (Byte.drop ((ByteS.indices "\"expr\":\"" out) !! 0 + 8) out) !! 0
  -- print tyRaw
  -- let ty = (parseCube . TL.unpack . TL.decodeUtf8) tyRaw
  -- print ty

searchLemmas :: String -> Id -> IO [Decl]
searchLemmas file name = do
  let cmd = "IOTCM \"" ++ file ++ "\" NonInteractive Indirect (Cmd_search_about_toplevel Simplified \"" ++ name ++ "\")"
  (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction"

  let relevant = (ByteS.split "Definitions about" out) !! 1
  print relevant

  let lemmasInd = ByteS.indices ("\\n  ") relevant
  let lemmasName = map (\i -> ByteS.split " : " (Byte.drop (i + 4) relevant) !! 0) lemmasInd
  let lemmasRaw = map (\k -> ByteS.split "\\n  " (Byte.drop ((lemmasInd !! k) + (Byte.length (lemmasName !! k)) + 7) relevant) !! 0) [0 .. (length lemmasInd)-1]

  let lemmas = (map  (parseCube . TL.unpack . TL.decodeUtf8) lemmasRaw)
  print lemmasName
  print lemmasRaw
  print lemmas
  let decls = (zipWith (,) (map (\n -> (ByteS.split " " n) !! 0) lemmasName) lemmas)
  -- case lemmas of
  --   Right ls -> return (zipWith (,) lemmasName ls)
  return $ filter (\(n,c) -> n /= "UNDEFINED") $ map (\(n,ty) -> case ty of
                      (Right c) -> ((TL.unpack . TL.decodeUtf8) n,c)
                      (Left _) -> ("UNDEFINED", Point)) decls





buildContext :: String -> Cube -> IO Tele
buildContext file goal = do
  gdecls <- mapM (\t -> inferType file t >>= (\ty -> return (t,ty))) (getCFaces goal)
  rdecls <- mapM (\t -> inferType file t >>= (\ty -> return (t,ty))) (nub (concat (map (\(n,c) -> getCFaces c) gdecls)))
  let decls = nub $ gdecls ++ rdecls

  lemmasAll <- mapM (\t -> searchLemmas file t) (getCFaces goal)
  let lemmas = filter (\(n,t) -> t /= goal) $ nub $ concat lemmasAll
  return $ sortBy (comparing snd) $ nub $ decls ++ lemmas


dimNames = ["i","j","k","l","m","n"]


agdaDim :: Dim -> Int -> Int -> Byte.ByteString
agdaDim (c:[]) d s = agdaClause c d s
agdaDim (c:c':cs) d s = (agdaClause c d s) <> " \226\136\168 " <> agdaDim (c' : cs) d s

agdaClause :: [Int] -> Int -> Int -> Byte.ByteString
agdaClause (i:[]) d s = dimNames !! (if d-i == s then (d-i + 1) else (d-i))
agdaClause (i:j:is) d s = (dimNames !! (if d-i == s then (d-i + 1) else (d-i))) <> " \226\136\167 " <> agdaClause (j:is) d s

-- agdaTerm :: Term -> Byte.ByteString
-- agdaTerm t = agdaTerm' t 0
--   where
--   agdaTerm' (Face name) d = fromString name
--   agdaTerm' (Abs t) d = "\206\187 " <> (dimNames !! d) <> " \226\134\146 " <> agdaTerm' t (d + 1)
--   agdaTerm' (App t r) d = agdaTerm' t d <> " (" <> agdaDim r d <> ")"

agdaTerm (Face name) d s = fromString name
agdaTerm (App t r) d s = agdaTerm t d s <> " (" <> agdaDim r d s <> ")"


agdaResult :: Result -> Byte.ByteString
agdaResult (Dir t) = let (n , t') = unpeelAbs t in agdaAbs n <> agdaTerm t' n 0
agdaResult (Comp b sides) = let
   (n , b') = unpeelAbs b
   sides' = zip (zip (map (\(zero,one) -> ((snd . unpeelAbs) zero , (snd . unpeelAbs) one)) sides) dimNames) [1..n]
   in
    agdaAbs n <> "hcomp (\206\187 " <> (dimNames !! n) <> " \226\134\146 \206\187 {\n"
              <> Byte.concat (map (\(((zero,one),dim),i) ->
                                      " ; (" <> dim <> " = i0) \226\134\146 " <> agdaTerm zero n i <> "\n" <>
                                     " ; (" <> dim <> " = i1) \226\134\146 " <> agdaTerm one n i <> "\n"
                                  ) sides')
              <> "}) (" <> agdaTerm b' n 0 <> ")"

agdaAbs :: Int -> Byte.ByteString
agdaAbs n = "\206\187" <> ((Byte.concat (map (\i -> " " <> (dimNames !! (i-1))) [1 .. n]))) <> " \226\134\146 "



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



-- WITH JSON

--   readGoals :: String -> IO [Cube]
-- readGoals file = do
--   let cmd = "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])"
--   (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
--   print out

--   let goalInd = ByteS.indices "\"type\":\"" out
--   let goalsRaw = map (\i -> ByteS.split "\"" (Byte.drop (i + 8) out) !! 0) goalInd

--   print $ (TL.unpack . TL.decodeUtf8) (goalsRaw !! 0)

--   let goals = sequence (map  (parseCube . TL.unpack . TL.decodeUtf8) goalsRaw)
--   print goals
--   case goals of
--     Right gs -> return gs
--     -- Left parserr -> print "Could not parse goals in file" >> return [Point]


-- inferType :: String -> Id -> IO Cube
-- inferType file name = do
--   let cmd = "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_infer_toplevel Simplified \"" ++ name ++ "\")"
--   (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
--   print out
--   let tyRaw = ByteS.split "\"" (Byte.drop ((ByteS.indices "\"expr\":\"" out) !! 0 + 8) out) !! 0
--   print tyRaw
--   let ty = (parseCube . TL.unpack . TL.decodeUtf8) tyRaw
--   print ty
--   case ty of
--     Right t -> return t

-- searchLemmas :: String -> Id -> IO [Decl]
-- searchLemmas file name = do
--   let cmd = "IOTCM \"" ++ file ++ "\" NonInteractive Indirect (Cmd_search_about_toplevel Simplified \"" ++ name ++ "\")"
--   (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
--   print out

--   let lemmasInd = ByteS.indices "\"name\":\"" out
--   let lemmasName = map (\i -> ByteS.split "\"" (Byte.drop (i + 8) out) !! 0) lemmasInd
--   let lemmasRaw = map (\k -> ByteS.split "\"" (Byte.drop ((lemmasInd !! k) + (Byte.length (lemmasName !! k)) + 18) out) !! 0) [0 .. (length lemmasInd)-1]

--   let lemmas = (map  (parseCube . TL.unpack . TL.decodeUtf8) lemmasRaw)
--   print lemmasRaw
--   print lemmas
--   let decls = (zipWith (,) lemmasName lemmas)
--   -- case lemmas of
--   --   Right ls -> return (zipWith (,) lemmasName ls)
--   return $ filter (\(n,c) -> n /= "UNDEFINED") $ map (\(n,ty) -> case ty of
--                       (Right c) -> ((TL.unpack . TL.decodeUtf8) n,c)
--                       (Left _) -> ("UNDEFINED", Point)) decls
