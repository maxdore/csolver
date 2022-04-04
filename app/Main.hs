{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hPutStr, hClose)
import System.Process.Typed
import System.Exit (ExitCode)
-- import qualified Data.ByteString as Byte
import qualified Data.ByteString.Lazy as Byte -- (ByteString , drop)
import qualified Data.ByteString.Lazy.Search as Byte

import System.Environment
import Data.List
import Data.HashMap.Strict
-- import qualified Data.List.NonEmpty as NE
import GHC.Generics
import Data.String
import Data.Aeson
import Data.Aeson.Types

import Lib



-- data AgdaGoal = AgdaGoal String
--   deriving (Show)
  
-- instance FromJSON AgdaGoal where
--   parseJSON = withObject "info" $ withObject "visibleGoals" $ \v -> do
--     ty <- v .: "visibleGoals"
--     return $ AgdaGoal ty


main :: IO ()
main = do
  args <- getArgs
  putStrLn "The arguments are:"
  mapM putStrLn args
  let file = args !! 0
  let cmd = "IOTCM \"" ++ file ++ "\" None Indirect (Cmd_load \"" ++ file ++ "\" [])"

  (out, err) <- readProcess_ $ setStdin (fromString cmd) "agda --interaction-json"
  let json = (((Byte.drop 6) . (Byte.take (Byte.length out - 7))) out)
  -- print json
  -- let test = "{\"visibleGoals\":[{\"constraintObj\":{\"range\":[{\"start\":{\"line\":38,\"pos\":940,\"col\":8},\"end\":{\"line\":38,\"pos\":941,\"col\":9}}],\"id\":0},\"kind\":\"OfType\",\"type\":\"PathP (\206\187 i \226\134\146 seg i \226\137\161 seg i) (\206\187 _ \226\134\146 zero) (\206\187 _ \226\134\146 one)\"}]}"


  let start = (Byte.indices "{\"kind\":\"DisplayInfo\"" json !! 0)
  let end = (Byte.indices "{\"kind\":\"InteractionPoints\"" json !! 0)
  -- print end
  let goals = ((Byte.take (end - start)) . (Byte.drop start) ) json

  -- print goals
  let mmh = decode goals :: Maybe Object
  case mmh of
    (Just res) -> mapM print (toList res)

  print "asd"

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
