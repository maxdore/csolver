{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hPutStr, hClose)
import System.Process.Typed
import System.Exit (ExitCode)
-- import qualified Data.ByteString as Byte
import qualified Data.ByteString.Lazy as Byte -- (ByteString , drop)

-- import System.Environment
-- import Data.List
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import Lib

main :: IO ()
main = do
    -- args <- getArgs
    -- putStrLn "The arguments are:"
    -- mapM putStrLn args
      -- alternatively: `shell "date"` or just "date"
  -- runProcess "agda --interaction-json" -- >>= print
  -- putStrLn "Finished"

  -- withProcessWait "agda --interaction-json" $ \process -> do
  --     exitCode <- waitExitCode (process :: Process () () ())
  --     print exitCode

  (out, err) <- readProcess_ $ setStdin "IOTCM \"/home/max/a/experiments/Cauto/Examples/Interval.agda\" None Indirect (Cmd_load \"/home/max/a/experiments/Cauto/Examples/Interval.agda\" [])" "agda --interaction-json"
  let json = (((Byte.drop 6) . (Byte.take (Byte.length out - 7))) out)
  print json
  let test = "{\"visibleGoals\":[{\"constraintObj\":{\"range\":[{\"start\":{\"line\":38,\"pos\":940,\"col\":8},\"end\":{\"line\":38,\"pos\":941,\"col\":9}}],\"id\":0},\"kind\":\"OfType\",\"type\":\"PathP (\206\187 i \226\134\146 seg i \226\137\161 seg i) (\206\187 _ \226\134\146 zero) (\206\187 _ \226\134\146 one)\"}]}"
  let mmh = decode test :: Maybe Object
  print mmh
  case mmh of
    (Just res) -> print res
