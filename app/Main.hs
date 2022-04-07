{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hPutStr, hClose)
import System.Process.Typed
import System.Exit (ExitCode)
-- import qualified Data.ByteString as Byte
import qualified Data.ByteString.Lazy as Byte -- (ByteString , drop)
import qualified Data.ByteString.Lazy.Char8 as ByteC -- (ByteString , drop)
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
import AgdaInteractive



main :: IO ()
main = do
  args <- getArgs
  -- putStrLn "The arguments are:"
  -- mapM putStrLn args
  let file = args !! 0

  goals <- readGoals file

  mapM (\g -> do
           ctxt <- buildContext file g
           s <- runSolve $ mkSEnv ctxt g
           case s of
             Left err -> undefined
             Right (res , _) -> ByteC.putStrLn $ agdaResult res
           print ""
           ) goals

  print "Success"
