{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process.Typed
import qualified Data.ByteString.Lazy.Char8 as ByteC

import Control.Monad.Except
import Control.Monad.State
import System.Environment

import Data
import Core
import AgdaInteractive
import Examples


runSolve :: SEnv s -> IO (Either String ([Result],SEnv s))
runSolve env = do
  res <- runExceptT $ runStateT solve env
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (rs , _)-> do
      (ByteC.putStrLn . agdaResult) (rs !! 0)
  return res

runInfer :: Tele -> Term -> IO (Either String (Cube,SEnv s))
runInfer ctxt t = do
  res <- runExceptT $ runStateT (infer t) (mkSEnv ctxt Point)
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (ty , _)->
      putStrLn $ show ty
  return res

runTest :: Tele -> Term -> Int -> Endpoint -> IO (Either String (Term,SEnv s))
runTest ctxt t i e = do
  res <- runExceptT $ runStateT (getBoundary t i e) (mkSEnv ctxt Point)
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (sigma , _) -> do
      putStrLn $ show sigma
      res <- runExceptT $ runStateT (infer sigma) (mkSEnv ctxt Point)
      case res of
        Left err -> do
          putStrLn $ "ERROR: " ++ err
        Right (ty , _)->
          putStrLn $ show ty
  return res

main :: IO ()
main = do
  args <- getArgs
  let file = args !! 0

  goals <- readGoals file

  mapM (\g -> do
           ctxt <- buildContext file g
           s <- runSolve $ mkSEnv ctxt g
           case s of
             Left err -> undefined
             Right (rs , _) -> ByteC.putStrLn $ agdaResult (rs !! 0)
           ) goals

  print "Success"
