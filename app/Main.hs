{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process.Typed
import qualified Data.ByteString.Lazy.Char8 as ByteC

import Control.Monad.Except
import Control.Monad.State
import System.Environment
import qualified Data.Map as Map

import Data
import Core
import AgdaInteractive
import Examples


runSolve :: (Bool -> SEnv s) -> IO (Either String ([Result],SEnv s))
runSolve env = do
  res <- runExceptT $ runStateT solve (env False)
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (rs , _)-> do
      (ByteC.putStrLn . agdaResult) (rs !! 0)
  return res


runSolve2 :: (Bool -> SEnv s) -> IO (Either String ([Result],SEnv s))
runSolve2 env = do
  -- res1 <- runExceptT $ runStateT solve (env False)
  res2 <- runExceptT $ runStateT solve (env True)
  -- case res1 of
  --   Left err -> do
  --     putStrLn $ "ERROR: " ++ err
  --   Right (r1 , _)-> do
  --     (ByteC.putStrLn . agdaResult) (r1 !! 0)
  case res2 of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (r2 , _)-> do
      (ByteC.putStrLn . agdaResult) (r2 !! 0)
  -- return $ res1 ++ res2
  return $ Left ""


runInfer :: Tele -> Term -> IO (Either String (Cube,SEnv s))
runInfer ctxt t = do
  res <- runExceptT $ runStateT (infer t) (mkSEnv ctxt Point False)
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (ty , _)->
      putStrLn $ show ty
  return res

runTest :: Tele -> Term -> Int -> Endpoint -> IO (Either String (Term,SEnv s))
runTest ctxt t i e = do
  res <- runExceptT $ runStateT (getBoundary t i e) (mkSEnv ctxt Point False)
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (sigma , _) -> do
      putStrLn $ show sigma
      res <- runExceptT $ runStateT (infer sigma) (mkSEnv ctxt Point False)
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
  let verbose = "--verbose" `elem` args
  let all = "--all" `elem` args

  goals <- readGoals file

  mapM (\g -> do
           ctxt <- buildContext file g
           -- print goals
           -- print ctxt
           s <- runExceptT $ runStateT solve $ SEnv ctxt g 0 Map.empty verbose all
           case s of
             Left err -> print err
             Right (rs , _) -> ByteC.putStrLn $ agdaResult (rs !! 0)
           ) goals
  return ()
