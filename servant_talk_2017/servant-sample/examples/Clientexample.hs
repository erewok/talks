{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Servant.Foreign
import           System.FilePath
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.Client

import           Servant.PY
import           Servant.PY.Python

import           Lib

-- Haskell client generation
postCounter
  :<|> multiplier
  :<|> reset
  :<|> paramCounter = client counterApi

queries :: ClientM (Counter, Counter, Counter)
queries = do
  initial <- reset $ Counter 10
  multCount <- multiplier 5
  post <- postCounter
  return (initial, multCount, post)


run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (initial, multCount, post) -> do
      print initial
      print multCount
      print post

instance HasForeignType Python B.ByteString Counter where
  typeFor _ _ _ = "{\"value\": int}"

main :: IO ()
main = do
  -- test out the Haskell clients
  run
  -- Write out a Python module with client code
  writePythonForAPI counterApi requests ("examples" </> "api.py")
