{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)

import qualified Lib as L

divider :: String
divider = mconcat $ replicate 20 "*"


main :: IO ()
main = do
  putStrLn "Run: Simple Evaluator"
  print (L.evalSimple L.answer)
  putStrLn divider
  putStrLn "Run: Can't run the simple evaluator on the erroring computation!"
  putStrLn divider

  -- Exceptions!
  putStrLn "Run: Error Evaluator specialized function for exception type (not monadic)"
  print (L.evalExcept L.error)
  putStrLn divider

  -- Exception Monads below
  putStrLn "Run: Answer Evaluation using ExceptM Monad instance"
  print (L.evalExcept' L.answer :: L.ExceptM Int)
  putStrLn divider

  putStrLn "Run: Error Evaluation using ExceptM Monad instance"
  print (L.evalExcept' L.error :: L.ExceptM Int)
  putStrLn divider

  -- State!
  putStrLn "Run: State Evaluator specialized function for StateM (not monadic)"
  print $ L.runState (L.evalState L.answer) 0
  print $ L.runState (L.evalState L.bigDiv) 0
  putStrLn divider

  -- StateM Monads below
  putStrLn "Run: State Evaluation using StateM Monad instance"
  -- Eval doesn't know what type of thing it should produce
  print $ L.runState (L.evalState' L.answer :: L.StateM Int) 0
  putStrLn "Here's a computation with 3 `divs`"
  print $ L.runState (L.evalState' L.bigDiv :: L.StateM Int) 0
  putStrLn divider

  -- Debuggin It!
  putStrLn "Run: Debug Evaluator specialized function for DebugM (not monadic)"
  let (L.DebugM (output, answer)) = L.evalDebug L.answer
  putStrLn output
  print answer
  putStrLn divider

  -- StateM Monads below
  putStrLn "Run: Debug Evaluation using DebugM Monad instance"
  -- Eval doesn't know what type of thing it should produce
  let (L.DebugM (output, answer)) = L.evalDebug' L.answer :: L.DebugM Int
  putStrLn output
  print answer

  let (L.DebugM (output, answer)) = L.evalDebug' L.bigDiv :: L.DebugM Int
  putStrLn output
  print answer
  putStrLn divider
