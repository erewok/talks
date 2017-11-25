{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)

import qualified Lib as L
-- import Parser ((∅), (▻))
import qualified Parser as P

main :: IO ()
main = do
  monadMain
  parserMain

parserMain :: IO ()
parserMain = do
  putStrLn divider
  putStrLn "Let's try all the parsing examples now!"
  putStrLn "These term examples start this section"
  print $ P.parse P.term "23"
  print $ P.parse P.term "23 and more"
  print $ P.parse P.term "not a term"
  print $ P.parse P.term "((1972 ÷ 2) ÷ 23)"
  putStrLn divider

  putStrLn "Item is defined next with examples: parsers one character"
  print $ P.parse P.item ""
  print $ P.parse P.item "monad"
  putStrLn divider

  putStrLn "twoItems is with examples: parsers two items, represents sequencing"
  print $ P.parse P.twoItems "m"
  print $ P.parse P.twoItems "monad"
  putStrLn divider

  putStrLn "Alternation definitions allows to try different parsers"
  print $ P.parse P.oneOrTwoItems ""
  print $ P.parse P.oneOrTwoItems "m"
  print $ P.parse P.oneOrTwoItems "monad"
  putStrLn divider

  putStrLn "We also define parsers for character and digit literals"
  print $ (P.parse $ P.lit 'm') "monad"
  print $ (P.parse $ P.lit 'm') "parse"
  print $ P.parse P.digit "2"
  putStrLn divider

  putStrLn "We continue on with iteration, to repeat a parser"
  print $ (P.parse $ P.iterate P.digit) "23 and more"
  putStrLn "Number uses `iterate` to create a big number out of parsed digits"
  print $ P.parse P.number' "23 and more"
  putStrLn divider

  putStrLn "We would like to be biased toward the longest possible parsed number. `reiterate` achieves this."
  print $ (P.parse $ P.reiterate P.digit) "23 and more"
  print $ P.parse P.number "23 and more"
  putStrLn divider

  putStrLn "Here's an example of how to create a search a space of possibilities"
  print $ (P.parse $ P.reiterate P.oneOrTwoItems) "many"
  putStrLn divider

  putStrLn "The first definition of `term` (called here `termInitial`) only works for fully parenthesized terms"
  print $ P.parse P.termInitial "((1972 ÷ 2) ÷ 23)"
  putStrLn divider

  putStrLn "The final definition of `term` solves the parenthesized problem and the left recursion problem"
  putStrLn "Parse: '1972 ÷ 2 ÷ 23'"
  print $ P.parse P.term "1972 ÷ 2 ÷ 23)"
  putStrLn "Parse: '1972 ÷ (2 ÷ 23)'"
  print $ P.parse P.term "1972 ÷ (2 ÷ 23)"
  putStrLn "Parse: '((1972 ÷ 2) ÷ 23)'"
  print $ P.parse P.term "((1972 ÷ 2) ÷ 23)"
  putStrLn divider

  putStrLn "The final contribution in the paper is about evaluating potentially non-terminating computations"
  putStrLn "We are only going to request the first parsed value or else the following will loop forever"
  print $ (head . fst . head) $ P.parse (P.reiterateWithGuarantee P.digit) (repeat '1')
  putStrLn divider


monadMain :: IO ()
monadMain = do
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

divider :: String
divider = mconcat $ replicate 20 "*"
