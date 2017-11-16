module Main where

import qualified Lib as L

main :: IO ()
main = do
  putStrLn "Simple Evaluator"
  print $ show $ L.evalSimple L.answer
  putStrLn "Error Evaluator specialized to exceptions"
  print $ show $ L.evalExcept L.error
  putStrLn "Error Evaluator using ExceptM Monad instance"
  errRes <- L.eval L.error :: (L.ExceptM Int)
  putStrLn $ show errRes
