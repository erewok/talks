module Main where

import qualified MyLib as Lib

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let calc = Lib.calculateInterestOnLoan (Lib.InterestRate 2) (Lib.LoanAmount 100000) (Lib.Duration 2)
  putStrLn "Calculated interest: "
  print calc
