module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Sum Type
data OfferType =
  Conditional
  | Regular
  deriving (Show, Eq)

offerTypeToLendio :: OfferType -> String
offerTypeToLendio Conditional = "soft"
offerTypeToLendio Regular     = "hard"

-- Product type (this is called a "record" in Haskell)
data Vehicle = Vehicle {
  wheelCount :: Int,
  isATruck :: Bool,
  modelName :: String
  } deriving (Show)

a_vehicle = Vehicle {
    wheelCount=4,
    isATruck=True,
    modelName="tacoma"}


-- | In Haskell, this is a common type called "Maybe"
data Option a =
  Some a
  | None
  deriving (Show)

-- | In Haskell, this is a common type called "Either"
data Result t e =
  Ok t
  | Err e
  deriving (Show)

-- Dealing with generics!
some_func :: a -> a
some_func val = undefined

-- What may be known about this `a` generic thing??
-- some_string :: a -> String
-- some_string val = show val

newtype InterestRate = InterestRate Double deriving (Show)
newtype LoanAmount = LoanAmount Double deriving (Show)
newtype Duration = Duration Double deriving (Show)
newtype InterestCharged = InterestCharged Double deriving (Show)

calculateInterestOnLoan :: InterestRate -> LoanAmount -> Duration -> InterestCharged
calculateInterestOnLoan (InterestRate rate) (LoanAmount amt) (Duration years) =
    let
        fractional_rate = rate / 100
    in InterestCharged (amt * years * fractional_rate)
