module MyLib where

some_func :: Int -> String
some_func 0 = ""
some_func 1 = "Only one"
some_func a_number = show a_number

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

-- | pattern-matching makes it so easy t
showMaybeInt :: Option Int -> String
showMaybeInt (Some number) = show number
showMaybeInt None = ""

-- Dealing with generics!
-- What may be known about this `a` generic thing??
some_generic_func :: a -> a
some_generic_func val = undefined

-- some_string_bad :: a -> String
-- some_string_bad val = show val

some_string :: (Show a) => a -> String
some_string val = show val

newtype InterestRate = InterestRate Double deriving (Show)
newtype LoanAmount = LoanAmount Double deriving (Show)
newtype Duration = Duration Double deriving (Show)
newtype InterestCharged = InterestCharged Double deriving (Show)

calculateInterestOnLoan :: InterestRate -> LoanAmount -> Duration -> InterestCharged
calculateInterestOnLoan (InterestRate rate) (LoanAmount amt) (Duration years) =
    let
        fractional_rate = rate / 100
    in InterestCharged (amt * years * fractional_rate)
