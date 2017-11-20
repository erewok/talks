{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude (Int
               , String
               , div
               , (+)
               , (++)
               , Show
               , show
               , (.)
               , ($)
               , Eq
               , Bool
               , (==))


-- First, Wadler introduces the following data type and a simple arithmetic evaluator
data Term = Con Int
          | Div Term Term
          deriving Show


evalSimple :: Term -> Int
evalSimple (Con a) = a
evalSimple (Div t u) = evalSimple t `div` evalSimple u

answer :: Term
answer = Div (Div (Con 1972) (Con 2)) (Con 23)

-- This one typechecks, but will throw a runtime error
error :: Term
error = Div (Con 1) (Con 0)

-- An Extra one that's bigger than the others
bigDiv :: Term
bigDiv = Div (Div (Div (Con 1972) (Con 2)) (Con 23)) (Con 7)


-- First motivating example: raising exceptions
data ExceptM a = Raise Exception
               | Return a
               deriving (Show, Eq)
type Exception = String

evalExcept :: Term -> ExceptM Int
evalExcept (Con a) = Return a
evalExcept (Div t u) = case evalExcept t of
                         Raise e -> Raise e
                         Return a ->
                           case evalExcept u of
                             Raise e -> Raise e
                             Return b ->
                               if b == 0
                               then Raise "divide by zero error"
                               else Return (a `div` b)

-- Second motiviating example: tracking state
-- We can't do this as a type synonym because GHC won't allow that later on without `TypeSynonymInstances`
newtype StateM a = StateM { runState :: State -> (a, State) }
type State = Int

evalState :: Term -> StateM Int
evalState (Con a) = StateM (\x -> (a, x))
evalState (Div t u) = StateM (\x-> let (a, y) = (runState $ evalState t) x
                                       (b, z) = (runState $ evalState u) y
                                    in (a `div` b, z + 1))


-- Third motiviating example: tracing execution (such as debug or print statements)
-- Also can't do it as a type synonym. It gets a bit more verbose, but same idea...
newtype DebugM a = DebugM (Output, a) deriving Show
type Output = String

evalDebug :: Term -> DebugM Int
evalDebug (Con a) = DebugM (line (Con a ) a, a)
evalDebug (Div t u) = let DebugM (x, a) = evalDebug t
                          DebugM (y, b) = evalDebug u
                      in DebugM (x ++ y ++ line (Div t u) (a `div` b), a `div` b)


line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"


-- Wadler introduces the monad to as the underlying pattern in all of these evaluators
-- Here, we define a class of operations under the name `Monad` and then we will describe
-- How each new "wrapper" data type behaves under these monad operations
class Monad m where
  return :: a -> m a  -- Wadler calls this `unit`, but it's `return` in Haskell
  (>>=) :: m a -> (a -> m b) -> m b  -- Wadler uses `*` here, but this is the `bind` operator in Haskell

-- Next, Wadler shows how all of the different `eval...` functions defined above can be collapsed into one
-- by using the monad operations. This function works for anything implementing `Monad` operations
-- but the 3 examples given by Wadler will each need to tweak this function a bit.
eval :: (Monad m) => Term -> m Int
eval (Con a) = return a
eval (Div t u) =
  eval t >>=
  \a -> eval u >>=
  \b -> return (a `div` b)

-- Now that we have a `Monad` class definition, we need to teach the compiler how each of our data types
-- may behave as a monad. We do this by defining instances of `Monad` for each one.

-- ExceptM as a Monad
instance Monad ExceptM where
  return a = Return a
  m >>= k = case m of
    Raise e -> Raise e
    Return a -> k a

-- Helper function for wrapping an `Exception` as an `ExceptM`
raise :: Exception -> ExceptM a
raise e = Raise e

-- StateM as a Monad
instance Monad StateM where
  return a = StateM (\s -> (a, s))
  m >>= k = StateM (\s ->
                      let (a, y) = runState m s
                          (b, z) = runState (k a) y
                      in (b, z))

-- helper function for ticking along the state
tick :: StateM ()
tick = StateM (\x -> ((), x+1))

-- DebugM as a Monad
instance Monad DebugM where
  return a = DebugM ("", a)
  m >>= k = DebugM result
    where result = let DebugM (firstOutput, a) = m
                       DebugM (secondOutput, b) = k a
                   in (firstOutput ++ secondOutput, b)

-- Helper function for starting the output composition
debug :: Output -> DebugM ()
debug x = DebugM (x, ())


-- `eval` alone won't properly handle the "divide by zero" exception, so we need to define this one:
evalExcept' :: Term -> ExceptM Int
evalExcept' (Con a) = return a
evalExcept' (Div t u) =
  evalExcept' t >>=
  \a -> evalExcept' u >>=
  \b -> if b == 0
           then raise "divide by zero error"
           else return (a `div` b)


-- In addition, we need to inject the `tick` function in for state, so we still need this one too
evalState' :: Term -> StateM Int
evalState' (Con a) = return a
evalState' (Div t u) =
  evalState' t >>=
  \a -> evalState' u >>=
  \b -> tick >>=
  \_ -> return (a `div` b)

-- finally. we need to adjust `eval` in the case of our Ouptut version as well
evalDebug' :: Term -> DebugM Int
evalDebug' (Con a) = debug (line (Con a) a) >>= \_ -> return a
evalDebug' (Div t u) =
  evalDebug' t >>=
  \a -> evalDebug' u >>=
  \b -> debug (line (Div t u) (a `div` b)) >>=
   \_ -> return (a `div` b)


-- Section 3: Monad laws
-- Wadler says that for something to be a monad it must implement the 2 operations and also satisfy 3 laws.
-- Here are the laws
-- [Left Identity]
-- return a >>= k  =  k a
-- [Right Identity]
-- m >>= return =  m
-- [Associativity]
-- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
-- We can show how these hold for our ExceptM monad defined above.

-- First, in left identity, the bind operator (`>>=`) will unnpack a monadic value `M a` to get the `a`,
-- which it will pass to a function with a signature like this k :: (a -> M b).
-- `return` wraps up `a` value into an `M a`.
-- Thus, first wrapping the `a` with `return` and then using bind to send it to function `k`
-- is the same as sending it directly to `k` without first wrapping it:
leftId :: Int -> (Int -> ExceptM Int) -> Bool
leftId n k = return n >>= k == k n

-- Next, in right identity, we recognize that `return` is another type of function `(a -> M b)` except in this case, it's `(a -> M a`).
-- Thus, we can take a monadic value `M a`, use `bind` to unwrap it, and then send it through to `return`
-- and it will be like we've done nothing at all.
rightId :: ExceptM Int -> Bool
rightId m = (m >>= return) == m

-- Finally, associativity shows how the placement of the parentheses doesn't really matter.
-- In other words, it doesn't matter where the parentheses are placed, computations will produce the same results.
