{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

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


-- First motivating example: raising exceptions
data ExceptM a = Raise Exception
               | Return a
               deriving Show
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
newtype DebugM a = DebugM (Output, a)
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
-- by using the monad operations
eval :: (Monad m) => Term -> m Int
eval (Con a) = return a
eval (Div t u) =
  eval t >>=
  \a -> eval u >>=
  \b -> return (a `div` b)


-- Now that we have a class definition, we need to teach the compiler how each of our data types
-- may behave as a monad. We do this by defining instances

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
                      let (a, y) = (runState m) s
                          (b, z) = runState (k a) y
                      in (b, z))

-- DebugM as a Monad
instance Monad DebugM where
  return a = DebugM ("", a)
  m >>= k = DebugM result
    where result = let DebugM (firstOutput, a) = m
                       DebugM (secondOutput, b) = k a
                   in (firstOutput ++ secondOutput, b)
