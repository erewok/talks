{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import Data.Char (isLetter, isDigit, ord)
import Data.List (foldl')
import Prelude (Int
               , String
               , Char
               , (+)
               , (-)
               , (*)
               , (++)
               , (.)
               , Eq(..)
               , Bool(..)
               , fst
               , snd
               , head
               , tail
               , undefined)

import Lib (return, (>>=), Monad(..), Term(..))
-- import qualified Lib as L

newtype Parser a = Parser { parse :: State -> [(a, State)] }
type State = String

-- "The basic parser returns the first item of input and fails if the input is exhausted"
-- Note: this example uses list syntax because of default String-type-as-linked-list in Haskell
-- Haskell's String problem is well-known, so there are lots of discussions about whether anyone
-- really wants to treat a String as a list-of-Char. Modern Haskellers would probably write this
-- parser over Text or ByteString or both.
item :: Parser Char
item = Parser go
  where go [] = []
        go (a:xs) = [(a, xs)]


-- The Parser instance for Monads represents "Sequencing"
-- It also looks really similar to our previous `StateM` example
instance Monad Parser where
  return a = Parser (\x -> [(a, x)])
  m >>= k = Parser (\x ->
                      [(b, z) | (a, y) <- parse m x, (b, z) <- parse (k a) y])

-- Sequecing happens by using monad operators with bind. This is a version of function composition.
twoItems :: Parser (Char, Char)
twoItems =
  item >>=
  \a -> item >>=
  \b -> return (a, b)

-- Alternation is a way to combine parsers
-- zero is the identity function: it fails on every input
zero :: Parser a
zero = Parser (\_ -> [])

(⊕) :: Parser a -> Parser a -> Parser a
m ⊕ n = Parser (\x -> parse m x ++ parse n x)
infixl 7 ⊕

oneOrTwoItems :: Parser String
oneOrTwoItems = (item >>= \a -> return [a])  ⊕ (item >>= \a -> item >>= \b -> return [a, b])

-- Filtering
(▻) :: Parser a -> (a -> Bool) -> Parser a
m ▻ p = m >>= \a -> if p a then return a else zero
infixl 8 ▻

letter :: Parser Char
letter = item ▻ isLetter

digit :: Parser Int
digit = (item ▻ isDigit) >>= \a -> return (ord a - ord '0')

lit :: Char -> Parser Char
lit c = item ▻ (\a -> a == c)

-- Iteration
iterate :: Parser a -> Parser [a]
iterate m = (m >>= \a -> iterate m >>= \x -> return (a :x)) ⊕ return []

-- parse a number using iteration
number' :: Parser Int
number' = digit >>= \a -> iterate digit >>= \x -> return (asNumber (a:x))

asNumber :: [Int] -> Int
asNumber = foldl' addDigit 0
   where addDigit num d = 10 * num + d

-- Biased choice, where the goal is to extract the longest possible run of digits into a number
(∅) :: (Eq a) => Parser a -> Parser a -> Parser a
m ∅ n = Parser (\x -> if parse m x /= [] then parse m x else parse n x)

-- Iteration using biased choice
reiterate :: (Eq a) => Parser a -> Parser [a]
reiterate m = (m >>= \a -> reiterate m >>= \x -> return (a:x)) ∅ return []

-- A better `number` using reiterate
number :: Parser Int
number = digit >>= \a -> reiterate digit >>= \x -> return (asNumber (a:x))

-- We finally have everything we need to parse `Term` equations
termInitial :: Parser Term
termInitial =
 (number >>=
  \a -> return (Con a))
  ⊕ (lit '(' >>=
     \_ -> termInitial >>=
     \t -> lit '÷' >>=
     \_ -> termInitial >>=
     \u -> lit ')' >>=
     \_ -> return (Div t u))

-- This term loops infinitely!
-- "This is called the *left recursion problem*".
termBad :: Parser Term
termBad =
  (termBad >>=
  \t -> lit '÷' >>=
  \_ -> factor >>=
  \u -> return (Div t u))
  ⊕ factor

factor :: Parser Term
factor =
  (number >>=
  \a -> return (Con a))
  ⊕ (lit '(' >>=
    \_ -> term >>=
    \t -> lit ')' >>=
    \_ -> return t)

-- Here's the final `term` definition.
term :: Parser Term
term = factor >>= \t -> term' t

term' :: Term -> Parser Term
term' t =
  (lit '÷' >>=
  \_ -> factor >>=
  \u -> term' (Div t u))
  ⊕ return t


-- One final contribution in the paper: lazy evaluation
-- The goal is parse what we can, even if the remaining stuff forms a computation that
-- does not complete (represented by "bottom" or `undefined` in Haskell)
guarantee :: Parser a -> Parser a
guarantee m = Parser (\x -> let u = parse m x
                       in (fst (head u), snd (head u)) : tail u)


-- Iteration using biased choice
reiterateWithGuarantee :: (Eq a) => Parser a -> Parser [a]
reiterateWithGuarantee m = guarantee (
  (m >>= \a -> reiterateWithGuarantee m >>= \x -> return (a:x)) ∅ return []
  )
