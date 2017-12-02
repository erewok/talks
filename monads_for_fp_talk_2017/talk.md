---
title: Monads for Functional Programming
separator: <!--s-->
verticalSeparator: <!--v-->
theme: solarized
revealOptions:
    transition: 'fade'
---
# Monads for Functional Programming

Philip Wadler, 1992

<!--v-->

Notes and code for this talk:

https://github.com/erewok/talks/tree/master/monads_for_fp_talk_2017

<!--v-->

## Questions

- What problem is Wadler solving?
- Why did Haskell have this problem in the first place?
- How does the monad work?
- Why is this solution interesting beyond the world of Haskell?

<!--v-->

### Our Approach in this Talk

1. A short history of Functional Programming
1. A bit of Lambda Calculus
1. Some Haskell
1. Close reading of Sections 1 through 3 of the paper
1. Light reading of Section 5

<!--s-->

Q: What's a functional programming language?

A: A language based on Lambda Calculus.

<!--v-->

### Chronology of FP

- 1958: Lisp by John McCarthy
- 1970: Scheme, a Lisp based more closely on Lambda Calculus
- 1973: ML by Robin Milner
- 1978: John Backus, Turing Award Lecture: "Can Programming Be Saved from the Von Neumann Style?"
- Mid-80s: Lots of Pure, Lazy FP languages; Simon Peyton Jones calls this period "a tower of Babel".
- 1987: First Haskell meeting.

<!--v-->

### Broader Context; the 80s

- End of Moore's Law in sight (parallel computing will become more important).
- Desire for a *research* programming language for PL researchers to use.
- Desire for languages with rigorous mathematical and theoretical foundations.

<!--s-->

## Lambda Calculus

The identity function:

```haskell
𝝺x.x
```

The "self-application function":

```haskell
𝝺s.(s s)
```

<!--v-->

### Beta Reduction 1

```haskell
 𝝺x.x 𝝺s.(s s)
  -> 𝝺s.(s s)
```

We can't reduce any more so this equation is in "normal form."

<!--v-->

### Beta Reduction 2

```haskell
𝝺s.(s s) 𝝺x.x
  -> (𝝺x.x 𝝺x.x)
    -> 𝝺x.x
```

<!--v-->

### Representing Bools

```haskell
def true = 𝝺first.𝝺second.first
def false = 𝝺first.𝝺second.second
```

<!--v-->

### Representing Numbers

We need `0` and a `succ` function:

```haskell
def zero = 𝝺x.x
def succ = 𝝺n.𝝺s((s false) n)

-- one, for example: substitute `false` with its definition above
def one = 𝝺s((s false) 𝝺x.x)
 -> 𝝺s((s 𝝺first.𝝺second.second) 𝝺x.x)
```

<!--v-->

## Using Numbers

```haskell
-- an `iszero` test (`𝝺first.𝝺second.first` is the same as `true`)
def iszero = 𝝺n(n 𝝺first.𝝺second.first)

-- iszero applied to zero
𝝺x.x 𝝺first.𝝺second.first
 -> 𝝺first.𝝺second.first -- `true`

-- iszero applied to one
𝝺s((s 𝝺first.𝝺second.second) 𝝺x.x) 𝝺first.𝝺second.first
 -> ((𝝺first.𝝺second.first 𝝺first.𝝺second.second) 𝝺x.x)
   -> 𝝺second.(𝝺first.𝝺second.second) 𝝺x.x
     -> 𝝺first.𝝺second.second -- `false`
```

<!--s-->

### Primary Goals for Haskell Implementers

- Laziness
- Functional Programming based on Typed Lambda Calculus (System F)

**Purity** is a side effect of laziness.

<!--v-->

> An immediate consequence of laziness is that evaluation order is demand-driven. As a result, it becomes more or less impossible to *reliably* perform input/output or other side effects as the result of a function call. For example, if a function `f` has type `Int -> Int` you can be sure that `f` will not read or write any mutable variables, nor will it perform any input/output. In short, `f` really is a function in the mathematical sense...

<!--v-->

### Haskell's Side Effect Problem

As a result of purity, Haskell could not compute side effects, such as:

- Print to the screen
- Take input from users
- Read files
- Write files

<!--s-->

## Quick Haskell Primer

```haskell
type MyId = Int

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

newtype MyId2 = MyId2 Int

-- Records:
data Contact = Contact { email :: String, phone :: String}

λ> Contact "heyyou@hey.you.email" "619-555-2717"
Contact {email = "heyyou@hey.you.email", phone = "619-555-2717"}
```

<!--v-->

### Functions

```haskell
doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100 then x else x*2

double y = let x = y * y in x * 2

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

printSomeNums :: Int -> IO ()
printSomeNums n = putStrLn allNums
  where allNums = concat $ map show numbers
        numbers = take n [1..]
```

<!--v-->

## Typeclasses

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Maybe a = Just a | Nothing
instace Functor Maybe where
   fmap f (Just a) = Just (f a )
   fmap f Nothing = Nothing

genericFunctorFunc :: (Functor f, Show a) => f a -> f String
genericFunctorFunc f = fmap show f

λ> genericFunctorFunc $ Just 5
Just "5"
λ> genericFunctorFunc [5, 4, 3]
["5","4","3"]
```

<!--s-->

### Monads Paper Section 1: Introduction

- Origins in Category Theory
- Inspired by Eugenio Moggi's work on monads
- Monads "integrate impure effects into pure functional languages"

(Category Theory also makes other appearances in Haskell: Functor, Applicative, Monad, Monoid, etc.)

<!--v-->

### Evaluator with three motivating examples

- Add error handling
- Count operations
- Add execution trace (printing, debugging)

<!--v-->

### Simple Evaluator: First Attempt

```haskell
data Term = Con Int
          | Div Term Term

eval :: Term -> Int
eval (Con a) = a
eval (Div t u) = eval t `div` eval u

-- BOOM
λ> eval (Div (Con 1) (Con 0))
*** Exception: divide by zero
```

<!--v-->

### Motivating Examples

```haskell
-- Exceptions
data M a = Raise Exception
         | Return a
type Exception = String

-- State
type M a = State -> (a, State)
type State = Int

-- Execution Trace
data M a = (Output, a)
type Output = String
```

<!--v-->

### Evaluation With Exceptions

```haskell
data M a = Raise Exception
         | Return a
type Exception = String

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = case eval t of
                   Raise e -> Raise e
                   Return a ->
                      case eval u of
                        Raise e -> Raise e
                        Return b ->
                          if b == 0
                            then Raise "Divide by zero"
                            else Return (a / b)
```

Notice: it still looks like lambda calculus. In other words: it's still "pure"!

<!--v-->

> The original evaluator has the type `Term -> Int`, while in each variation its type took the form `Term -> M Int`. In general, a function of type `a -> b` is replaced by a function of type `a -> M b`. This can be read as a function that accepts an argument of type `a` and returns a result of type `b` with **a possible additional effect captured by `M`.**

<!--s-->

### What Is a Monad

- A data type describing a strategy to produce another type
- 2 operations
- 3 laws

<!--v-->

### Caution: Metaphors May Lead to Errors

"wrapper"

"action"

"context"

"side effect"

"strategy"

<!--v-->

### What's a Monad

- 2 operations
- 3 laws

<!--v-->

### The 2 Monad Operations in Haskell

```haskell
class Monad m where
  return :: a -> m a  -- called "unit" by Wadler
  (>>=) :: m a -> (a -> m b) -> m b  -- called "bind"
```

<!--v-->

```haskell
data ExceptM a = Raise Exception
              | Return a
type Exception = String

instance Monad ExceptM where

   return :: a -> ExceptM a
   return a = Return a

   (>>=) :: ExceptM a -> (a -> ExceptM b) -> ExceptM b
   m >>= k = case m of
               Raise e -> Raise e
               Return a -> k a
```

<!--v-->

### What the Exception Monad *Means*

1. We have the result from some computation that may have raised an error.
1. We want to pass this result to a new function, which may itself raise an error.

<!--v-->

### We Can Now Rework the Generic Evaluator

```haskell
eval :: (Monad m) => Term -> m Int
eval (Con a) = return a
eval (Div t u) = eval t >>=
                    \a -> eval u >>=
                        \b -> return (a `div` b)
```

<!--v-->

### Ordering and Side Effects

Wadler writes: "Perform computation `m` and bind `a` to the resulting value, and then perform computation `n`."

Ordering is important here: the side effects must happen in order.

<!--v-->

## Let's Try to Implement State

```haskell
type StateM a = State -> (a, State)
type State = Int
```

<!--v-->

```haskell
m >>= k :: M a -> (a -> M b) -> M b
m >>= k :: StateM a -> (a -> StateM b) -> StateM b

m = \startState -> (a, newState)

k :: a -> (State -> (b, State))

-- because of the associativity of function arrows, we can write:
k :: a -> State -> (b, State)
```

Notice: k is a function that takes two arguments.


<!--v-->

```haskell
m = \startState -> (a, newState)
k :: a -> State -> (b, State)

instance Monad State where
    return x = \state -> (x, state)
    m >>= k = \startState -> let (a, newState) = m startState
                                 (b, finalState) = k a newState
                             in  (b, finalState)
```

<!--v-->

### StateM `bind` Operation Summary

1. Run the first function in `m` with an argument to be provided.
1. Take the result *and* State from running the first function and pass both of those to `k`.
1. Return the final *wrapped* value from running `k`.

<!--s-->

## Section 3: Laws

All monad implementations must follow three laws:

- Left identity
- Right identity
- Associativity (in other words: it doesn't matter where you put the parentheses)

These laws keep us honest and prevent unexpected evaluation or strange things.

<!--v-->

### Left Identity

```haskell
return a >>= k = k a
```

Specialized to our Exceptions Example:

```haskell
return a :: a -> ExceptM a
k :: a -> ExceptM b
(>>=) :: ExceptM a -> (a -> ExceptM b) -> ExceptM b
```

<!--v-->

Left identity for `ExceptM` says the following:

1. Wrap up a value in the monad, `ExceptM a`, then
1. Pass the wrapped value to `bind`, which will unwrap it, and then
1. Pass the unwrapped `a` to `k`, which will then return a new wrapped value

This is the same as starting with an unwrapped value and passing it to `k` directly.

<!--v-->

### Right Identity

```haskell
m >>= return = m
```

Specialized to our Exceptions Example:

```haskell
ExceptM a >>= return
(>>=) :: ExceptM a -> (a -> ExceptM a) -> ExceptM a
```

<!--v-->

`bind` here computes the following:

1. Take an `Except a`
1. Unwrap the `Except` from the `a`
1. Pass the `a` to `return`, and
1. You end up what you started with: `Except a`.

<!--v-->

### Associativity

```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

Specialized to our Exceptions Example

<!--s-->

## Section 5: Parsers

```haskell
newtype Parser a = Parser { parse :: State -> [(a, State)] }
type State = String

item :: Parser Char
item = Parser go
  where go [] = []
        go (a:xs) = [(a, xs)]
```

<!--v-->

### Parsers Look Like StateM (from above)

```haskell
instance Monad Parser where
  return a = Parser (\x -> [(a, x)])
  m >>= k = Parser (\x ->
                      [(b, z) | (a, y) <- parse m x, (b, z) <- parse (k a) y])
```

<!--v-->

### Monadic Operators for Parsers: Sequencing

```haskell
twoItems :: Parser (Char, Char)
twoItems =
  item >>=
  \a -> item >>=
  \b -> return (a, b)
```

Similar to ideas present in `StateM`, there's an idea of composition or "andThen" here.

<!--v-->

### Wadler Builds Up Pretty Complex Parser by Combining Trivial Functions

```haskell
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
```

<!--s-->

## Why Are Monads Interesting

- Preservation of referential transparency: if `f` is a pure function, `f 3` will always return the same value.
- Allow side effects without impurity (except for IO) and make you *really aware* of side effects.
- "Out of the Tar Pit": Accidental and Necessary Complexity. One source of complexity: side effects.
- Thus, managing side effects is one strategy for managing bugs in software.

<!--v-->

> Whether a pure language (with monadic effects) is ultimately the best way to write programs is still an open question, but it certainly is a radical and elegant attack on the challenge of programming, and it was that combination of power and beauty that motivated the designers.

<!--v-->

> In retrospect, therefore, perhaps the biggest single benefit of laziness is not laziness per se, but rather that **laziness kept us pure**, and thereby motivated a great deal of productive work on monads and encapsulated state.

Simon Peyton Jones in "History of Haskell"

<!--v-->

### What Can Other (non-FP) Languages Learn from Monads

- They're probably already using them.
- Difference of terminology ("Maybe Monad" vs "andThen", "futures" and "callbacks").
- Mathematical terms link coding concepts to scholarship for the same ideas present in Mathematics.

<!--v-->

### Closing Points

- Monads are not impure.
- Monads are not simply about side effects.
- Monads are not simply a solution to the problem of IO.
- Edward Kmett (paraphrasing): "Monads are data structures with 2 operations and 3 laws. That's it."
- Trying to build intuition for *all* monads is doomed to failure.

<!--v-->

### Additional Readings

- Greg Michaelson: *Functional Programming Through Lambda Caculus*, 1989
- Philip Wadler, Simon Peyton Jones, "Imperative functional programming", 1992
- Paul Hudak, Simon Peyton Jones, John Hughes, Philip Wadler: "A History of Haskell: Being Lazy With Class", 2007
- Stephen Diehl, "What I Wish I Knew When Learning Haskell", http://dev.stephendiehl.com/hask/

<!--v-->

### Thank You

Erik Aker

Notes and code for this talk:

https://github.com/erewok/talks/tree/master/monads_for_fp_talk_2017