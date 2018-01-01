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

github.com/erewok
-> talks/tree/master/monads_for_fp_talk_2017

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
1. Some Notes on Haskell
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

<!--s-->

> Pure languages, such as Miranda and Haskell, are lambda calculus pure and simple.

<!--v-->

## Lambda Calculus


```haskell
𝝺x.x
```

The identity function

<!--v-->

> ...the value of an expression depends only on its free variables.

```haskell
𝝺bound.(free bound)
```

<!--v-->

## Other Combinators

```haskell
𝝺s.(s s)
```

The "self-application function"

<!--v-->

## More Combinators (With Names)

```haskell
def select_first = 𝝺first.𝝺second.first

def apply = 𝝺func.𝝺arg.(func arg)

def make_pair = 𝝺first.𝝺second.𝝺func((func first) second)
```

<!--v-->

### Beta Reduction

```haskell
 𝝺x.x 𝝺s.(s s)
  -> 𝝺s.(s s)
```

We can't reduce any more so this equation is said to be in "normal form."

<!--v-->

### Another Beta Reduction

```haskell
𝝺s.(s s) 𝝺x.x
  -> (𝝺x.x 𝝺x.x)
    -> 𝝺x.x
```

<!--v-->

Haskell as Lambda Calculus:

```haskell
add :: Int -> (Int -> Int)
add x y = x + y

-- this is equivalent to...
add' = \x -> \y -> x + y
```

<!--s-->

### Primary Goals for Haskell Implementers

- Desire for a *research* programming language for PL researchers to use.
- Based on Typed Lambda Calculus.
- Laziness.

**Purity** is a side effect of laziness.

<!--v-->

> An immediate consequence of laziness is that evaluation order is demand-driven. As a result, it becomes more or less impossible to *reliably* perform input/output or other side effects as the result of a function call.

"History of Haskell: Being Lazy with Class"

<!--v-->

### Haskell's Side Effect Problem

- Print to the screen
- Take input from users
- Read files
- Write files

Monads became the primary solution for these problems.

<!--s-->

### Monads Paper Section 1: Introduction

- Monads "integrate impure effects into pure functional languages"
- Origins in Category Theory

<!--v-->

### Example Computation: Evaluator

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

### Evaluator with three motivating examples

- Add error handling
- Count operations
- Add execution trace (printing, debugging)

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

<!--v-->

State and Execution Trace look similar.

There's a pattern here...

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

- A data type
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

(almost)

<!--v-->

### Evaluator that may `Raise e`

```haskell
data ExceptM a = Raise Exception | Return a
type Exception = String

-- `eval` alone won't properly handle "divide by zero"
evalExceptM :: Term -> ExceptM Int
evalExceptM (Con a) = return a
evalExceptM (Div t u) =
  evalExceptM t >>=
  \a -> evalExceptM u >>=
  \b -> if b == 0
           then raise "divide by zero error"
           else return (a `div` b)
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
instance Monad StateM where
    return :: a -> StateM a
    return x = \state -> (x, state)

    (>>=) :: StateM a -> (a -> StateM b) -> StateM b
    m >>= k = \startState -> let (a, newState) = m startState
                                 (b, finalState) = k a newState
                             in  (b, finalState)

k :: a -> (State -> (b, State))
-- k takes an `a` and returns a function from `State -> (b, State)`
```

<!--v-->

### What StateM *Means*

1. Run the first *stateful computation* (`m`, with an argument to be provided).
1. Take the result *and* State from running the first function and run another *stateful computation*.
1. This is like *function composition*.

<!--v-->

## The StateM Evaluator

```haskell
-- helper function for ticking along the state
tick :: StateM ()
tick = StateM (\x -> ((), x+1))

evalState' :: Term -> StateM Int
evalState' (Con a) = return a
evalState' (Div t u) =
  evalState' t >>=
  \a -> evalState' u >>=
  \b -> tick >>=
  \_ -> return (a `div` b)
```

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

<!--v-->

### Right Identity

```haskell
m >>= return = m
```

<!--v-->

### Associativity

```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

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

### Parsers Look Somewhat Like State Monad

```haskell
instance Monad Parser where
  return a = Parser (\x -> [(a, x)])
  m >>= k =
    Parser (\x ->
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

letter :: Parser Char
letter = item ▻ isLetter

digit :: Parser Int
digit = (item ▻ isDigit) >>= \a -> return (ord a - ord '0')

lit :: Char -> Parser Char
lit c = item ▻ (\a -> a == c)
```

<!--s-->

## Why Are Monads Interesting

- Allow side effects without impurity (except for IO).
- Force awareness/handling of side effects.
- Preservation of referential transparency.

(See "Out of the Tar Pit" for discussion of accidental and necessary complexity, and side effects.)

<!--v-->

> Whether a pure language (with monadic effects) is ultimately the best way to write programs is still an open question, but it certainly is a radical and elegant attack on the challenge of programming, and it was that combination of power and beauty that motivated the designers.

<!--v-->

> In retrospect, therefore, perhaps the biggest single benefit of laziness is not laziness per se, but rather that **laziness kept us pure**, and thereby motivated a great deal of productive work on monads and encapsulated state.

"History of Haskell: Being Lazy with Class"

<!--v-->

### What Can Other (non-FP) Languages Learn from Monads

- Isolation and awareness of side effects.
- They're probably already using them.
- Difference of terminology ("Maybe Monad" vs "andThen", "futures" and "callbacks").
- Mathematical terms link coding concepts to scholarship for the same ideas present in Mathematics.

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