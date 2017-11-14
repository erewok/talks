# Monads for Functional Programming

Philip Wadler, 1995

"A monad is just a monoid in the category of endofunctors, what's the problem?"

- from "A Brief, Incomplete, and Mostly Wrong History of Programming Languages"

## Some Questions

- Why did they create programming languages with this problem? In other words, why “purity”?
- How does the monad work?
- Why is the solution interesting beyond the world of Haskell?

## Plan of Attack

- A short history of Functional Programming
- A bit of Lambda Calculus
- Some Haskell
- Close reading of Sections 1 through 3 of the paper
- Light reading of Section 5

## Chronology of Functional Programming

What's a functional programming language? A language based on Lambda Calculus.

- Lisp, John McCarthy 1958
- Scheme, 1970, a Lisp deliberately more closely aligned with Lambda Calculus
- John Backus, 1978 Turing Award Lecture: "Can Programming Be Saved from the Von Neumann Style?"
- ML, 1973, Robin Milner
- Early 80s, Lisp conferences become "Lisp and Functional Programming" conferences
- Mid-80s, Lots of Pure, Lazy FP languages (Miranda et. al.); Simon Peyton Jones calls this period "a tower of Babel".
- 1987: First Haskell meeting to create a pure, lazy functional programming language for everyone to use.

## Some Broader Context

The 80s:

- End of Moore's Law in sight.
- Future to include massive parallel computations (with desire for simplified programming models for parallel computing).
- Desire for languages with rigorous mathematical and theoretical foundations ("discovered" languages).
- Desire for a *research* programming language for PL researchers to use.

"Most of you use programming languages that were invented and you can tell."

- Philip Wadler in "Propositions As Types", Strange Loop Conference, 2014

## Lambda Calculus Defined

- Functions only
- One bound variable
- Function body
- Only functions allowed so all functions are higher order.

Free (not bound) variables may also appear.

## Some Lambda Calculus

The identity function:

```haskell
𝝺x.x
```

The "self-application function":

```haskell
𝝺s.(s s)
```

## A beta reduction (calculating results)

```haskell
𝝺s.(s s) 𝝺x.x
  -> (𝝺x.x 𝝺x.x)
    -> 𝝺x.x
```

## Representing Bools

```haskell
def true = 𝝺first.𝝺second.first
def false = 𝝺first.𝝺second.second
```

## Representing Numbers

If we have a `0` and `succ` function, then `1` is `succ 0` and `2` is `succ 1`, etc.

```haskell
def zero = 𝝺x.x
def succ = 𝝺n.𝝺s((s false) n)

-- one, for example, substitute `false` for definition above
def one = 𝝺s((s false) 𝝺x.x)
 -> 𝝺s((s 𝝺first.𝝺second.second) 𝝺x.x)

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

## Haskell is Lambda Calculus

Lambda Calculus ->
  Simply Typed Lambda Calculus (no polymorphic functions) ->
     System F (polymorphism) ->
       Haskell

Wadler: "Pure languages are lambda calculus pure and simple"

## Consider the Identity Function

```haskell
ident :: a -> a
ident ...
```

Here `a` is a type variable. This function is polymorphic so `a` can be anything in the Haskell universe.

Only one valid definition of this function (Haskellers don't even need to see the function body to know it):

```haskell
ident :: a -> a
ident arg = arg
```

## Haskell's Purity

Consider a function with this type signature:

```haskell
square :: Double -> Double
```

This function *cannot* operate on anything but the `Double` that is its first argument and it can do nothing outside of returning a `Double`. It cannot print to the screen. It cannot read a file or write a file. It can't talk to the internet. Every invocation with a particular argument will *always* return the same result.

"A function can only read what is supplied to it in its arguments and the only way it can have an effect on the world is through the values it returns." (Dan Piponi, "You Could Have Invented Monads")

## From "History of Haskell": Purity is a result of laziness

"An immediate consequence of laziness is that evaluation order is demand-driven. As a result, it becomes more or less impossible to reliably perform input/output or other side effects as the result of a function call. Haskell is, therefore, a pure language. For example, if a function `f` has type `Int -> Int` you can be sure that `f` will not read or write any mutable variables, nor will it perform any input/output. In short, `f` really is a function in the mathematical sense: every call (`f 3`) will return the same value.

"Whether a pure language (with monadic effects) is ultimately the best way to write programs is still an open question, but it certainly is a radical and elegant attack on the challenge of programming, and it was that combination of power and beauty that motivated the designers. In retrospect, therefore, perhaps the biggest single benefit of laziness is not laziness per se, but rather that **laziness kept us pure**, and thereby motivated a great deal of productive work on monads and encapsulated state."

## Monads Paper Section 1: Introduction

- Wadler was inspired by Eugenio Moggi's work on monads
- Monads are used to "integrate impure effects into pure functional languages"

Haskell had a side effect problem. Monads were a useful abstraction that *helped to solve that problem* among other problems.

Category Theory also makes other appearances in Haskell: Functor, Applicative, Monad, Monoid, etc.

## Section 2: Evaluating Monads

A simple evaluator (it divides two terms) with three motivating "side effect" examples:

- Add error handling
- Count operations
- Add execution trace (for instance, to print the evaluation)

## Simple Evaluator: First Attempt

```haskell
data Term = Con Int
          | Div Term Term

eval :: Term -> Int
eval (Con a) = a
eval (Div t u) = eval t `div` eval u
```

`Term` is a recursive data structure: it may contain nested `Term`s. When we evaluate `Div`, we must evaluate the left and right sides before running the `div` operation.

```haskell
-- BOOM
λ> eval (Div (Con 1) (Con 0))
*** Exception: divide by zero
```

## Motivating Examples: Exceptions, State, Execution Trace

Wadler defines three new, independent types to wrap our `Term` types to perform these side effects:

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

In each case, we wrap a computation `a` with some auxiliary computation, which represents a "side effect".

## Evaluation With Exceptions

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

Notice: it still looks like lambda calculus and follows equational reasoning. In other words: it's still "pure"!

## There's a Pattern in the Examples: They Are All "Monadic Evaluators"

"The original evaluator has the type `Term -> Int`, while in each variation its type took the form `Term -> M Int`. In general, a function of type `a -> b` is replaced by a function of type `a -> M b`.

"This can be read as a function that accepts an argument of type `a` and returns a result of type `b` with **a possible additional effect captured by `M`.**"

## What Is a Monad

- A data type that wraps, contains, or provides context for another type.
- 2 operations
- 3 laws

In Haskell the two required operations have these type signatures:

```haskell
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

The first is called `unit` and the second is called `bind` (with apologies for the confusion over `return` and the infix operator `>>=`).

## The Two Monad Operations for Exceptions

```haskell
data Except a = Raise Exception
              | Return a
type Exception = String

instance Monad Except where

   return :: a -> Except a
   return a = Return a

   (>>=) :: Except a -> (a -> Except b) -> Except b
   m >>= k = case m of
               Raise e -> Raise e
               Return a -> k a
```

Look closely at the following: `(a -> m b)`.

The type signatures and definitions say:

1. We have the result from some computation that may have raised an error.
1. We want to pass this result to a new, pure function, which may itself raise an error.

Thus, `bind` unwraps the first computation and passes the unwrapped value to the second, which then returns a wrapped result.

## Ordering and Side Effects

Wadler writes: "Perform computation `m` and bind `a` to the resulting value, and then perform computation `n`."

Ordering is important here: the side effects must happen in order.

## We Can Now Rework the Generic Evaluator

`eval` will now work on *any* monadic value. It needs know _nothing_ about the particular side effect or context involved:

```haskell
eval :: (Monad m) => Term -> m Int
eval (Con a) = return a
eval (Div t u) = eval t >>=
                    \a -> eval u >>=
                        \b -> return (a / b)
```

Equational reasoning: "unfolding the definitions and simplifying" yields the evaluator given in the previous section.

## How Do We Implement the Monad Instance for State

The key is always in how the two operations `return` and `>>=` (also called `bind`) are implemented:

```haskell
type StateM a = State -> (a, State)
type State = Int

instance Monad State where
    return x = \state -> (x, state)
    m >>= k = \startState -> let (a, newState) = m startState
                                 (b, finalState) = k a newState
                             in  (b, finalState)
```

The `bind` instance here may be tough to interpret, but we can let the types guide us. Here's a more fleshed-out version of the `bind`, which may make it easier to see what's happening.

First, `m` is whatever value we're wrapping, included inside a function. It's also waiting for the state argument:

```haskell
m = \startState -> (a, startState)
```

`k` is the middle argument to bind, something like this:

```haskell
k :: a -> M b
k :: a -> (newState -> (a, newState))

-- because of the associativity of function arrows, we can write it like this:
k :: a -> newState -> (a, newState)
```

This is why `k a newState` appears in the definition above.

`bind` does the following:

1. Run the first function in `m` with an argument to be provided.
1. Take the result and State from running that function and pass those to `k`.
1. Return the final wrapped value from running `k`.

The function type ends up being something like this:

```haskell
(>>=) :: M a -> (a -> M b) -> M b
(>>=) :: (s -> (a, s)) -> (a -> t -> (b, t)) -> (t -> (b, t))
```

Another key takeway: both the `M a` at the beginning and `M b` as the final return value are functions in this monadic instance. Thus, `bind` will also return a function that's waiting for an argument. It needs a starting state to kick off the computation.

## Section 3: Laws

All monad implementations must follow three laws:

- Left identity
- Right identity
- Associativity (in other words: it doesn't matter where you put the parentheses)

These laws keep us honest and prevent unexpected evaluation or strange things. However, Haskell won't tell you if your `Monad` instance is lawful, but it is considered bad form in the community *not* to have a lawful instance.

## Left Identity

```haskell
return a >>= k = k a
```

Specialized to our Exceptions Example:

```haskell
return a :: a -> Except a

(>>=) :: Except a -> (a -> Except b) -> Except b

k :: a -> Except b
```

This says the following:

1. Wrap up a value in the monad, `Except a`, then
1. Pass the wrapped value to `bind`, which will unwrap it, and then
1. Pass the unwrapped `a` to `k`, which will then return a new wrapped value

This is the same as starting with an unwrapped value and passing it to `k` directly.

## Right Identity

```haskell
m >>= return = m
```

Specialized to our Exceptions Example:

```haskell
Except a >>= return
(>>=) :: Except a -> (a -> Except b) -> Except b
```

`bind` in this case will unwrap the left side `a` and pass it to the function `return`, which returns `Except a`.

Thus, right identity says this:

1. Take an `Except a`
1. Unwrap the `Except` from the `a`
1. Pass the `a` to `return`, and
1. You end up what you started with: `Except a`.

## Associativity

```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)

```

Specialized to our Exceptions Example:

```haskell
Except a >>= \x -> f x >>= g
```

 TO DO

## Section 5: Parsers

 TO DO


## Why Are Monads Interesting

"Out of the Tar Pit": Accidental and Necessary Complexity

One source of complexity: side effects.

Thus, managing side effects is one strategy for managing bugs in software.

## Closing Points

- Monads are not simply a solution to the problem of IO.
- Monads are not impure.
- Monads are not only about side effects.
- Trying to build intuition for *all* monads is doomed to failure.
- The only way to understand all monads is to say, "they wrap something and perform two operations and follow 3 laws."

For the latter point: imagine a class in an OOP language that had two particular methods. Now imagine trying to suggest that one could understand *every* class that *also* implemented these two methods. There's no unifying principal, *except the two methods*: how they get implemented is up to the implementer.

## Additional Readings

- Greg Michaelson: *Functional Programming Through Lambda Caculus*, 1989
- Paul Hudak, Simon Peyton Jones, John Hughes, Philip Wadler: "A History of Haskell: Being Lazy With Class", 2007
- Philip Wadler: "Propositions as Types", 2014
- Stephen Diehl, "What I Wish I Knew When Learning Haskell", http://dev.stephendiehl.com/hask/
- "Typeclassopedia", https://wiki.haskell.org/Typeclassopedia