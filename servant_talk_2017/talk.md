## Describing the Elephant:
#### Using Servant to Serve, Request, and Document APIs (and more!)

---

## What Makes Servant Different or Interesting?

----

## There's a paper

http://alpmestan.com/servant/servant-wgp.pdf

(It's interesting and approachable)

----

## Misconceptions

People often describe Servant as a Haskell...

- web framwork
- web-client-generator
- library for making APIs

But these descriptions may be confusing.

----

## From the Servant paper's abstract

> ...Servant [is] an extensible, type-level DSL for describing Web APIs. Servant APIs are Haskell types. An API type can be interpreted in several different ways...

----

## "The Expression Problem"

For a good description of this problem, read "Data Types A La Carte":

http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

----

## Some Theoretical Takeaways

- APIs are type-safe
- Servant is meant to be open/extensible:
  - Users can invent new ways to *interpret* Servant API types
  - Users can *extend* the capabilities of Servant library: new combinators, new content-types, etc.

---

## Servant: A library (or DSL) for *Describing* APIs

[github.com/haskell-servant/servant](https://github.com/haskell-servant/servant)

----

## What does "describing web APIs" look like?

```haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import Servant

type CounterHome = "counter-html" :> Get '[HTML] Html
```

----

## A web endpoint/resource is a *type*

```haskell
type CounterHome = "counter-html" :> Get '[HTML] Html
```

Like a function type definition: if we go to this URI, we *GET* back something of the form *HTML*.

----

## We can also combine APIs

```haskell
-- A sample thing to accept and/or return
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)
instance ToJSON Counter
instance FromJSON Counter

type CounterHome = "counter-html" :> Get '[HTML] Html

type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter

type SampleApi = CounterHome :<|> CounterApi

```

----

## A web endpoint or resource as a *type*

```haskell
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)
instance ToJSON Counter
instance FromJSON Counter

type CounterHome = "counter-html" :> Get '[HTML] Html

type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter

type SampleApi = CounterHome :<|> CounterApi
```

How can we get a value of our API type?

----

## Proxy: a stand-in *value* for a type

```haskell

newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)
instance ToJSON Counter
instance FromJSON Counter

type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter

-- Here's the *value* representation of our CounterApi type
counterApi :: Proxy CounterApi
counterApi = Proxy
```

----

## An extended example:

```haskell
-- The *type* of our API
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter
    :<|> "counter-reset-post" :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
    :<|> "counter-queryparam" :> QueryParam "sortby" T.Text
        :> Header "Some-Header" T.Text :> Get '[JSON] Counter

-- Our API as a Servant value
counterApi :: Proxy CounterApi
counterApi = Proxy
```

----

## An extended example:

```haskell
-- The *type* of our API
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter
    :<|> "counter-reset-post" :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
    :<|> "counter-queryparam" :> QueryParam "sortby" T.Text
        :> Header "Some-Header" T.Text :> Get '[JSON] Counter

-- Our API as a Servant value
counterApi :: Proxy CounterApi
counterApi = Proxy
```

Next step: interpreting our API

---

## Interpretation I: Serving Servant

----

## Server Example

```haskell
-- Creating a counter that starts from 0
newCounter :: IO (TVar Counter)
newCounter = newTVarIO 0

-- Our Server
server :: TVar Counter -> Server SampleApi
server counter = counterHome counter
  :<|> currentValue counter
  :<|> counterMult counter
  :<|> counterReset counter
  :<|> counterWithParam counter

main :: IO ()
main = do
  cntr <- newCounter
  Warp.run 8000 (serve sampleApi $ server cntr)
```

Requires the dependency `servant-server` and `warp`.

----

## Server Handler example: HTML

```haskell
-- Our Server
type CounterHome = "counter-html" :> Get '[HTML] Html

server :: TVar Counter -> Server SampleApi
server counter = counterHome counter
  ...

-- Simple HTML Endpoint: We can return Servant's `Handler a` type
counterHome :: TVar Counter -> Handler H.Html
counterHome counter = do
  current <- liftIO . atomically $ readTVar counter
```

----

## Server Handler example: JSON POST

```haskell
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-reset-post" :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
    ...

-- Our Server
server :: TVar Counter -> Server SampleApi
server counter = ...
  :<|> counterReset counter
  ...

-- Reset Counter to POSTed Counter value
counterReset :: MonadIO m => TVar Counter -> Counter -> m Counter
counterReset oldCounter newCounter = liftIO . atomically $ do
  writeTVar oldCounter newCounter
  pure newCounter

```

---

## Some Difficulties

----

## Tough for Beginners:

- Lots of GHC Extensions
- Lots of typeclass and type family stuff

----


## Gnarly Error Messages

```
    • Couldn't match type ‘(Counter -> m0 Counter)
                           :<|> (Maybe T.Text -> Maybe T.Text -> m1 Counter)’
                     with ‘Maybe T.Text
                           -> Maybe T.Text
                           -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                ServantErr IO Counter’
      Expected type: Server SampleApi
        Actual type: transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                       ServantErr IO H.Html
                     :<|> (transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                             ServantErr IO Counter
                           :<|> ((Int
                                  -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                       ServantErr IO Counter)
                                 :<|> ((Int
                                        -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                             ServantErr IO Counter)
                                       :<|> ((Counter -> m0 Counter)
                                             :<|> (Maybe T.Text -> Maybe T.Text -> m1 Counter)))))
    • In the expression:
        counterHome counter
        :<|>
          currentValue counter
          :<|>
            counterPlusOne counter
            :<|>
              counterMult counter
              :<|> counterReset counter :<|> counterWithParam counter

```

----

## Some Common "Web Framework" Stuff is harder

- Authentication & sessions can be tough
- Uploading files is not quite implemented
- Returning Errors as JSON*


---

## Interpretation II: Generating Client code (Haskell, Javascript, Python...)

----

## Client Example (Python generator)

```haskell

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Servant.Foreign
import           System.FilePath
import           Servant.PY
import           Servant.PY.Python

import           Lib


instance HasForeignType Python B.ByteString Counter where
  typeFor _ _ _ = "{\"value\": int}"

-- where our static files reside
result :: FilePath
result = "examples"

main :: IO ()
main = writePythonForAPI counterApi requests (result </> "api.py")

```

---

## Interpretation III: Generating Swagger Docs

----

## Client Example

```haskell

```

Requires the dependency `servant-docs`.


---

## Other Interpretations

- Servant Quickcheck
- Generating Servant API types *from* Swagger Definitions
- Whatever else people dream up

---

## Thanks for listening!

Erik Aker
@erewok
github.com/erewok
