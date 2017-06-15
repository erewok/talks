## Describing the Elephant:
#### Using Servant to Serve, Request, and Document APIs (and more!)

Erik Aker
@erewok

----

## Administrivia

All code for this talk available here:

github.com/erewok/talks/tree/master/servant_talk_2017

---

## What Makes Servant Different or Interesting?

----

## Misconceptions

People often describe Servant as a Haskell...

- web framework
- web-client-generator
- library for making APIs

But these descriptions may be confusing.

----

## There's a paper

http://alpmestan.com/servant/servant-wgp.pdf

(It's interesting and approachable)

----

## From the Servant paper's abstract

> ...Servant [is] an extensible, type-level DSL for describing Web APIs. Servant APIs are Haskell types. An API type can be interpreted in several different ways...

----

## "The Expression Problem"

How to create "open" data types/functions?

For a good description, read "Data Types A La Carte":

http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

----

## Working with Servant: Core Ideas

1) "Describe" an API and 2) Build interpretations


Interpretations can include:

- Running a server
- Generating clients (in any programming language)
- Documentation
- Testing

----

## Some Theoretical Takeaways

- APIs are type-safe
- Servant is meant to be open/extensible:
  - Users can *extend* the capabilities of Servant library: new combinators, new content-types, etc.
  - Users can invent new ways to *interpret* Servant API types



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

## We can also combine APIs

```haskell
-- A sample thing to accept and/or return
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num, ToJSON, FromJSON)

type CounterHome = "counter-html" :> Get '[HTML] Html
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier"
         :> Capture "mult" Int
         :> Post '[JSON] Counter
    :<|> "counter-reset-post"
         :> ReqBody '[JSON] Counter
         :> Post '[JSON] Counter

type SampleApi = CounterHome :<|> CounterApi

```

----

## A web endpoint or resource is a *type*

```haskell
newtype Counter = Counter { value :: Int }...

type CounterHome = "counter-html" :> Get '[HTML] Html
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier"
         :> Capture "mult" Int
         :> Post '[JSON] Counter
    :<|> "counter-reset-post"
         :> ReqBody '[JSON] Counter
         :> Post '[JSON] Counter

type SampleApi = CounterHome :<|> CounterApi
```

This means we can make assertions about it:
    - we can know a client is matched to a server or
    - we can even assert that two APIs are equivalent.

----

## Q: How can we get a value of our API type?

A: Proxy, a stand-in *value* for a type

```haskell
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-multiplier"
         :> Capture "mult" Int
         :> Post '[JSON] Counter
    :<|> "counter-reset-post"
         :> ReqBody '[JSON] Counter
         :> Post '[JSON] Counter

-- Here's the *value* representation of our CounterApi type
counterApi :: Proxy CounterApi
counterApi = Proxy
```

----

## An extended example:

```haskell
-- The *type* of our API
type CounterApi = "counter-post" :> Post '[JSON] Counter
  :<|> "counter-multiplier"
    :> Capture "mult" Int :> Post '[JSON] Counter
  :<|> "counter-reset-post"
    :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
  :<|> "counter-queryparam"
    :> QueryParam "sortby" T.Text
    :> Header "Some-Header" T.Text
    :> Get '[JSON] Counter

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
newCounter :: IO (TVar Counter)
newCounter = newTVarIO 0  -- Create counter; start at 0

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

Requires the dependencies `servant-server`, `wai`, and `warp`.

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
  pure $ H.docTypeHtml $ H.body $ H.p $ (H.string . show . value) current
```

----

## Server Handler example: JSON POST

```haskell
type CounterApi = "counter-post" :> Post '[JSON] Counter
    :<|> "counter-reset-post"
        :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
    ...

-- Our Server
server :: TVar Counter -> Server SampleApi
server counter = ...
  :<|> counterReset counter
  ...

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
- Lots of Monad Transformers in typical servant-server code.

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
- Testing??

*I have a gist for an example of this

---

## Interpretation II: Generating Client code (Haskell, Javascript, Python...)

----

## Client Example

```haskell
module Main where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

import Lib

-- Haskell client generation
postCounter
  :<|> multiplier
  :<|> reset
  :<|> paramCounter = client counterApi
```

The `client` function creates a bunch of client functions for each endpoint in our API.

----

```haskell
queries :: ClientM (Counter, Counter, Counter)
queries = do
  initial <- reset $ Counter 10
  multCount <- multiplier 5
  post <- postCounter
  reset2 <- reset $ Counter 20
  multCount2 <- multiplier 5
  return (initial, multCount, post, reset2, multCount2)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (initial, multCount, post, reset2, multCount2) -> do
      print initial
      print multCount
      print post
      print reset2
      print multCount2
```

----

## In action...

```
❯ stack exec servant-sample-client
Counter {value = 10}
Counter {value = 50}
Counter {value = 50}
Counter {value = 20}
Counter {value = 100}
```

----

## We can also use servant-foreign to write client generators for any language

```haskell
instance HasForeignType Python B.ByteString Counter where
  typeFor _ _ _ = "{\"value\": int}"

main :: IO ()
main = do
  -- test out the Haskell clients
  run
  -- Write out a Python module with client code
  writePythonForAPI counterApi requests ("examples" </> "api.py")
```

(Here's the Python-code-generator).

---

## Interpretation III: Generating (and serving) Docs

----

## Docs Example

```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}
...
instance ToCapture (Capture "mult" Int) where
  toCapture _ =
    DocCapture "mult"                                 -- name
               "(integer) to multiply our counter by" -- description

instance ToSample Counter where
  toSamples _ = singleSample (Counter 5) -- example of output


instance ToParam (QueryParam "sortby" T.Text) where
  toParam _ =
    DocQueryParam "sortby"                     -- name
                  ["val", "..."] -- example of values (not necessarily exhaustive)
                  "A dummy query param we're not even using." -- description
                  Normal -- Normal, List or Flag
```

Requires the dependency `servant-docs`.

----

## Running the docs generator

```haskell
apiDocs :: API
apiDocs = docs counterApi

main :: IO ()
main = writeFile "./examples/docs.md" . markdown $ apiDocs
```

----

## Documentation!

```
## POST /counter-multiplier/:mult

Clients must supply the following data

#### Captures:

- *mult*: (integer) to multiply our counter by

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"value":5}
```

---

## Other Interpretations

- Clients in Javascript (Angular, jQuery, React, etc.) and other languages.
- Servant Quickcheck: quickcheck that two APIs are equal or no 500s!
- Generating Servant API types *from* Swagger Definitions.
- Whatever else people dream up.

---

## Thanks for listening!

Erik Aker

@erewok

github.com/erewok
