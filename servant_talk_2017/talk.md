# Describing the Elephant:
## Using the Servant Library to Serve, Request, and Document APIs (and more!)

----
# Why Is Servant Different or Interesting?


----

# Servant: A library for *Describing* APIs

[github.com/haskell-servant/servant](https://github.com/haskell-servant/servant)

----

# What does that look like?

----

```haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

type SampleApi = "login-html" :> Get '[HTML] Html
```

----

```haskell
type SampleApi = "login-html" :> Get '[HTML] Html
```

Notice: this is a *type*. It's a description of what we might call a *resource* or an *endpoint*.

If we go to this URI, we get back something of the form *HTML*. But how do you interact with this type: how do we pass a value of it to a function?

----

We *proxy* it:

```haskell
type SampleApi = "login-html" :> Get '[HTML] Html

-- This is how we get a *value* representation of our SampleApi type
sampleApi :: Proxy SampleApi
sampleApi = Proxy  
```

----

We can describe more endpoints:

```haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           Data.Aeson
import qualified Data.ByteString.Char8       as B
import           Data.Data
import qualified Data.Text                   as T
import           GHC.Generics
import           Servant
import           Servant.HTML.Blaze
import           System.Environment          (lookupEnv)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A


-- * A simple Counter data type
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)
instance ToJSON Counter

-- * Our Sample API type
type SampleApi ="login-html" :> Get '[HTML] Html
          :<|> "counter-req-header" :> Post '[JSON] Counter

-- This is how we get a *value* of our SampleApi type
sampleApi :: Proxy SampleApi
sampleApi = Proxy          
```


---

# There's a Servant paper

(You should read it: it's interesting)

http://alpmestan.com/servant/servant-wgp.pdf

----

# From the Servant paper's abstract

> We describe the design and motivation for Servant, an extensible, type-level DSL for describing Web APIs. Servant APIs are Haskell types. An API type can be interpreted in several different ways: as a server that processes requests, interprets them and dispatches them to appropriate handlers; as a client that can correctly query the endpoints of the API; as systematic documentation for the API; and more.
