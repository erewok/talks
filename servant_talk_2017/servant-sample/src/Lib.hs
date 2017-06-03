{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           Data.Aeson
import           Data.Data
import qualified Data.Text                   as T
import           GHC.Generics
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5

-- * A simple Counter data type
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)
instance ToJSON Counter
instance FromJSON Counter

-- * Our Sample API type
type CounterHome = "counter-html" :> Get '[HTML] Html

type CounterApi = "counter-post" :> Post '[JSON] Counter
          :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter
          :<|> "counter-reset-post" :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
          :<|> "counter-queryparam" :> QueryParam "sortby" T.Text
            :> Header "Some-Header" T.Text :> Get '[JSON] Counter

counterApi :: Proxy CounterApi
counterApi = Proxy


type SampleApi = CounterHome :<|> CounterApi

-- This is how we get a *value* of our SampleApi type
sampleApi :: Proxy SampleApi
sampleApi = Proxy
