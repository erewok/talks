{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
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


data LoginForm = LoginForm
 { username     :: !T.Text
 , password     :: !T.Text
 , otherMissing :: Maybe T.Text
 } deriving (Eq, Show, Generic, Typeable, Data)
instance ToJSON LoginForm
instance FromJSON LoginForm


-- * Our Sample API type
type SampleApi ="login-html" :> Get '[HTML] Html
          :<|> "counter-req-header" :> Post '[JSON] Counter
          :<|> "counter-queryparam" :> QueryParam "sortby" T.Text
            :> Header "Some-Header" T.Text :> Get '[JSON] Counter
          :<|> "login-queryflag" :> QueryFlag "published" :> Get '[JSON] LoginForm
          :<|> "login-params-authors-with-reqBody"
            :> QueryParams "authors" T.Text
            :> ReqBody '[JSON] LoginForm :> Post '[JSON] LoginForm
          :<|> "login-with-path-var-and-header"
            :> Capture "id" Int
            :> Capture "Name" T.Text
            :> Capture "hungrig" Bool
            :> ReqBody '[JSON] LoginForm
            :> Post '[JSON] (Headers '[Header "test-head" B.ByteString] LoginForm)

-- This is how we get a *value* of our SampleApi type
sampleApi :: Proxy SampleApi
sampleApi = Proxy
