{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Main where


import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                   as T
import           GHC.Generics
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Lib

-- Our SampleApi type for reference:
-- type CounterHome = "counter-html" :> Get '[HTML] Html
-- type CounterApi = "counter-post" :> Post '[JSON] Counter
--           :<|> "counter-multiplier" :> Capture "mult" Int :> Post '[JSON] Counter
--           :<|> "counter-reset-post" :> ReqBody '[JSON] Counter :> Post '[JSON] Counter
--           :<|> "counter-queryparam" :> QueryParam "negate" T.Text
--             :> Header "Some-Header" T.Text :> Get '[JSON] Counter

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


-- Our handlers referenced by the server definition

-- Simple HTML Endpoint: We can return Servant's `Handler a` type
counterHome :: TVar Counter -> Handler H.Html
counterHome counter = do
  current <- liftIO . atomically $ readTVar counter
  pure $ H.docTypeHtml $ H.body $ H.p $ (H.string . show . value) current

-- Get and return current value as JSON:
-- Alternately we can return something inside MonadIO
currentValue ::  MonadIO m => TVar Counter -> m Counter
currentValue counter = do
  current <- liftIO . atomically $ readTVar counter
  pure current

-- Counter multiplied
counterMult :: MonadIO m => TVar Counter -> Int -> m Counter
counterMult counter mult = liftIO . atomically $ do
  oldValue <- readTVar counter
  let newValue = Counter $ (value oldValue) * mult
  writeTVar counter newValue
  pure newValue

-- Reset Counter to POSTed Counter value
counterReset :: MonadIO m => TVar Counter -> Counter -> m Counter
counterReset oldCounter newCounter = liftIO . atomically $ do
  writeTVar oldCounter newCounter
  pure newCounter

-- Sample QueryParam, Sample Header argument
counterWithParam :: MonadIO m => TVar Counter -> Maybe T.Text -> Maybe T.Text -> m Counter
counterWithParam counter param header = liftIO . atomically $ do
  oldValue <- readTVar counter
  let newValue = if (isJust param) then -1 * oldValue else oldValue
  writeTVar counter newValue
  pure newValue
