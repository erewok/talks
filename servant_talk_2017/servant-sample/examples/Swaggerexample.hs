{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server

import Lib

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

apiDocs :: API
apiDocs = docs counterApi

main :: IO ()
main = writeFile "./examples/docs.md" . markdown $ apiDocs
