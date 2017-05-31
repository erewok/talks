{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Jsexample where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Data
import qualified Data.Text             as T
import           GHC.Generics
import           Servant
import           Servant.Foreign
import           System.FilePath

import           Lib

-- You could provide a direct instance of your types to represent them as strings
-- instance HasForeignType Python T.Text LoginForm where
--   typeFor _ _ _ = "{\"username\": str, \"password\": str, \"otherMissing\":  Maybe Text}"


-- instance HasForeignType Python T.Text Counter where
--   typeFor _ _ _ = "{\"value\": int}"

-- Alternately, if you make your instance derive Typeable and Data, and
-- you enable the pragma DeriveDataTypeable,
-- then you can use recordToDict to automatically derive your records
instance HasForeignType Python T.Text LoginForm where
  typeFor _ _ _ = recordToDict (undefined :: LoginForm) LoginForm

instance HasForeignType Python T.Text Counter where
  typeFor _ _ _ = recordToDict (undefined :: Counter) Counter



main :: IO ()
main = someFunc
