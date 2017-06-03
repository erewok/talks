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
