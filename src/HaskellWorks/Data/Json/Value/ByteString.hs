{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Value.ByteString where

import qualified Data.ByteString as BS
import           Data.Map

data JsonValue
  = JsonString BS.ByteString
  | JsonNumber BS.ByteString
  | JsonObject (Map BS.ByteString JsonValue)
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)
