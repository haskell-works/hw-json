{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Value where

import Data.Map

data JsonValue s n
  = JsonString s
  | JsonNumber n
  | JsonObject (Map s (JsonValue s n))
  | JsonArray [JsonValue s n]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class JsonValueAt s n a where
  jsonValueAt :: a -> Maybe (JsonValue s n)
