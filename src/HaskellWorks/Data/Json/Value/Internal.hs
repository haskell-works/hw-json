{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Value.Internal where

import Data.Map

data GenJsonValue s n
  = JsonString s
  | JsonNumber n
  | JsonObject (Map s (GenJsonValue s n))
  | JsonArray [GenJsonValue s n]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class GenJsonValueAt s n a where
  jsonValueAt :: a -> Maybe (GenJsonValue s n)
