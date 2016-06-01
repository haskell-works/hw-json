module HaskellWorks.Data.Json.FromValue where

import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Json.Value

class FromJsonValue a where
  fromJsonValue :: JsonValue -> Either DecodeError a

instance FromJsonValue JsonValue where
  fromJsonValue = Right
