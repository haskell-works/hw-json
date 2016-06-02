{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskellWorks.Data.Json.FromValue where

import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Json.Value

class FromJsonValue a where
  fromJsonValue :: JsonValue -> Either DecodeError a

instance FromJsonValue JsonValue where
  fromJsonValue = Right

instance FromJsonValue String where
  fromJsonValue v = case v of
    JsonString r  -> Right r
    _             -> Left (DecodeError "Not a string")

instance FromJsonValue Int where
  fromJsonValue v = case v of
    JsonNumber r  -> Right (floor r)
    _             -> Left (DecodeError "Not an integer")

instance FromJsonValue Double where
  fromJsonValue v = case v of
    JsonNumber r  -> Right r
    _             -> Left (DecodeError "Not a double")

instance FromJsonValue Bool where
  fromJsonValue v = case v of
    JsonBool r    -> Right r
    _             -> Left (DecodeError "Not a boolean")

instance FromJsonValue a => FromJsonValue [a] where
  fromJsonValue v = case v of
    JsonArray xs  -> mapM fromJsonValue xs
    _             -> Left (DecodeError "Not an array")

instance (FromJsonValue a, FromJsonValue b) => FromJsonValue (a, b) where
  fromJsonValue v = case v of
    JsonArray (a:b:_) -> (,) <$> fromJsonValue a <*> fromJsonValue b
    _                 -> Left (DecodeError "Not a 2-tuple")

instance (FromJsonValue a, FromJsonValue b, FromJsonValue c) => FromJsonValue (a, b, c) where
  fromJsonValue v = case v of
    JsonArray (a:b:c:_) -> (,,) <$> fromJsonValue a <*> fromJsonValue b <*> fromJsonValue c
    _                   -> Left (DecodeError "Not a 3-tuple")

instance (FromJsonValue a, FromJsonValue b, FromJsonValue c, FromJsonValue d) => FromJsonValue (a, b, c, d) where
  fromJsonValue v = case v of
    JsonArray (a:b:c:d:_) -> (,,,) <$> fromJsonValue a <*> fromJsonValue b <*> fromJsonValue c <*> fromJsonValue d
    _                     -> Left (DecodeError "Not a 4-tuple")
