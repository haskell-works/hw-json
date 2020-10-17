{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Value where

import Data.Text                             (Text)
import HaskellWorks.Data.Json.DecodeError
import HaskellWorks.Data.Json.Internal.Index
import HaskellWorks.Data.Json.Internal.Value

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as BS
import qualified Data.Text                        as T

-- | Traditional JSON type.
--
-- This type has the a constructor per JSON data type as is typical for JSON in most libraries.
--
-- See 'jsonValueAt' on how to parse JSON text into this datatype.
--
-- Although Haskell data types are lazy by default, you will not get a fully lazy data structure
-- when parsing to this type because there is no way to express parsing errors in this datatype.
--
-- For a data type that gives you lazier behaviour, see other alternatives such as
-- 'HaskellWorks.Data.Json.PartialValue.JsonPartialValue' or 'HaskellWorks.Data.Json.LightJson.LightJson'.
data JsonValue
  = JsonString Text
  | JsonNumber Double
  | JsonObject [(Text, JsonValue)]
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class JsonValueAt a where
  -- | Get a JSON value from another type
  --
  -- The @hw-json@ library does not do full JSON validation for efficiency reasons, but parsing can
  -- fail if the JSON is malformed.  When parsing fails, then 'Left' will be returned.
  --
  -- If 'Right' is returned then that means there are no parsing failures, which implies "knowing"
  -- that there parsing failures in the entire document, which implies that pattern matching on
  -- 'Right' evaluates the entire document.
  --
  -- This limits the laziness of the JSON parsing.  For lazier alternatives, see
  -- 'HaskellWorks.Data.Json.PartialValue.jsonPartialJsonValueAt' or 'HaskellWorks.Data.Json.LightJson.lightJsonAt'.
  jsonValueAt :: a -> Either DecodeError JsonValue

instance JsonValueAt JsonIndex where
  jsonValueAt i = case i of
    JsonIndexString  s  -> case ABC.parse parseJsonString s of
      ABC.Fail    {}  -> Left (DecodeError ("Invalid string: '" ++ show (BS.take 20 s) ++ "...'"))
      ABC.Partial _   -> Left (DecodeError "Unexpected end of string")
      ABC.Done    _ r -> Right (JsonString (T.pack r)) -- TODO optimise
    JsonIndexNumber  s  -> case ABC.parse ABC.rational s of
      ABC.Fail    {}    -> Left (DecodeError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'"))
      ABC.Partial f     -> case f " " of
        ABC.Fail    {}  -> Left (DecodeError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'"))
        ABC.Partial _   -> Left (DecodeError "Unexpected end of number")
        ABC.Done    _ r -> Right (JsonNumber r)
      ABC.Done    _ r   -> Right (JsonNumber r)
    JsonIndexObject  fs -> JsonObject <$> mapM (\f -> (,) <$> parseText (fst f) <*> jsonValueAt (snd f)) fs
    JsonIndexArray   es -> JsonArray <$> mapM jsonValueAt es
    JsonIndexBool    v  -> Right (JsonBool v)
    JsonIndexNull       -> Right JsonNull
    where parseText bs = T.pack <$> parseString bs -- TODO optimise
          parseString bs = case ABC.parse parseJsonString bs of
            ABC.Fail    {}  -> Left (DecodeError ("Invalid field: '" ++ show (BS.take 20 bs) ++ "...'"))
            ABC.Partial _   -> Left (DecodeError "Unexpected end of field")
            ABC.Done    _ s -> Right s
