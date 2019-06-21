{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
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

data JsonValue
  = JsonString Text
  | JsonNumber Double
  | JsonObject [(Text, JsonValue)]
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class JsonValueAt a where
  jsonValueAt :: a -> Either DecodeError JsonValue

instance JsonValueAt JsonIndex where
  jsonValueAt i = case i of
    JsonIndexString  s  -> case ABC.parse parseJsonText s of
      ABC.Fail    {}  -> Left (DecodeError ("Invalid string: '" ++ show (BS.take 20 s) ++ "...'"))
      ABC.Partial _   -> Left (DecodeError "Unexpected end of string")
      ABC.Done    _ r -> Right (JsonString r) -- TODO optimise
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
    where parseText bs = case ABC.parse parseJsonText bs of
            ABC.Fail    {}  -> Left (DecodeError ("Invalid field: '" ++ show (BS.take 20 bs) ++ "...'"))
            ABC.Partial _   -> Left (DecodeError "Unexpected end of field")
            ABC.Done    _ s -> Right s
