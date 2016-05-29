{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Json.Value.Internal where

import qualified Data.ByteString                                            as BS
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Json.Succinct.Index
import qualified Data.Attoparsec.ByteString.Char8                           as ABC

data GenJsonValue s
  = JsonString s
  | JsonNumber Double
  | JsonObject [(s, GenJsonValue s)]
  | JsonArray [GenJsonValue s]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class GenJsonValueAt s a where
  jsonValueAt :: a -> Maybe (GenJsonValue s)

class FromJsonIndex a where
  fromJsonIndex :: JsonIndex BS.ByteString -> Either DecodeError a

instance FromJsonIndex (GenJsonValue BS.ByteString) where
  fromJsonIndex i = case i of
    JsonByteStringString  s   -> Right (JsonString s)
    JsonByteStringNumber  s   -> case ABC.parse ABC.rational s of
      ABC.Fail    {}  -> Left (DecodeError "Invalid number")
      ABC.Partial _   -> Left (DecodeError "Unexpected end of number")
      ABC.Done    _ r -> Right (JsonNumber r)
    JsonByteStringObject  fs  -> JsonObject <$> mapM (\(k, v) -> (k, ) <$> fromJsonIndex v) fs
    JsonByteStringArray   es  -> JsonArray <$> mapM fromJsonIndex es
    JsonByteStringBool    v   -> Right (JsonBool v)
    JsonByteStringNull        -> Right JsonNull
