{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Json.Internal.Index where

import HaskellWorks.Data.Json.DecodeError
import Prelude                            hiding (drop)

import qualified Data.ByteString as BS

data JsonIndex
  = JsonIndexString BS.ByteString
  | JsonIndexNumber BS.ByteString
  | JsonIndexObject [(BS.ByteString, JsonIndex)]
  | JsonIndexArray [JsonIndex]
  | JsonIndexBool Bool
  | JsonIndexNull
  deriving (Eq, Show)

class JsonIndexAt a where
  jsonIndexAt :: a -> Either DecodeError JsonIndex
