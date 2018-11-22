module HaskellWorks.Data.Json.Internal.PartialIndex where

import qualified Data.ByteString as BS

data JsonPartialIndex
  = JsonPartialIndexString BS.ByteString
  | JsonPartialIndexNumber BS.ByteString
  | JsonPartialIndexObject [(BS.ByteString, JsonPartialIndex)]
  | JsonPartialIndexArray [JsonPartialIndex]
  | JsonPartialIndexBool Bool
  | JsonPartialIndexNull
  | JsonPartialIndexError String
  deriving (Eq, Show)

class JsonPartialIndexAt a where
  jsonPartialIndexAt :: a -> JsonPartialIndex
