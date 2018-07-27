
module HaskellWorks.Data.Json.Internal.Cursor.BlankedJson
  ( BlankedJson(..)
  , FromBlankedJson(..)
  , getBlankedJson
  ) where

import HaskellWorks.Data.ByteString
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Internal.Blank

import qualified Data.ByteString as BS

newtype BlankedJson = BlankedJson [BS.ByteString] deriving (Eq, Show)

getBlankedJson :: BlankedJson -> [BS.ByteString]
getBlankedJson (BlankedJson bs) = bs

class FromBlankedJson a where
  fromBlankedJson :: BlankedJson -> a

instance FromByteString BlankedJson where
  fromByteString bs = BlankedJson (blankJson (chunkedBy 4096 bs))
