
module HaskellWorks.Data.Json.Standard.Cursor.Internal.BlankedJson
  ( BlankedJson(..)
  , ToBlankedJson(..)
  , toBlankedJsonTyped
  ) where

import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Json.Standard.Cursor.Internal.Blank

import qualified Data.ByteString as BS

newtype BlankedJson = BlankedJson
  { unBlankedJson :: [BS.ByteString]
  } deriving (Eq, Show)

class ToBlankedJson a where
  toBlankedJson :: a -> [BS.ByteString]

instance ToBlankedJson BS.ByteString where
  toBlankedJson bs = blankJson (chunkedBy 4096 bs)

toBlankedJsonTyped :: ToBlankedJson a => a -> BlankedJson
toBlankedJsonTyped = BlankedJson . toBlankedJson
