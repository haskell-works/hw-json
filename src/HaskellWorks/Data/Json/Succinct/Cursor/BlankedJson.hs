
module HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
  ( BlankedJson(..)
  , FromBlankedJson(..)
  , getBlankedJson
  ) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.ByteString
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString

newtype BlankedJson = BlankedJson [BS.ByteString] deriving (Eq, Show)

getBlankedJson :: BlankedJson -> [BS.ByteString]
getBlankedJson (BlankedJson bs) = bs

class FromBlankedJson a where
  fromBlankedJson :: BlankedJson -> a

instance FromByteString BlankedJson where
  fromByteString bs = BlankedJson (runListConduit blankJson (chunkedBy 4064 bs))
