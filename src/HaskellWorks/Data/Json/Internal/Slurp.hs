{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Internal.Slurp
  ( JsonState(..)
  , slurpText
  , slurpNumber
  ) where

import Data.Text
import Data.Word
import Data.Word8
import HaskellWorks.Data.Json.Standard.Cursor.Internal.Word8
import Prelude                                               hiding (drop)

import qualified Data.Aeson.Parser.Internal  as AP
import qualified Data.Attoparsec.ByteString  as PBS
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T

data JsonState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

-- | Slurp a JSON string
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> slurpText "\"Hello\""
-- Right "Hello"
-- >>> slurpText "123"
-- Left "34: Failed reading: satisfy"
slurpText :: BS.ByteString -> Either Text Text
slurpText bs = case PBS.parseOnly AP.jstring bs of
  Right t -> Right t
  Left e  -> Left (T.pack e)

-- | Slurp a JSON number
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> slurpNumber "123, true"
-- "123"
-- >>> slurpNumber "\"Hello\""
-- "\"Hello\""
slurpNumber :: BS.ByteString -> BS.ByteString
slurpNumber bs = let (!cs, _) = BS.unfoldrN (BS.length bs) genNumber (InJson, bs) in cs
    where genNumber :: (JsonState, BS.ByteString) -> Maybe (Word8, (JsonState, BS.ByteString))
          genNumber (InJson, cs) = case BS.uncons cs of
            Just (!d, !ds) | isLeadingDigit d -> Just (d           , (InNumber , ds))
            Just (!d, !ds)                    -> Just (d           , (InJson   , ds))
            Nothing                           -> Nothing
          genNumber (InNumber, cs) = case BS.uncons cs of
            Just (!d, !ds) | isTrailingDigit d -> Just (d           , (InNumber , ds))
            Just (!d, !ds) | d == _quotedbl    -> Just (_parenleft  , (InString , ds))
            _                                  -> Nothing
          genNumber (_, _) = Nothing
