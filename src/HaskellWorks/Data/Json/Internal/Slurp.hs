{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Internal.Slurp where

import Data.String
import Data.Text
import Data.Word
import Data.Word8
import HaskellWorks.Data.Json.Internal.Word8
import Prelude                               hiding (drop)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List             as L
import qualified Data.Text             as T

data JsonState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

slurpString :: BS.ByteString -> Text
slurpString bs = T.pack $ L.unfoldr genString (InJson, BSC.unpack bs) -- TODO optimise
  where genString :: (JsonState, String) -> Maybe (Char, (JsonState, String))
        genString (InJson, ds) = case ds of
          (e:es) | e == '"' -> genString  (InString , es)
          (_:es)            -> genString  (InJson   , es)
          _                 -> Nothing
        genString (InString, ds) = case ds of
          (e:es) | e == '\\' -> genString  (Escaped  , es)
          (e:_ ) | e == '"'  -> Nothing
          (e:es)             -> Just (e,   (InString , es))
          _                  -> Nothing
        genString (Escaped, ds) = case ds of
          (_:es) -> Just ('.', (InString , es))
          _      -> Nothing
        genString (_, _) = Nothing

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
