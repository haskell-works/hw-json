{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Internal.Backend.Standard.Blank
  ( blankJson
  ) where

import Data.ByteString                       as BS
import Data.Word
import Data.Word8
import HaskellWorks.Data.Json.Internal.Word8
import Prelude                               as P

data BlankState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

blankJson :: [BS.ByteString] -> [BS.ByteString]
blankJson = blankJson' InJson

blankJson' :: BlankState -> [BS.ByteString] -> [BS.ByteString]
blankJson' lastState as = case as of
  (bs:bss) ->
      let (!cs, Just (!nextState, _)) = unfoldrN (BS.length bs) blankByteString (lastState, bs) in
      cs:blankJson' nextState bss
  [] -> []
  where
    blankByteString :: (BlankState, ByteString) -> Maybe (Word8, (BlankState, ByteString))
    blankByteString (InJson, bs) = case BS.uncons bs of
      Just (!c, !cs) | isLeadingDigit c -> Just (_1          , (InNumber , cs))
      Just (!c, !cs) | c == _quotedbl   -> Just (_parenleft  , (InString , cs))
      Just (!c, !cs) | isAlphabetic c   -> Just (c           , (InIdent  , cs))
      Just (!c, !cs)                    -> Just (c           , (InJson   , cs))
      Nothing                           -> Nothing
    blankByteString (InString, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == _backslash -> Just (_space      , (Escaped  , cs))
      Just (!c, !cs) | c == _quotedbl  -> Just (_parenright , (InJson   , cs))
      Just (_ , !cs)                   -> Just (_space      , (InString , cs))
      Nothing                          -> Nothing
    blankByteString (Escaped, bs) = case BS.uncons bs of
      Just (_, !cs) -> Just (_space, (InString, cs))
      Nothing       -> Nothing
    blankByteString (InNumber, bs) = case BS.uncons bs of
      Just (!c, !cs) | isTrailingDigit c -> Just (_0          , (InNumber , cs))
      Just (!c, !cs) | c == _quotedbl    -> Just (_parenleft  , (InString , cs))
      Just (!c, !cs) | isAlphabetic c    -> Just (c           , (InIdent  , cs))
      Just (!c, !cs)                     -> Just (c           , (InJson   , cs))
      Nothing                            -> Nothing
    blankByteString (InIdent, bs) = case BS.uncons bs of
      Just (!c, !cs) | isAlphabetic c   -> Just (_underscore , (InIdent  , cs))
      Just (!c, !cs) | isLeadingDigit c -> Just (_1          , (InNumber , cs))
      Just (!c, !cs) | c == _quotedbl   -> Just (_parenleft  , (InString , cs))
      Just (!c, !cs)                    -> Just (c           , (InJson   , cs))
      Nothing                           -> Nothing
