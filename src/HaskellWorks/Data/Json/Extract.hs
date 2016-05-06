{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Extract
  ( extractJsonSnippet
  ) where

import qualified Data.ByteString as BS
import           Data.Word8
import           HaskellWorks.Data.Conduit.Json.Words
import           HaskellWorks.Data.Json.Type

data ExtractJsonState
  = ExtractJsonStringEscaped
  | ExtractJsonStringInJson
  | ExtractJsonStringInNumber
  | ExtractJsonStringInString

extractJsonSnippet :: BS.ByteString -> Maybe (JsonType, BS.ByteString)
extractJsonSnippet bs = case extractJsonSnippet' 0 ExtractJsonStringInJson bs of
  Just (jsonType, len)  -> Just (jsonType, BS.take len bs)
  Nothing               -> Nothing

extractJsonSnippet' :: Int -> ExtractJsonState -> BS.ByteString -> Maybe (JsonType, Int)
extractJsonSnippet' n ExtractJsonStringInJson bs = case BS.uncons bs of
  Just (!c, !cs) | isLeadingDigit c   -> extractJsonSnippet' (n + 1) ExtractJsonStringInNumber cs
  Just (!c, !cs) | c == _quotedbl     -> extractJsonSnippet' (n + 1) ExtractJsonStringInString cs
  Just (!c,   _) | c == _t            -> Just (JsonTypeBool, n + 1)
  Just (!c,   _) | c == _f            -> Just (JsonTypeBool, n + 1)
  Just (!c,   _) | c == _n            -> Just (JsonTypeNull, n + 1)
  Just (!c,   _) | c == _braceleft    -> Just (JsonTypeObject, n + 1)
  Just (!c,   _) | c == _bracketleft  -> Just (JsonTypeArray, n + 1)
  Just _                              -> Nothing
  Nothing                             -> Nothing
extractJsonSnippet' n ExtractJsonStringInString bs = case BS.uncons bs of
  Just (!c, !cs) | c == _backslash    -> extractJsonSnippet' (n + 1) ExtractJsonStringEscaped  cs
  Just (!c,   _) | c == _quotedbl     -> Just (JsonTypeString, n + 1)
  Just (_ , !cs)                      -> extractJsonSnippet' (n + 1) ExtractJsonStringInString cs
  Nothing                             -> Nothing
extractJsonSnippet' n ExtractJsonStringEscaped bs = case BS.uncons bs of
  Just (_, !cs)                       -> extractJsonSnippet' (n + 1) ExtractJsonStringInString cs
  Nothing                             -> Nothing
extractJsonSnippet' n ExtractJsonStringInNumber bs = case BS.uncons bs of
  Just (!c, !cs) | isTrailingDigit c  -> extractJsonSnippet' (n + 1) ExtractJsonStringInNumber cs
  Just _                              -> Just (JsonTypeNumber, n)
  Nothing                             -> Just (JsonTypeNumber, n)
