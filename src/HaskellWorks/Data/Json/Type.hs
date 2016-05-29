{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Json.Type where

import qualified Data.ByteString                                            as BS
import           Data.Char
import           Data.Word8
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Json.Conduit.Words
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Positioning
import qualified HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data ExtractJsonState
  = ExtractJsonStringEscaped
  | ExtractJsonStringInJson
  | ExtractJsonStringInNumber
  | ExtractJsonStringInString

wIsJsonNumberDigit :: Word8 -> Bool
wIsJsonNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

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

data JsonType
  = JsonTypeArray
  | JsonTypeBool
  | JsonTypeNull
  | JsonTypeNumber
  | JsonTypeObject
  | JsonTypeString
  deriving (Eq, Show)

class JsonTypeAt a where
  jsonTypeAtPosition :: Position -> a -> Maybe JsonType
  jsonTypeAt :: a -> Maybe JsonType

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonTypeAt (JsonCursor String v w) where
  jsonTypeAtPosition p k = case vDrop (toCount p) (cursorText k) of
    c:_ | fromIntegral (ord c) == _bracketleft      -> Just JsonTypeArray
    c:_ | fromIntegral (ord c) == _f                -> Just JsonTypeBool
    c:_ | fromIntegral (ord c) == _t                -> Just JsonTypeBool
    c:_ | fromIntegral (ord c) == _n                -> Just JsonTypeNull
    c:_ | wIsJsonNumberDigit (fromIntegral (ord c)) -> Just JsonTypeNumber
    c:_ | fromIntegral (ord c) == _braceleft        -> Just JsonTypeObject
    c:_ | fromIntegral (ord c) == _quotedbl         -> Just JsonTypeString
    _                                               -> Nothing

  jsonTypeAt k = jsonTypeAtPosition p k
    where p   = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonTypeAt (JsonCursor BS.ByteString v w) where
  jsonTypeAtPosition p k = case BS.uncons (vDrop (toCount p) (cursorText k)) of
    Just (c, _) | c == _bracketleft     -> Just JsonTypeArray
    Just (c, _) | c == _f               -> Just JsonTypeBool
    Just (c, _) | c == _t               -> Just JsonTypeBool
    Just (c, _) | c == _n               -> Just JsonTypeNull
    Just (c, _) | wIsJsonNumberDigit c  -> Just JsonTypeNumber
    Just (c, _) | c == _braceleft       -> Just JsonTypeObject
    Just (c, _) | c == _quotedbl        -> Just JsonTypeString
    _                                   -> Nothing

  jsonTypeAt k = jsonTypeAtPosition p k
    where p   = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => Decode (JsonCursor BS.ByteString v w) (BS.ByteString, JsonType) where
  decode :: JsonCursor BS.ByteString v w -> Either DecodeError (BS.ByteString, JsonType)
  decode k = case BS.uncons remainder of
    Just (!c, _) | isLeadingDigit c   -> Right (remainder, JsonTypeNumber )
    Just (!c, _) | c == _quotedbl     -> Right (remainder, JsonTypeString )
    Just (!c, _) | c == _t            -> Right (remainder, JsonTypeBool   )
    Just (!c, _) | c == _f            -> Right (remainder, JsonTypeBool   )
    Just (!c, _) | c == _n            -> Right (remainder, JsonTypeNull   )
    Just (!c, _) | c == _braceleft    -> Right (remainder, JsonTypeObject )
    Just (!c, _) | c == _bracketleft  -> Right (remainder, JsonTypeArray  )
    Just _                            -> Left (DecodeError "Invalid Json Type")
    Nothing                           -> Left (DecodeError "End of data"      )
    where ik        = interests k
          bpk       = balancedParens k
          p         = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder = (vDrop (toCount p) (cursorText k))
