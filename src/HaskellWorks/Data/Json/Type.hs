{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Json.Type
  ( JsonType(..)
  , JsonTypeAt(..)
  ) where

import qualified Data.ByteString                                            as BS
import           Data.Char
import           Data.Word8
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Positioning
import qualified HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

wIsJsonNumberDigit :: Word8 -> Bool
wIsJsonNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

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
