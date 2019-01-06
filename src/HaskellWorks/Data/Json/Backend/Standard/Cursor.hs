{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Backend.Standard.Cursor
  ( JsonCursor(..)
  , jsonCursorPos
  ) where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.String
import Data.Word8
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Json.DecodeError
import HaskellWorks.Data.Json.Internal.CharLike
import HaskellWorks.Data.Json.Internal.Index
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.Internal.Slurp
import HaskellWorks.Data.Json.Internal.Word8
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Json.Type
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Uncons
import Prelude                                      hiding (drop)

import qualified Data.ByteString                  as BS
import qualified Data.List                        as L
import qualified HaskellWorks.Data.BalancedParens as BP

data JsonCursor t v w = JsonCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Show)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (JsonCursor t v u) where
  firstChild :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  firstChild k = let mq = BP.firstChild (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  nextSibling :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  nextSibling k = (\q -> k { cursorRank = q }) <$> BP.nextSibling (balancedParens k) (cursorRank k)

  parent :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: JsonCursor t v u -> Maybe Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: JsonCursor t v u -> Maybe Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

jsonCursorPos :: (Rank1 w, Select1 v) => JsonCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => LightJsonAt (JsonCursor BS.ByteString v w) where
  lightJsonAt k = case uncons remainder of
    Just (!c, _) | isLeadingDigit2 c -> LightJsonNumber  (slurpNumber remainder)
    Just (!c, _) | isQuotDbl c       -> LightJsonString  (slurpString remainder)
    Just (!c, _) | isChar_t c        -> LightJsonBool    True
    Just (!c, _) | isChar_f c        -> LightJsonBool    False
    Just (!c, _) | isChar_n c        -> LightJsonNull
    Just (!c, _) | isBraceLeft c     -> LightJsonObject (mapValuesFrom   (firstChild k))
    Just (!c, _) | isBracketLeft c   -> LightJsonArray  (arrayValuesFrom (firstChild k))
    Just _                           -> LightJsonError "Invalid Json Type"
    Nothing                          -> LightJsonError "End of data"
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = drop (toCount p) (cursorText k)
          arrayValuesFrom   = L.unfoldr (fmap (id &&& nextSibling))
          mapValuesFrom j   = pairwise (arrayValuesFrom j) >>= asField
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case lightJsonAt a of
                                LightJsonString s -> [(s, b)]
                                _                 -> []

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonTypeAt (JsonCursor String v w) where
  jsonTypeAtPosition p k = case drop (toCount p) (cursorText k) of
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
  jsonTypeAtPosition p k = case BS.uncons (drop (toCount p) (cursorText k)) of
    Just (c, _) | c == _bracketleft    -> Just JsonTypeArray
    Just (c, _) | c == _f              -> Just JsonTypeBool
    Just (c, _) | c == _t              -> Just JsonTypeBool
    Just (c, _) | c == _n              -> Just JsonTypeNull
    Just (c, _) | wIsJsonNumberDigit c -> Just JsonTypeNumber
    Just (c, _) | c == _braceleft      -> Just JsonTypeObject
    Just (c, _) | c == _quotedbl       -> Just JsonTypeString
    _                                  -> Nothing

  jsonTypeAt k = jsonTypeAtPosition p k
    where p   = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonPartialIndexAt (JsonCursor BS.ByteString v w) where
  jsonPartialIndexAt k = case uncons remainder of
    Just (!c, _) | isLeadingDigit2 c -> JsonPartialIndexNumber  remainder
    Just (!c, _) | isQuotDbl c       -> JsonPartialIndexString  remainder
    Just (!c, _) | isChar_t c        -> JsonPartialIndexBool    True
    Just (!c, _) | isChar_f c        -> JsonPartialIndexBool    False
    Just (!c, _) | isChar_n c        -> JsonPartialIndexNull
    Just (!c, _) | isBraceLeft c     -> JsonPartialIndexObject (mapValuesFrom   (firstChild k))
    Just (!c, _) | isBracketLeft c   -> JsonPartialIndexArray  (arrayValuesFrom (firstChild k))
    Just _                           -> JsonPartialIndexError "Invalid Json Type"
    Nothing                          -> JsonPartialIndexError "End of data"
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = drop (toCount p) (cursorText k)
          arrayValuesFrom :: Maybe (JsonCursor BS.ByteString v w) -> [JsonPartialIndex]
          arrayValuesFrom = L.unfoldr (fmap (jsonPartialIndexAt &&& nextSibling))
          mapValuesFrom j   = pairwise (arrayValuesFrom j) >>= asField
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case a of
                                JsonPartialIndexString s -> [(s, b)]
                                _                        -> []

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonIndexAt (JsonCursor BS.ByteString v w) where
  jsonIndexAt k = case uncons remainder of
    Just (!c, _) | isLeadingDigit2 c -> Right (JsonIndexNumber  remainder)
    Just (!c, _) | isQuotDbl c       -> Right (JsonIndexString  remainder)
    Just (!c, _) | isChar_t c        -> Right (JsonIndexBool    True)
    Just (!c, _) | isChar_f c        -> Right (JsonIndexBool    False)
    Just (!c, _) | isChar_n c        -> Right  JsonIndexNull
    Just (!c, _) | isBraceLeft c     -> JsonIndexObject <$> mapValuesFrom   (firstChild k)
    Just (!c, _) | isBracketLeft c   -> JsonIndexArray  <$> arrayValuesFrom (firstChild k)
    Just _                           -> Left (DecodeError "Invalid Json Type")
    Nothing                          -> Left (DecodeError "End of data"      )
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = drop (toCount p) (cursorText k)
          arrayValuesFrom j = sequence (L.unfoldr (fmap (jsonIndexAt &&& nextSibling)) j)
          mapValuesFrom j   = (pairwise >=> asField) <$> arrayValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case a of
                                JsonIndexString s -> [(s, b)]
                                _                 -> []

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonPartialValueAt (JsonCursor BS.ByteString v w) where
  jsonPartialJsonValueAt = jsonPartialJsonValueAt . jsonPartialIndexAt


