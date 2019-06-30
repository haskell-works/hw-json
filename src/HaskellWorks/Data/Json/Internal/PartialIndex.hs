{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Json.Internal.PartialIndex where

import Control.Arrow
import Control.Monad
import Data.String
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Json.Internal.CharLike
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Uncons
import Prelude                                        hiding (drop)

import qualified Data.ByteString                  as BS
import qualified Data.List                        as L
import qualified HaskellWorks.Data.BalancedParens as BP

data JsonPartialIndex
  = JsonPartialIndexString BS.ByteString
  | JsonPartialIndexNumber BS.ByteString
  | JsonPartialIndexObject [(BS.ByteString, JsonPartialIndex)]
  | JsonPartialIndexArray [JsonPartialIndex]
  | JsonPartialIndexBool Bool
  | JsonPartialIndexNull
  | JsonPartialIndexError String
  deriving (Eq, Show)

class JsonPartialIndexAt a where
  jsonPartialIndexAt :: a -> JsonPartialIndex

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonPartialIndexAt (GenericCursor BS.ByteString v w) where
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
          arrayValuesFrom :: Maybe (GenericCursor BS.ByteString v w) -> [JsonPartialIndex]
          arrayValuesFrom = L.unfoldr (fmap (jsonPartialIndexAt &&& nextSibling))
          mapValuesFrom j   = pairwise (arrayValuesFrom j) >>= asField
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case a of
                                JsonPartialIndexString s -> [(s, b)]
                                _                        -> []

