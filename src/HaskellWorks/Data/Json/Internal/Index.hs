{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Json.Internal.Index where

import Control.Arrow
import Control.Monad
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Json.DecodeError
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

data JsonIndex
  = JsonIndexString BS.ByteString
  | JsonIndexNumber BS.ByteString
  | JsonIndexObject [(BS.ByteString, JsonIndex)]
  | JsonIndexArray [JsonIndex]
  | JsonIndexBool Bool
  | JsonIndexNull
  deriving (Eq, Show)

class JsonIndexAt a where
  jsonIndexAt :: a -> Either DecodeError JsonIndex

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonIndexAt (GenericCursor BS.ByteString v w) where
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
