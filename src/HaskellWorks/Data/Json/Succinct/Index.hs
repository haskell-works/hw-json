{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Json.Succinct.Index where

import           Control.Arrow
import           Control.Monad
import qualified Data.List                                                  as L
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Json.CharLike
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Positioning
import qualified HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.TreeCursor
import           HaskellWorks.Data.Vector.VectorLike

data JsonIndex s
  = JsonByteStringString s
  | JsonByteStringNumber s
  | JsonByteStringObject [(s, JsonIndex s)]
  | JsonByteStringArray [JsonIndex s]
  | JsonByteStringBool Bool
  | JsonByteStringNull
  deriving (Eq, Show)

class JsonIndexAt a where
  jsonIndexAt :: a -> Either DecodeError (JsonIndex s)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w, VectorLike s, JsonCharLike (Elem s)) => JsonIndexAt (JsonCursor s v w) where
  jsonIndexAt k = case vUncons remainder of
    Just (!c, _) | isLeadingDigit2 c  -> Right (JsonByteStringNumber  undefined)
    Just (!c, _) | isQuotDbl c        -> Right (JsonByteStringString  undefined)
    Just (!c, _) | isChar_t c         -> Right (JsonByteStringBool    True)
    Just (!c, _) | isChar_f c         -> Right (JsonByteStringBool    False)
    Just (!c, _) | isChar_n c         -> Right  JsonByteStringNull
    Just (!c, _) | isBraceLeft c      -> JsonByteStringObject <$> mapValuesFrom   (firstChild k)
    Just (!c, _) | isBracketLeft c    -> JsonByteStringArray  <$> arrayValuesFrom (firstChild k)
    Just _                            -> Left (DecodeError "Invalid Json Type")
    Nothing                           -> Left (DecodeError "End of data"      )
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = vDrop (toCount p) (cursorText k)
          arrayValuesFrom j = sequence (L.unfoldr (fmap (jsonIndexAt &&& nextSibling)) j)
          mapValuesFrom j   = (pairwise >=> asField) <$> arrayValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case a of
                                JsonByteStringString s  -> [(s, b)]
                                _                       -> []
