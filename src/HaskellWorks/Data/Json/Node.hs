{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Json.Node where

import           Control.Arrow
import qualified Data.ByteString                                            as BS
import qualified Data.List                                                  as L
import qualified Data.Map                                                   as M
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
import           HaskellWorks.Data.TreeCursor
import           HaskellWorks.Data.Vector.VectorLike

data JsonByteStringNode
  = JsonByteStringString BS.ByteString
  | JsonByteStringNumber BS.ByteString
  | JsonByteStringObject (M.Map BS.ByteString JsonByteStringNode)
  | JsonByteStringArray [JsonByteStringNode]
  | JsonByteStringBool Bool
  | JsonByteStringNull
  deriving (Eq, Show)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => Decode (JsonCursor BS.ByteString v w) JsonByteStringNode where
  decode :: JsonCursor BS.ByteString v w -> Either DecodeError JsonByteStringNode
  decode k = case BS.uncons remainder of
    Just (!c, _) | isLeadingDigit c   -> Right (JsonByteStringNumber  undefined)
    Just (!c, _) | c == _quotedbl     -> Right (JsonByteStringString  undefined)
    Just (!c, _) | c == _t            -> Right (JsonByteStringBool    True)
    Just (!c, _) | c == _f            -> Right (JsonByteStringBool    False)
    Just (!c, _) | c == _n            -> Right  JsonByteStringNull
    Just (!c, _) | c == _braceleft    -> JsonByteStringObject <$> mapValuesFrom   (firstChild k)
    Just (!c, _) | c == _bracketleft  -> JsonByteStringArray  <$> arrayValuesFrom (firstChild k)
    Just _                            -> Left (DecodeError "Invalid Json Type")
    Nothing                           -> Left (DecodeError "End of data"      )
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = vDrop (toCount p) (cursorText k)
          arrayValuesFrom j = sequence (L.unfoldr (fmap (decode &&& nextSibling)) j)
          mapValuesFrom j   = (\v -> M.fromList (pairwise v >>= asField)) <$> arrayValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case a of
                                JsonByteStringString s  -> [(s, b)]
                                _                 -> []
