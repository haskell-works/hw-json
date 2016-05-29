{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Value.Internal where

import qualified Data.ByteString                                            as BS
import           Data.ByteString.Internal                                   as BSI
import qualified Data.List                                                  as L
import qualified Data.Map                                                   as M
import           Data.Word8
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal
import           HaskellWorks.Data.Json.Type
import qualified HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.TreeCursor
import           HaskellWorks.Data.Vector.VectorLike

data GenJsonValue s n
  = JsonString s
  | JsonNumber n
  | JsonObject (M.Map s (GenJsonValue s n))
  | JsonArray [GenJsonValue s n]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class GenJsonValueAt s n a where
  jsonValueAt :: a -> Maybe (GenJsonValue s n)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => GenJsonValueAt BS.ByteString BS.ByteString (JsonCursor BS.ByteString v w) where
  jsonValueAt :: JsonCursor BS.ByteString v w -> Maybe (GenJsonValue BS.ByteString BS.ByteString)
  jsonValueAt k = case extractJsonSnippet remainder of
    Just (JsonTypeArray ,  _) -> Just $ JsonArray (arrayValuesAt k)
    Just (JsonTypeBool  , bs) -> case BS.uncons bs of
      Just (c, _) | c == _t   -> Just $ JsonBool True
      Just (c, _) | c == _f   -> Just $ JsonBool False
      _                       -> Nothing
    Just (JsonTypeNull  ,  _) -> Just JsonNull
    Just (JsonTypeNumber, bs) -> Just $ JsonNumber bs
    Just (JsonTypeObject,  _) -> Just $ JsonObject (mapValuesAt k)
    Just (JsonTypeString, bs) -> Just $ JsonString bs
    Nothing                   -> Nothing
    where p = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          ik  = interests k
          bpk = balancedParens k
          remainder = vDrop (toCount p) (cursorText k)
          genArrayValue :: JsonCursor BS.ByteString v w -> Maybe (GenJsonValue ByteString ByteString, JsonCursor ByteString v w)
          genArrayValue j = (,) <$> jsonValueAt j <*> nextSibling j
          arrayValuesAt :: JsonCursor BS.ByteString v w -> [GenJsonValue BS.ByteString BS.ByteString]
          arrayValuesAt j = case firstChild j of
            Just c  -> L.unfoldr genArrayValue c
            Nothing -> []
          mapValuesAt :: JsonCursor BS.ByteString v w -> M.Map ByteString (GenJsonValue ByteString ByteString)
          mapValuesAt j = M.fromList (pairwise (arrayValuesAt j) >>= asField)
          asField :: (GenJsonValue ByteString ByteString, GenJsonValue ByteString ByteString) -> [(ByteString, GenJsonValue ByteString ByteString)]
          asField (a, b) = case a of
            JsonString s  -> [(s, b)]
            _             -> []
          pairwise :: [a] -> [(a, a)]
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
