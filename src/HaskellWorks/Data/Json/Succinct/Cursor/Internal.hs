{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Succinct.Cursor.Internal
  ( JsonCursor(..)
  , jsonCursorPos
  ) where

import qualified Data.ByteString                                            as BS
import qualified Data.ByteString.Char8                                      as BSC
import           Data.ByteString.Internal                                   as BSI
import           Data.Char
import qualified Data.List                                                  as L
import qualified Data.Map                                                   as M
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           Data.Word8
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Conduit.Words
import           HaskellWorks.Data.Json.Extract
import qualified HaskellWorks.Data.Json.Succinct.Cursor.BalancedParens      as CBP
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
import           HaskellWorks.Data.Json.Type
import qualified HaskellWorks.Data.Json.Value.ByteString                    as VBS
import           HaskellWorks.Data.Json.Value.Internal
import           HaskellWorks.Data.Positioning
import qualified HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import           HaskellWorks.Data.TreeCursor
import           HaskellWorks.Data.Vector.VectorLike

data JsonCursor t v w = JsonCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Show)

instance  (FromBlankedJson (JsonInterestBits a), FromBlankedJson (CBP.JsonBalancedParens b))
          => FromByteString (JsonCursor BS.ByteString a b) where
  fromByteString bs   = JsonCursor
    { cursorText      = bs
    , interests       = getJsonInterestBits (fromBlankedJson blankedJson)
    , balancedParens  = CBP.getJsonBalancedParens (fromBlankedJson blankedJson)
    , cursorRank      = 1
    }
    where blankedJson :: BlankedJson
          blankedJson = fromByteString bs

instance IsString (JsonCursor String (BitShown [Bool]) (BP.SimpleBalancedParens [Bool])) where
  fromString :: String -> JsonCursor String (BitShown [Bool]) (BP.SimpleBalancedParens [Bool])
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank      = 1
    , interests       = getJsonInterestBits (fromBlankedJson blankedJson)
    , balancedParens  = CBP.getJsonBalancedParens (fromBlankedJson blankedJson)
    }
    where blankedJson :: BlankedJson
          blankedJson = fromByteString (BSC.pack s)

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (BP.SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (BP.SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (BP.SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (BP.SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (BP.SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (BP.SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (JsonCursor t v u) where
  firstChild :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  firstChild k = let mq = BP.firstChild (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  nextSibling :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  nextSibling k = let mq = BP.nextSibling (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  parent :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: JsonCursor t v u -> Maybe Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: JsonCursor t v u -> Maybe Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

wIsJsonNumberDigit :: Word8 -> Bool
wIsJsonNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

jsonCursorPos :: (Rank1 w, Select1 v, VectorLike s) => JsonCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k

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

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => Decode (JsonCursor BS.ByteString v w) VBS.JsonValue where
  decode :: JsonCursor BS.ByteString v w -> Either DecodeError VBS.JsonValue
  decode k = case BS.uncons remainder of
    Just (!c, _) | isLeadingDigit c   -> Right (VBS.JsonNumber  undefined)
    Just (!c, _) | c == _quotedbl     -> Right (VBS.JsonString  undefined)
    Just (!c, _) | c == _t            -> Right (VBS.JsonBool    True)
    Just (!c, _) | c == _f            -> Right (VBS.JsonBool    False)
    Just (!c, _) | c == _n            -> Right (VBS.JsonNull)
    Just (!c, _) | c == _braceleft    -> VBS.JsonObject <$> mapValuesFrom   (firstChild k)
    Just (!c, _) | c == _bracketleft  -> VBS.JsonArray  <$> arrayValuesFrom (firstChild k)
    Just _                            -> Left (DecodeError "Invalid Json Type")
    Nothing                           -> Left (DecodeError "End of data"      )
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = (vDrop (toCount p) (cursorText k))
          arrayValuesFrom j = sequence (L.unfoldr (fmap (\s -> (decode s, nextSibling s))) j)
          mapValuesFrom j   = (\v -> M.fromList (pairwise v >>= asField)) <$> arrayValuesFrom j
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case a of
                                VBS.JsonString s  -> [(s, b)]
                                _                 -> []

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
          remainder = (vDrop (toCount p) (cursorText k))
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
