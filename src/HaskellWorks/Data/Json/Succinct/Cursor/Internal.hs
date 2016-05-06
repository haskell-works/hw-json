{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.Internal
  ( JsonCursor(..)
  ) where

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import           Data.ByteString.Internal                              as BSI
import           Data.String
import qualified Data.Vector.Storable                                  as DVS
import           Data.Word
import           Data.Word8
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor.BalancedParens
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
import           HaskellWorks.Data.Json.Type
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens             as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
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

instance  (FromBlankedJson (JsonInterestBits a), FromBlankedJson (JsonBalancedParens b))
          => FromByteString (JsonCursor BS.ByteString a b) where
  fromByteString bs   = JsonCursor
    { cursorText      = bs
    , interests       = getJsonInterestBits (fromBlankedJson blankedJson)
    , balancedParens  = getJsonBalancedParens (fromBlankedJson blankedJson)
    , cursorRank      = 1
    }
    where blankedJson :: BlankedJson
          blankedJson = fromByteString bs

instance IsString (JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])) where
  fromString :: String -> JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank      = 1
    , interests       = getJsonInterestBits (fromBlankedJson blankedJson)
    , balancedParens  = getJsonBalancedParens (fromBlankedJson blankedJson)
    }
    where blankedJson :: BlankedJson
          blankedJson = fromByteString (BSC.pack s)

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (JsonCursor t v u) where
  firstChild :: JsonCursor t v u -> JsonCursor t v u
  firstChild k = k { cursorRank = BP.firstChild (balancedParens k) (cursorRank k) }

  nextSibling :: JsonCursor t v u -> JsonCursor t v u
  nextSibling k = k { cursorRank = BP.nextSibling (balancedParens k) (cursorRank k) }

  parent :: JsonCursor t v u -> JsonCursor t v u
  parent k = k { cursorRank = BP.parent (balancedParens k) (cursorRank k) }

  depth :: JsonCursor t v u -> Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: JsonCursor t v u -> Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

wIsJsonNumberDigit :: Word8 -> Bool
wIsJsonNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

instance TestBit w => JsonTypeAt (JsonCursor BS.ByteString v w) where
  jsonTypeAtPosition p k = if balancedParens k .?. p
    then case cursorText k !!! p of
      c | c == _bracketleft     -> Just JsonTypeArray
      c | c == _t               -> Just JsonTypeBool
      c | c == _n               -> Just JsonTypeNull
      c | wIsJsonNumberDigit c  -> Just JsonTypeNumber
      c | c == _braceleft       -> Just JsonTypeObject
      c | c == _quotedbl        -> Just JsonTypeString
      _                         -> Nothing
    else Nothing

  jsonTypeAt k = jsonTypeAtPosition (lastPositionOf (cursorRank k)) k

instance TestBit w => JsonValueAt BS.ByteString BS.ByteString (JsonCursor BS.ByteString v w) where
  jsonValueAt :: JsonCursor BS.ByteString v w -> Maybe (JsonValue BS.ByteString BS.ByteString)
  jsonValueAt k = case jsonTypeAtPosition p k of
    Just JsonTypeArray  -> error "Not Implemented"
    Just JsonTypeBool   -> case cursorText k !!! p of
      c | c == _t -> Just $ JsonBool True
      c | c == _t -> Just $ JsonBool False
      _           -> Nothing
    Just JsonTypeNull   -> Just JsonNull
    Just JsonTypeNumber -> error "Not Implemented"
    Just JsonTypeObject -> error "Not Implemented"
    Just JsonTypeString -> error "Not Implemented"
    Nothing             -> Nothing
    where p = lastPositionOf (cursorRank k)
