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
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import qualified HaskellWorks.Data.Json.Succinct.Cursor.BalancedParens      as CBP
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
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
  nextSibling k = (\q -> k { cursorRank = q }) <$> BP.nextSibling (balancedParens k) (cursorRank k)

  parent :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: JsonCursor t v u -> Maybe Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: JsonCursor t v u -> Maybe Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

jsonCursorPos :: (Rank1 w, Select1 v, VectorLike s) => JsonCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k
