{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
  ( JsonInterestBits(..)
  , getJsonInterestBits
  ) where

import           Control.Applicative
import qualified Data.ByteString                                       as BS
import           Data.ByteString.Internal
import qualified Data.Vector.Storable                                  as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512

newtype JsonInterestBits a = JsonInterestBits a

getJsonInterestBits :: JsonInterestBits a -> a
getJsonInterestBits (JsonInterestBits a) = a

blankedJsonBssToInterestBitsBs :: [ByteString] -> ByteString
blankedJsonBssToInterestBitsBs bss = BS.concat $ runListConduit blankedJsonToInterestBits bss

genInterest :: ByteString -> Maybe (Word8, ByteString)
genInterest = BS.uncons

genInterestForever :: ByteString -> Maybe (Word8, ByteString)
genInterestForever bs = BS.uncons bs <|> Just (0, bs)

instance FromBlankedJson (JsonInterestBits (BitShown [Bool])) where
  fromBlankedJson = JsonInterestBits . fromByteString . BS.concat . runListConduit blankedJsonToInterestBits . getBlankedJson

instance FromBlankedJson (JsonInterestBits (BitShown BS.ByteString)) where
  fromBlankedJson = JsonInterestBits . BitShown . BS.unfoldr genInterest . blankedJsonBssToInterestBitsBs . getBlankedJson

instance FromBlankedJson (JsonInterestBits (BitShown (DVS.Vector Word8))) where
  fromBlankedJson = JsonInterestBits . BitShown . DVS.unfoldr genInterest . blankedJsonBssToInterestBitsBs . getBlankedJson

instance FromBlankedJson (JsonInterestBits (BitShown (DVS.Vector Word16))) where
  fromBlankedJson bj = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS    = blankedJsonBssToInterestBitsBs (getBlankedJson bj)
          newLen        = (BS.length interestBS + 1) `div` 2 * 2

instance FromBlankedJson (JsonInterestBits (BitShown (DVS.Vector Word32))) where
  fromBlankedJson bj = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS    = blankedJsonBssToInterestBitsBs (getBlankedJson bj)
          newLen        = (BS.length interestBS + 3) `div` 4 * 4

instance FromBlankedJson (JsonInterestBits (BitShown (DVS.Vector Word64))) where
  fromBlankedJson bj    = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS    = blankedJsonBssToInterestBitsBs (getBlankedJson bj)
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

instance FromBlankedJson (JsonInterestBits Poppy512) where
  fromBlankedJson = JsonInterestBits . makePoppy512 . bitShown . getJsonInterestBits . fromBlankedJson
