{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Internal.Cursor.BalancedParens
  ( JsonBalancedParens(..)
  , getJsonBalancedParens
  ) where

import Control.Applicative
import Data.Word
import HaskellWorks.Data.BalancedParens                   as BP
import HaskellWorks.Data.Json.Internal.Cursor.BlankedJson
import HaskellWorks.Data.Json.Internal.MakeIndex

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

newtype JsonBalancedParens a = JsonBalancedParens a

getJsonBalancedParens :: JsonBalancedParens a -> a
getJsonBalancedParens (JsonBalancedParens a) = a

genBitWordsForever :: BS.ByteString -> Maybe (Word8, BS.ByteString)
genBitWordsForever bs = BS.uncons bs <|> Just (0, bs)
{-# INLINE genBitWordsForever #-}

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens [Bool])) where
  fromBlankedJson (BlankedJson bj) = JsonBalancedParens (SimpleBalancedParens (blankedJsonToBalancedParens bj))

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word8))) where
  fromBlankedJson bj  = JsonBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever bpBS)))
    where bpBS        = BS.concat ((compressWordAsBit . blankedJsonToBalancedParens2) (getBlankedJson bj))
          newLen      = (BS.length bpBS + 7) `div` 8 * 8

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word16))) where
  fromBlankedJson bj  = JsonBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever bpBS)))
    where bpBS        = BS.concat ((compressWordAsBit . blankedJsonToBalancedParens2) (getBlankedJson bj))
          newLen      = (BS.length bpBS + 7) `div` 8 * 8

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word32))) where
  fromBlankedJson bj  = JsonBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever bpBS)))
    where bpBS        = BS.concat ((compressWordAsBit . blankedJsonToBalancedParens2) (getBlankedJson bj))
          newLen      = (BS.length bpBS + 7) `div` 8 * 8

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word64))) where
  fromBlankedJson bj  = JsonBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever bpBS)))
    where bpBS        = BS.concat ((compressWordAsBit . blankedJsonToBalancedParens2) (getBlankedJson bj))
          newLen      = (BS.length bpBS + 7) `div` 8 * 8
