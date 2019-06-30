{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Standard.Cursor.Internal.ToBalancedParens64
  ( ToBalancedParens64(..)
  ) where

import Control.Applicative
import Data.Word
import HaskellWorks.Data.Json.Standard.Cursor.Internal.MakeIndex

import qualified Data.ByteString.Lazy                                        as LBS
import qualified Data.Vector.Storable                                        as DVS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.BlankedJson as J

genBitWordsForever :: LBS.ByteString -> Maybe (Word8, LBS.ByteString)
genBitWordsForever bs = LBS.uncons bs <|> Just (0, bs)
{-# INLINE genBitWordsForever #-}

class ToBalancedParens64 a where
  toBalancedParens64 :: a -> DVS.Vector Word64

instance ToBalancedParens64 J.BlankedJson where
  toBalancedParens64 (J.BlankedJson bj) = DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever bpBS)
    where bpBS    = LBS.fromChunks (compressWordAsBit (blankedJsonToBalancedParens bj))
          newLen  = fromIntegral ((LBS.length bpBS + 7) `div` 8 * 8)
