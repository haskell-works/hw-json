{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Json.Simple.Cursor.SemiIndex
  ( buildSemiIndex
  , SemiIndex(..)
  ) where

import Control.Monad.ST
import Data.Word

import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Unsafe                              as BSU
import qualified Data.Vector.Storable                                as DVS
import qualified HaskellWorks.Data.Bits.Writer.Storable              as W
import qualified HaskellWorks.Data.Json.Simple.Cursor.Internal.Word8 as W8

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data Context = InJson | InString | InEscape deriving (Eq, Show)

data SemiIndex v = SemiIndex
  { semiIndexContext :: !Context
  , semiIndexIb      :: !v
  , semiIndexBp      :: !v
  } deriving (Functor, Traversable, Foldable)

buildSemiIndex :: BS.ByteString -> SemiIndex (DVS.Vector Word64)
buildSemiIndex bs = DVS.createT $ do
  let len = (BS.length bs + 7) `div` 8
  mib <- W.newWriter len
  mbp <- W.newWriter (len * 2)
  buildFromByteString mib mbp bs 0 InJson
{-# INLINE buildSemiIndex #-}

buildFromByteString :: W.Writer s -> W.Writer s -> BS.ByteString -> Int -> Context -> ST s (SemiIndex (DVS.MVector s Word64))
buildFromByteString ib bp bs i context = if i < BS.length bs
  then do
    let c = BSU.unsafeIndex bs i
    case context of
      InJson -> if
        | c == W8.openBracket || c == W8.openBrace -> do
          W.unsafeWriteBit bp 1
          W.unsafeWriteBit bp 1
          W.unsafeWriteBit ib 1
          buildFromByteString ib bp bs (i + 1) InJson
        | c == W8.closeBracket || c == W8.closeBrace -> do
          W.unsafeWriteBit bp 0
          W.unsafeWriteBit bp 0
          W.unsafeWriteBit ib 1
          buildFromByteString ib bp bs (i + 1) InJson
        | c == W8.comma || c == W8.colon -> do
          W.unsafeWriteBit bp 0
          W.unsafeWriteBit bp 1
          W.unsafeWriteBit ib 1
          buildFromByteString ib bp bs (i + 1) InJson
        | c == W8.doubleQuote -> do
          W.unsafeWriteBit ib 0
          buildFromByteString ib bp bs (i + 1) InString
        | otherwise -> do
          W.unsafeWriteBit ib 0
          buildFromByteString ib bp bs (i + 1) InJson
      InString -> do
        W.unsafeWriteBit ib 0
        let newContext = if
              | c == W8.doubleQuote  -> InJson
              | c == W8.backSlash    -> InEscape
              | otherwise           -> InString
        buildFromByteString ib bp bs (i + 1) newContext
      InEscape -> do
        W.unsafeWriteBit ib 0
        buildFromByteString ib bp bs (i + 1) InString
  else do
    ibv <- W.written ib
    bpv <- W.written bp
    return (SemiIndex context ibv bpv)
{-# INLINE buildFromByteString #-}
