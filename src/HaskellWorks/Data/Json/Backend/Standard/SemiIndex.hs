{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Json.Backend.Standard.SemiIndex
  ( semiIndexBuilder
  , SemiIndex(..)
  , buildSemiIndex
  , stateMachine
  ) where

import Control.Monad.ST
import Data.Word

import qualified Data.ByteString                                                     as BS
import qualified Data.ByteString.Builder                                             as B
import qualified Data.ByteString.Lazy                                                as LBS
import qualified Data.ByteString.Unsafe                                              as BSU
import qualified Data.Vector.Storable                                                as DVS
import qualified HaskellWorks.Data.Bits.Writer.Storable                              as W
import qualified HaskellWorks.Data.ByteString                                        as BS
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.Blank              as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex          as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.ToBalancedParens64 as J
import qualified HaskellWorks.Data.Json.Internal.Word8                               as W8

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data State = InJson | InString | InEscape | InValue deriving (Eq, Enum, Bounded, Show)

data SemiIndex v = SemiIndex
  { semiIndexIb :: !v
  , semiIndexBp :: !v
  } deriving (Functor, Traversable, Foldable)

semiIndexBuilder :: LBS.ByteString -> SemiIndex B.Builder
semiIndexBuilder lbs = SemiIndex (B.lazyByteString ibs) (B.byteString (BS.toByteString bps))
  where blankedJson = J.blankJson (LBS.toChunks lbs)
        ibs = LBS.fromChunks (J.blankedJsonToInterestBits blankedJson)
        bps = J.toBalancedParens64 (J.BlankedJson blankedJson)
{-# INLINE semiIndexBuilder #-}

buildSemiIndex :: BS.ByteString -> SemiIndex (DVS.Vector Word64)
buildSemiIndex bs = DVS.createT $ do
  let len = (BS.length bs + 7) `div` 8
  mib <- W.newWriter len
  mbp <- W.newWriter (len * 2)
  buildFromByteString mib mbp bs 0 InJson
{-# INLINE buildSemiIndex #-}

buildFromByteString :: W.Writer s -> W.Writer s -> BS.ByteString -> Int -> State -> ST s (SemiIndex (DVS.MVector s Word64))
buildFromByteString ib bp bs i = go
  where go state = if i < BS.length bs
          then do
            let c = BSU.unsafeIndex bs i
            -- let bp0 = bp
            case state of
              InJson -> if
                | c == W8.openBracket || c == W8.openBrace -> do
                  W.unsafeWriteBit ib 1
                  W.unsafeWriteBit bp 1
                  buildFromByteString ib bp bs (i + 1) InJson
                | c == W8.closeBracket || c == W8.closeBrace -> do
                  W.unsafeWriteBit bp 0
                  W.unsafeWriteBit ib 0
                  buildFromByteString ib bp bs (i + 1) InJson
                | c == W8.comma || c == W8.colon -> do
                  buildFromByteString ib bp bs (i + 1) InJson
                | W8.isAlphabetic c || W8.isDigit c || W8.isPeriod c || W8.isMinus c || W8.isPlus c -> do
                  W.unsafeWriteBit ib 1
                  buildFromByteString ib bp bs (i + 1) InValue
                | c == W8.doubleQuote -> do
                  W.unsafeWriteBit ib 1
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
              InValue -> if
                | W8.isAlphabetic c || W8.isDigit c || W8.isPeriod c || W8.isMinus c || W8.isPlus c -> do
                  W.unsafeWriteBit ib 0
                  buildFromByteString ib bp bs (i + 1) InValue
                | otherwise -> go InJson
          else do
            ibv <- W.written ib
            bpv <- W.written bp
            return (SemiIndex ibv bpv)
{-# INLINE buildFromByteString #-}

stateMachine :: State -> Word8 -> (State, Bool, Bool)
stateMachine InJson   c | W8.isOpen c         = (InJson  , True  , True  )
stateMachine InJson   c | W8.isClose c        = (InJson  , False , False )
stateMachine InJson   c | W8.isDelim c        = (InJson  , False , False )
stateMachine InJson   c | W8.isValueChar c    = (InValue , True  , False )
stateMachine InJson   c | W8.isDoubleQuote c  = (InString, True  , False )
stateMachine InJson   _ | otherwise           = (InJson  , False , False )
stateMachine InString c | W8.isDoubleQuote c  = (InJson  , False , False )
stateMachine InString c | W8.isBackSlash c    = (InEscape, False , False )
stateMachine InString _ | otherwise           = (InString, False , False )
stateMachine InEscape _ | otherwise           = (InString, False , False )
stateMachine InValue  c | W8.isValueChar c    = (InValue , False , False )
stateMachine InValue  c | otherwise           = stateMachine InJson c
{-# INLINE stateMachine #-}
