{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Data.Json.Simd.Internal.Index.Simple where

import Control.Monad.ST
import Data.Word
import Foreign

import qualified Data.Vector.Storable.Mutable                 as DVSM
import qualified Foreign                                      as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data WorkBuffers = WorkBuffers
  { workBuffersP :: !(ForeignPtr F.UInt8)
  , workBuffersD :: !(Ptr F.UInt8)
  , workBuffersA :: !(Ptr F.UInt8)
  , workBuffersZ :: !(Ptr F.UInt8)
  , workBuffersQ :: !(Ptr F.UInt8)
  , workBuffersB :: !(Ptr F.UInt8)
  , workBuffersE :: !(Ptr F.UInt8)
  }

data WorkState = WorkState
  { workStateZ :: !(Ptr F.Size)
  , workStateO :: !(Ptr F.Size)
  , workStateE :: !(Ptr F.Size)
  , workStateM :: !(Ptr F.UInt64)
  , workStateP :: !(ForeignPtr Word8)
  }

newtype BpState = BpState
  { bpStateP :: ForeignPtr Word8
  }

data Step where
  Step :: ( forall s
            .   BpState
            ->  DVSM.MVector s Word64
            ->  ST s Int)
          -> Int
          -> Step

emptyBpState :: IO BpState
emptyBpState = do
  fptr <- F.mallocForeignPtrBytes 32
  return (BpState (F.castForeignPtr fptr))

allocWorkBuffers :: Int -> IO WorkBuffers
allocWorkBuffers n = do
  fptr <- F.mallocForeignPtrBytes (6 * n)
  let ptr = F.unsafeForeignPtrToPtr fptr
  return WorkBuffers
    { workBuffersP = fptr
    , workBuffersD = ptr `F.plusPtr`  0
    , workBuffersA = ptr `F.plusPtr`  n
    , workBuffersZ = ptr `F.plusPtr` (n * 2)
    , workBuffersQ = ptr `F.plusPtr` (n * 3)
    , workBuffersB = ptr `F.plusPtr` (n * 4)
    , workBuffersE = ptr `F.plusPtr` (n * 5)
    }

allocWorkState :: IO WorkState
allocWorkState = do
  fptr <- F.mallocForeignPtrBytes 256
  let ptr = F.unsafeForeignPtrToPtr fptr
  let ws = WorkState
        { workStateZ = ptr `F.plusPtr`  0
        , workStateO = ptr `F.plusPtr`  8
        , workStateE = ptr `F.plusPtr` (8 * 2)
        , workStateM = ptr `F.plusPtr` (8 * 3)
        , workStateP = fptr
        }
  F.poke (workStateZ ws) 0
  F.poke (workStateO ws) 0
  F.poke (workStateE ws) 1
  F.poke (workStateM ws) 0
  return ws
