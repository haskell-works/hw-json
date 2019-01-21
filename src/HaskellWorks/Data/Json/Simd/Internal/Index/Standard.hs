{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Data.Json.Simd.Internal.Index.Standard where

import Data.Word
import Foreign

import qualified Foreign                                      as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data WorkBuffers = WorkBuffers
  { workBuffersF :: !(ForeignPtr F.UInt8)
  , workBuffersP :: !(Ptr F.UInt32)
  , workBuffersO :: !(Ptr F.UInt8)
  , workBuffersC :: !(Ptr F.UInt8)
  }

data WorkState = WorkState
  { workStateFptr       :: !(ForeignPtr Word8)
  , workStateRemBits    :: !(Ptr F.UInt64)
  , workStateRemBitsLen :: !(Ptr F.Size)
  , workStateState      :: !(Ptr F.UInt32)
  }

allocWorkBuffers :: Int -> IO WorkBuffers
allocWorkBuffers n = do
  fptr <- F.mallocForeignPtrBytes (3 * n)
  let ptr = F.unsafeForeignPtrToPtr fptr
  return WorkBuffers
    { workBuffersF = fptr
    , workBuffersP = ptr `F.plusPtr`  0
    , workBuffersO = ptr `F.plusPtr`  n
    , workBuffersC = ptr `F.plusPtr` (n * 2)
    }

newWorkState :: Word32 -> IO WorkState
newWorkState initialValue = do
  fptr <- F.mallocForeignPtrBytes 256
  let ptr = F.unsafeForeignPtrToPtr fptr
  let ws = WorkState
        { workStateState      = ptr `F.plusPtr`  0
        , workStateRemBits    = ptr `F.plusPtr`  8
        , workStateRemBitsLen = ptr `F.plusPtr`  16
        , workStateFptr       = fptr
        }
  F.poke (workStateState      ws) (fromIntegral initialValue)
  F.poke (workStateRemBits    ws) 0
  F.poke (workStateRemBitsLen ws) 0
  return ws
