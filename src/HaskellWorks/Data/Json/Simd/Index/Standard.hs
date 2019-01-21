{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Simd.Index.Standard
  ( makeIbBps
  ) where

import Control.Monad
import Data.Word
import HaskellWorks.Data.Json.Simd.Internal.Index.Standard

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.ByteString.Lazy                         as LBS
import qualified Foreign.ForeignPtr                           as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified Foreign.Marshal.Unsafe                       as F
import qualified Foreign.Ptr                                  as F
import qualified Foreign.Storable                             as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F
import qualified System.IO.Unsafe                             as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

makeIbBps :: LBS.ByteString -> [(BS.ByteString, BS.ByteString)]
makeIbBps lbs = F.unsafeLocalState $ do
  wb <- allocWorkBuffers (32 * 1024 * 1204)
  ws <- newWorkState 0
  fptrState       :: F.ForeignPtr F.UInt32  <- F.mallocForeignPtr
  fptrRemBits     :: F.ForeignPtr F.UInt64  <- F.mallocForeignPtr
  fptrRemBitsLen  :: F.ForeignPtr F.Size    <- F.mallocForeignPtr
  let ptrState      = F.unsafeForeignPtrToPtr fptrState
  let ptrRemBits    = F.unsafeForeignPtrToPtr fptrRemBits
  let ptrRemBitsLen = F.unsafeForeignPtrToPtr fptrRemBitsLen
  F.poke ptrState       0
  F.poke ptrRemBits     0
  F.poke ptrRemBitsLen  0
  IO.unsafeInterleaveIO $ go wb ws fptrState fptrRemBits fptrRemBitsLen (LBS.toChunks lbs)
  where go :: ()
          => WorkBuffers
          -> WorkState
          -> F.ForeignPtr F.UInt32
          -> F.ForeignPtr F.UInt64
          -> F.ForeignPtr F.Size
          -> [BS.ByteString]
          -> IO [(BS.ByteString, BS.ByteString)]
        go _ _ _ fptrRemBits fptrRemBitsLen  []       = do
          resBpFptr  <- F.mallocForeignPtrBytes 8
          let resBpPtr      = F.castPtr (F.unsafeForeignPtrToPtr resBpFptr  )
          let ptrRemBits    = F.unsafeForeignPtrToPtr fptrRemBits
          let ptrRemBitsLen = F.unsafeForeignPtrToPtr fptrRemBitsLen
          remBits     <- F.peek ptrRemBits
          remBitsLen  <- F.peek ptrRemBitsLen
          bpByteLen <- F.smWriteBpChunkFinal
            remBits     -- remaining_bp_bits
            remBitsLen  -- remaning_bp_bits_len
            resBpPtr    -- out_buffer
          return  [ ( BS.empty
                    , BSI.fromForeignPtr resBpFptr 0 (fromIntegral bpByteLen * 8)
                    )
                  ]
        go wb ws fptrState fptrRemBits fptrRemBitsLen (bs:bss) = do
          let (!bsFptr, !bsOff, !bsLen) = BSI.toForeignPtr bs
          let !idxByteLen = (bsLen + 7) `div` 8
          resIbFptr  <- F.mallocForeignPtrBytes idxByteLen
          resBpFptr  <- F.mallocForeignPtrBytes idxByteLen
          let resIbPtr      = F.castPtr (F.unsafeForeignPtrToPtr resIbFptr  )
          let resBpPtr      = F.castPtr (F.unsafeForeignPtrToPtr resBpFptr  )
          let bsPtr         = F.castPtr (F.unsafeForeignPtrToPtr bsFptr)
          let ptrState      = F.unsafeForeignPtrToPtr fptrState
          let ptrRemBits    = F.unsafeForeignPtrToPtr fptrRemBits
          let ptrRemBitsLen = F.unsafeForeignPtrToPtr fptrRemBitsLen
          s :: Word8 <- fromIntegral <$> F.peek ptrState
          void $ F.smProcessChunk
            (F.plusPtr bsPtr bsOff) -- in_buffer:   Ptr UInt8
            (fromIntegral bsLen)    -- in_length:   Size
            ptrState                -- work state:  Ptr UInt32
            (workBuffersP wb)       -- result_phi:  Ptr UInt8
          void $ F.smMakeIbOpClChunks
            (fromIntegral s)          -- state
            (workBuffersP wb)         -- in_phis
            (fromIntegral bsLen)      -- phi_length
            resIbPtr                  -- out_ibs
            (workBuffersO wb)         -- out_ops
            (workBuffersC wb)         -- out_cls
          bpByteLen <- F.smWriteBpChunk
            (workBuffersO wb)         -- result_op
            (workBuffersC wb)         -- result_cl
            (fromIntegral idxByteLen) -- ib_bytes
            ptrRemBits                -- remaining_bp_bits
            ptrRemBitsLen             -- remaning_bp_bits_len
            resBpPtr                  -- out_buffer
          let !r =
                ( BSI.fromForeignPtr resIbFptr 0 idxByteLen
                , BSI.fromForeignPtr resBpFptr 0 (fromIntegral bpByteLen * 8)
                )
          rs <- IO.unsafeInterleaveIO $ go wb ws fptrState fptrRemBits fptrRemBitsLen bss
          return (r:rs)
