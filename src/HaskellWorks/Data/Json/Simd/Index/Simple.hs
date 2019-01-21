{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Data.Json.Simd.Index.Simple
  ( makeIbBps
  ) where

import Control.Monad.ST
import Data.Word
import HaskellWorks.Data.Json.Simd.Internal.Index.Simple

import qualified Control.Monad.ST.Unsafe                      as ST
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.Vector.Storable.Mutable                 as DVSM
import qualified Foreign.ForeignPtr                           as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified Foreign.Marshal.Unsafe                       as F
import qualified Foreign.Ptr                                  as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F
import qualified HaskellWorks.Data.Json.Simd.Internal.List    as L
import qualified System.IO.Unsafe                             as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

makeIbBps :: LBS.ByteString -> [(BS.ByteString, BS.ByteString)]
makeIbBps lbs = L.zipPadded BS.empty BS.empty ibs bps
  where chunks  = makeIbs lbs
        ibs     = fmap (\(a, _, _) -> a) chunks
        bps     = ibsToIndexByteStrings chunks

makeIbs :: LBS.ByteString -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
makeIbs lbs = F.unsafeLocalState $ do
  wb <- allocWorkBuffers (32 * 1024 * 1204)
  ws <- allocWorkState
  IO.unsafeInterleaveIO $ go wb ws (LBS.toChunks lbs)
  where go :: WorkBuffers -> WorkState -> [BS.ByteString] -> IO [(BS.ByteString, BS.ByteString, BS.ByteString)]
        go _  _  []       = return []
        go wb ws (bs:bss) = do
          let resLen = BS.length bs `div` 8
          resIbFptr  <- F.mallocForeignPtrBytes resLen
          resAFptr   <- F.mallocForeignPtrBytes resLen
          resBFptr   <- F.mallocForeignPtrBytes resLen
          let resIbPtr  = F.castPtr (F.unsafeForeignPtrToPtr resIbFptr)
          let resAPtr   = F.castPtr (F.unsafeForeignPtrToPtr resAFptr )
          let resBPtr   = F.castPtr (F.unsafeForeignPtrToPtr resBFptr )
          let (bsFptr, bsOff, bsLen) = BSI.toForeignPtr bs
          let bsPtr = F.castPtr (F.unsafeForeignPtrToPtr bsFptr)
          _ <- F.processChunk
            (F.plusPtr bsPtr bsOff) -- in_buffer:           Ptr UInt8
            (fromIntegral bsLen)    -- in_length:           Size
            (workBuffersD wb)       -- work_bits_of_d:      Ptr UInt8
            (workBuffersA wb)       -- work_bits_of_a:      Ptr UInt8
            (workBuffersZ wb)       -- work_bits_of_z:      Ptr UInt8
            (workBuffersQ wb)       -- work_bits_of_q:      Ptr UInt8
            (workBuffersB wb)       -- work_bits_of_b:      Ptr UInt8
            (workBuffersE wb)       -- work_bits_of_e:      Ptr UInt8
            (workStateZ ws)         -- last_trailing_ones:  Ptr Size
            (workStateO ws)         -- quote_odds_carry:    Ptr Size
            (workStateE ws)         -- quote_evens_carry:   Ptr Size
            (workStateM ws)         -- quote_mask_carry:    Ptr UInt64
            resIbPtr                -- result_ibs:          Ptr UInt8
            resAPtr                 -- result_a:            Ptr UInt8
            resBPtr                 -- result_z:            Ptr UInt8
          let r =
                ( BSI.fromForeignPtr resIbFptr 0 resLen
                , BSI.fromForeignPtr resAFptr  0 resLen
                , BSI.fromForeignPtr resBFptr  0 resLen
                )
          rs <- IO.unsafeInterleaveIO $ go wb ws bss
          return (r:rs)

ibsToIndexByteStrings :: ()
  => [(BS.ByteString, BS.ByteString, BS.ByteString)]
  -> [BS.ByteString]
ibsToIndexByteStrings bits = F.unsafeLocalState $ do
  bpState <- emptyBpState
  IO.unsafeInterleaveIO $ go bpState (fmap (\(a, b, c) -> mkIndexStep a b c) bits)
  where go :: ()
          => BpState
          -> [Step]
          -> IO [BS.ByteString]
        go s (step:steps) = do
          let bp = stepToByteString s step
          bps <- IO.unsafeInterleaveIO $ go s steps
          return $ bp:bps
        go s [] = return [stepToByteString s indexStepFinal]

mkIndexStep :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Step
mkIndexStep is as zs | isLen == asLen && asLen == zsLen = Step go isLen
  where isLen = BS.length is
        asLen = BS.length as
        zsLen = BS.length zs
        (isFptr, _, _) = BSI.toForeignPtr is
        (asFptr, _, _) = BSI.toForeignPtr as
        (zsFptr, _, _) = BSI.toForeignPtr zs
        go  :: BpState
            -> DVSM.MVector s Word64
            -> ST s Int
        go bpState bpvm = fmap fromIntegral . ST.unsafeIOToST $ do
          let (outFptr, _, _) = DVSM.unsafeToForeignPtr bpvm

          F.withForeignPtr outFptr $ \outPtr ->
            F.withForeignPtr isFptr $ \isPtr ->
              F.withForeignPtr asFptr $ \asPtr ->
                F.withForeignPtr zsFptr $ \zsPtr ->
                  F.withForeignPtr (bpStateP bpState) $ \bpStatePtr -> do
                    F.writeBpChunk
                      (F.castPtr isPtr)
                      (F.castPtr asPtr)
                      (F.castPtr zsPtr)
                      (fromIntegral (BS.length is))
                      (F.castPtr bpStatePtr)
                      (F.castPtr outPtr)
mkIndexStep _ _ _ = error "Mismatched input size"

indexStepFinal :: Step
indexStepFinal = Step go 2
  where go  :: BpState
            -> DVSM.MVector s Word64
            -> ST s Int
        go bpState bpvm = fmap fromIntegral . ST.unsafeIOToST $ do
          let (outFptr, _, _) = DVSM.unsafeToForeignPtr bpvm

          F.withForeignPtr outFptr $ \outPtr ->
            F.withForeignPtr (bpStateP bpState) $ \bpStatePtr -> do
              F.writeBpChunkFinal (F.castPtr bpStatePtr) (F.castPtr outPtr)

stepToByteString :: BpState -> Step -> BS.ByteString
stepToByteString state (Step step size) = F.unsafeLocalState $ do
  let bsSize = size * 8
  bpFptr <- BSI.mallocByteString bsSize
  let bpVm = DVSM.unsafeFromForeignPtr (F.castForeignPtr bpFptr) 0 size
  w64Size <- stToIO $ step state bpVm
  return (BSI.PS bpFptr 0 (w64Size * 8))
