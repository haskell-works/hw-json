{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellWorks.Data.Json.Simd.Internal.Foreign where

import Foreign
import Foreign.C.Types (CChar(..), CSize(..))

#include "simd.h"

type UInt8  = {#type uint8_t #}
type UInt32 = {#type uint32_t#}
type UInt64 = {#type uint64_t#}
type Size   = {#type size_t  #}

foreign import ccall "run" run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

processChunk :: ()
  => Ptr UInt8    -- in_buffer
  -> Size         -- in_length
  -> Ptr UInt8    -- work_bits_of_d
  -> Ptr UInt8    -- work_bits_of_a
  -> Ptr UInt8    -- work_bits_of_z
  -> Ptr UInt8    -- work_bits_of_q
  -> Ptr UInt8    -- work_bits_of_b
  -> Ptr UInt8    -- work_bits_of_e
  -> Ptr Size     -- last_trailing_ones
  -> Ptr Size     -- quote_odds_carry
  -> Ptr Size     -- quote_evens_carry
  -> Ptr UInt64   -- quote_mask_carry
  -> Ptr UInt8    -- result_ibs
  -> Ptr UInt8    -- result_a
  -> Ptr UInt8    -- result_z
  -> IO UInt64
processChunk = do
  {#call unsafe process_chunk as c_process_chunk#}
{-# INLINE processChunk #-}

initBpState :: ()
  => Ptr ()
  -> IO ()
initBpState = {#call unsafe init_bp_state as c_init_bp_state#}
{-# INLINE initBpState #-}

writeBpChunk :: ()
  => Ptr UInt8  -- result_ib
  -> Ptr UInt8  -- result_a
  -> Ptr UInt8  -- result_z
  -> Size       -- ib_bytes
  -> Ptr ()     -- bp_state
  -> Ptr UInt8  -- out_buffer
  -> IO Size
writeBpChunk = {#call unsafe write_bp_chunk as c_write_bp_chunk#}
{-# INLINE writeBpChunk #-}

writeBpChunkFinal :: ()
  => Ptr ()     -- bp_state
  -> Ptr UInt8  -- out_buffer
  -> IO Size
writeBpChunkFinal = {#call unsafe write_bp_chunk_final as c_write_bp_chunk_final#}
{-# INLINE writeBpChunkFinal #-}

smProcessChunk :: ()
  => Ptr UInt8    -- in_buffer
  -> Size         -- in_length
  -> Ptr UInt32   -- inout_state
  -> Ptr UInt32   -- out_phi_buffer
  -> IO ()
smProcessChunk = {#call unsafe sm_process_chunk as c_sm_process_chunk#}

smMakeIbOpClChunks :: ()
  => UInt8        -- state
  -> Ptr UInt32   -- in_phis
  -> Size         -- phi_length
  -> Ptr UInt8    -- out_ibs
  -> Ptr UInt8    -- out_ops
  -> Ptr UInt8    -- out_cls
  -> IO ()
smMakeIbOpClChunks = {#call unsafe sm_make_ib_op_cl_chunks as c_sm_make_ib_op_cl_chunks#}

smWriteBpChunk :: ()
  => Ptr UInt8    -- result_op
  -> Ptr UInt8    -- result_cl
  -> Size         -- ib_bytes
  -> Ptr UInt64   -- remaining_bp_bits
  -> Ptr Size     -- remaning_bp_bits_len
  -> Ptr UInt64   -- out_buffer
  -> IO Size
smWriteBpChunk = {#call unsafe sm_write_bp_chunk as c_sm_write_bp_chunk#}

smWriteBpChunkFinal :: ()
  => UInt64       -- remaining_bits
  -> Size         -- remaining_bits_len
  -> Ptr UInt64   -- out_buffer
  -> IO Size
smWriteBpChunkFinal = {#call unsafe sm_write_bp_chunk_final as c_sm_write_bp_chunk_final#}
