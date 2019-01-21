#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <immintrin.h>

#include "debug.h"

#include "simd.h"

extern uint32_t simd_transition_table_32[256];
extern uint32_t simd_phi_table_32       [256];

void
sm_process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint32_t *inout_state,
    uint32_t *out_phi_buffer) {
  __m128i s = _mm_set_epi64x(0, *inout_state);

  for (size_t i = 0; i < in_length; i += 1) {
    uint8_t w = in_buffer[i];
    __m128i p = _mm_shuffle_epi8(_mm_set1_epi32(simd_phi_table_32[w]), s);
    out_phi_buffer[i] = _mm_extract_epi32(p, 0);
    s = _mm_shuffle_epi8(_mm_set1_epi32(simd_transition_table_32[w]), s);
  }

  *inout_state = (uint32_t)_mm_extract_epi32(s, 0);
}

void
sm_make_ib_op_cl_chunks(
    uint8_t state,
    uint32_t *in_phis,
    size_t phi_length,
    uint8_t *out_ibs,
    uint8_t *out_ops,
    uint8_t *out_cls) {
  uint32_t state_offset = state * 8;

  uint32_t ib_offset = 5;
  uint32_t op_offset = 6;
  uint32_t cl_offset = 7;

  for (size_t i = 0; i < phi_length; i += 8) {
    __m256i v_8 = *(__m256i *)&in_phis[i];
    __m256i v_ib_8 = _mm256_slli_epi64(_mm256_srli_epi64(v_8, state_offset), ib_offset);
    __m256i v_op_8 = _mm256_slli_epi64(_mm256_srli_epi64(v_8, state_offset), op_offset);
    __m256i v_cl_8 = _mm256_slli_epi64(_mm256_srli_epi64(v_8, state_offset), cl_offset);
    uint8_t all_ibs = (uint8_t)_pext_u32(_mm256_movemask_epi8(v_ib_8), 0x11111111);
    uint8_t all_ops = (uint8_t)_pext_u32(_mm256_movemask_epi8(v_op_8), 0x11111111);
    uint8_t all_cls = (uint8_t)_pext_u32(_mm256_movemask_epi8(v_cl_8), 0x11111111);

    size_t j = i / 8;
    out_ibs[j] = all_ibs;
    out_ops[j] = all_ops;
    out_cls[j] = all_cls;
  }
}

size_t
sm_write_bits(
    uint64_t bits,
    size_t bits_len,
    uint64_t *remaining_bits,
    size_t *remaning_bits_len,
    uint64_t *out_buffer);

size_t
sm_write_bp_chunk(
    uint8_t *result_op,
    uint8_t *result_cl,
    size_t ib_bytes,
    uint64_t *remaining_bits,
    size_t *remaning_bits_len,
    uint64_t *out_buffer) {
  uint64_t *w64_result_op = (uint64_t *)result_op;
  uint64_t *w64_result_cl = (uint64_t *)result_cl;
  uint64_t *w64_work_bp   = (uint64_t *)out_buffer;

  uint64_t  w64_len           = ib_bytes / 8;
  size_t    w64s_ready        = 0;

  for (size_t i = 0; i < w64_len; ++i) {
    uint64_t w64_op = w64_result_op[i];
    uint64_t w64_cl = w64_result_cl[i];

    uint64_t w64_op_lo = w64_op;
    uint64_t w64_op_hi = w64_op >> 32;

    uint64_t w64_cl_lo = w64_cl;
    uint64_t w64_cl_hi = w64_cl >> 32;

    uint64_t op_lo = _pdep_u64(w64_op_lo, 0x5555555555555555);
    uint64_t cl_lo = _pdep_u64(w64_cl_lo, 0xaaaaaaaaaaaaaaaa);
    uint64_t ib_lo = op_lo | cl_lo;

    uint64_t op_hi = _pdep_u64(w64_op_hi, 0x5555555555555555);
    uint64_t cl_hi = _pdep_u64(w64_cl_hi, 0xaaaaaaaaaaaaaaaa);
    uint64_t ib_hi = op_hi | cl_hi;

    size_t pc_ib_lo = __builtin_popcountll(ib_lo);
    size_t pc_ib_hi = __builtin_popcountll(ib_hi);

    uint64_t ext_lo = _pext_u64(op_lo, ib_lo);
    uint64_t ext_hi = _pext_u64(op_hi, ib_hi);

    w64s_ready += sm_write_bits(ext_lo, pc_ib_lo, remaining_bits, remaning_bits_len, w64_work_bp + w64s_ready);
    w64s_ready += sm_write_bits(ext_hi, pc_ib_hi, remaining_bits, remaning_bits_len, w64_work_bp + w64s_ready);
  }

  return w64s_ready;
}

size_t
sm_write_bits(
    uint64_t bits,
    size_t bits_len,
    uint64_t *remaining_bits,
    size_t *remaining_bits_len,
    uint64_t *out_buffer) {
  *remaining_bits |= (bits << *remaining_bits_len);

  if (*remaining_bits_len + bits_len >= 64) {
    *out_buffer = *remaining_bits;

    *remaining_bits = bits >> (64 - *remaining_bits_len);

    *remaining_bits_len = *remaining_bits_len + bits_len - 64;

    return 1;
  } else {
    *remaining_bits_len += bits_len;

    return 0;
  }
}

size_t
sm_write_bp_chunk_final(
    uint64_t remaining_bits,
    size_t remaining_bits_len,
    uint64_t *out_buffer) {
  if (remaining_bits_len > 0) {
    size_t zero_len = 64 - remaining_bits_len;

    *out_buffer = (remaining_bits << zero_len) >> zero_len;

    return 1;
  } else {
    return 0;
  }
}
