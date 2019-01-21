#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <immintrin.h>
#include <mmintrin.h>

#include "simd.h"

#define W8_BUFFER_SIZE    (1024 * 32)
#define W32_BUFFER_SIZE   (W8_BUFFER_SIZE / 4)
#define W64_BUFFER_SIZE   (W8_BUFFER_SIZE / 8)

typedef struct bp_state {
  uint64_t  remainder_bits_d;
  uint64_t  remainder_bits_a;
  uint64_t  remainder_bits_z;
  size_t    remainder_len;
} bp_state_t;

int main_spliced(
    int argc,
    char **argv) {
  if (argc != 4) {
    fprintf(stderr, "./a.out <input-file> <output-ib-file> <output-bp-file>\n");
    exit(1);
  }

  char *in_filename     = argv[1];
  char *ib_out_filename = argv[2];
  char *bp_out_filename = argv[3];

  FILE *in = fopen(in_filename, "r");

  if (!in) {
    fprintf(stderr, "Failed to open input file %s\n", in_filename);
    exit(1);
  }

  FILE *ib_out = fopen(ib_out_filename, "w");
  
  if (!ib_out) {
    fprintf(stderr, "Failed to open ib output file %s\n", ib_out_filename);
    exit(1);
  }

  FILE *bp_out = fopen(bp_out_filename, "w");
  
  if (!bp_out) {
    fprintf(stderr, "Failed to open bp output file %s\n", bp_out_filename);
    exit(1);
  }

  uint8_t buffer[W8_BUFFER_SIZE];

  uint8_t *bits_of_d = malloc(W32_BUFFER_SIZE); memset(bits_of_d, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_a = malloc(W32_BUFFER_SIZE); memset(bits_of_a, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_z = malloc(W32_BUFFER_SIZE); memset(bits_of_z, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_b = malloc(W32_BUFFER_SIZE); memset(bits_of_b, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_e = malloc(W32_BUFFER_SIZE); memset(bits_of_e, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_q = malloc(W32_BUFFER_SIZE); memset(bits_of_q, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_w = malloc(W32_BUFFER_SIZE); memset(bits_of_q, 0, W32_BUFFER_SIZE);

  uint8_t result_ib[W8_BUFFER_SIZE / 8];
  uint8_t result_a [W8_BUFFER_SIZE / 8];
  uint8_t result_z [W8_BUFFER_SIZE / 8];
  bp_state_t bp_state;

  uint64_t accum = 0;

  size_t last_trailing_ones = 0;
  size_t total_bytes_read   = 0;
  uint64_t quote_mask_carry = 0;
  size_t quote_odds_carry   = 0;
  size_t quote_evens_carry  = 1;

  uint8_t out_bp_buffer[W32_BUFFER_SIZE * 2];

  while (1) {
    size_t bytes_read = fread(buffer, 1, W8_BUFFER_SIZE, in);

    total_bytes_read += bytes_read;

    if (bytes_read < W8_BUFFER_SIZE) {
      if (ferror(in)) {
        fprintf(stderr, "Error reading file\n");
        exit(1);
      }

      if (bytes_read == 0) {
        if (feof(in)) {
          break;
        }
      }

      size_t next_alignment = ((bytes_read + 63) / 64) * 64;

      memset(buffer + bytes_read, 0, next_alignment - bytes_read);

      bytes_read = next_alignment;
    }

    accum += process_chunk(buffer, bytes_read,
      bits_of_d,
      bits_of_a,
      bits_of_z,
      bits_of_q,
      bits_of_b,
      bits_of_e,
      &last_trailing_ones,
      &quote_odds_carry,
      &quote_evens_carry,
      &quote_mask_carry,
      result_ib,
      result_a,
      result_z);

    size_t ib_bytes = (bytes_read + 7) / 8;

    fwrite(result_ib, 1, ib_bytes, ib_out);

    size_t out_bp_bytes = write_bp_chunk(
      result_ib,
      result_a,
      result_z,
      ib_bytes,
      &bp_state,
      out_bp_buffer);

    fwrite(out_bp_buffer, out_bp_bytes, sizeof(uint64_t), bp_out);

    fflush(ib_out);
    fflush(bp_out);
  }

  write_bp_chunk_final(&bp_state, out_bp_buffer);

  fwrite(out_bp_buffer, 2, sizeof(uint64_t), bp_out);

  fclose(in);
  fclose(ib_out);

  return 0;
}

void init_bp_state(
    bp_state_t *bp_state) {
  memset(bp_state, 0, sizeof(*bp_state));
}

size_t write_bp_chunk(
    uint8_t *result_ib,
    uint8_t *result_a,
    uint8_t *result_z,
    size_t ib_bytes,
    bp_state_t *bp_state,
    uint8_t *out_buffer) {
  uint64_t *w64_result_ib = (uint64_t *)result_ib;
  uint64_t *w64_result_a  = (uint64_t *)result_a;
  uint64_t *w64_result_z  = (uint64_t *)result_z;
  uint64_t *w64_work_bp   = (uint64_t *)out_buffer;

  uint64_t  w64_len           = ib_bytes / 8;
  uint64_t  remainder_bits_d  = (*bp_state).remainder_bits_d;
  uint64_t  remainder_bits_a  = (*bp_state).remainder_bits_a;
  uint64_t  remainder_bits_z  = (*bp_state).remainder_bits_z;
  size_t    remainder_len     = (*bp_state).remainder_len;
  size_t    w64s_ready        = 0;

  for (size_t i = 0; i < w64_len; ++i) {
    uint64_t w64_ib = w64_result_ib[i];
    uint64_t w64_a  = w64_result_a[i];
    uint64_t w64_z  = w64_result_z[i];

    size_t pc_ib = __builtin_popcountll(w64_ib);

    uint64_t ext_d = _pext_u64(~(w64_a | w64_z) , w64_ib);
    uint64_t ext_a = _pext_u64(w64_a            , w64_ib);
    uint64_t ext_z = _pext_u64(w64_z            , w64_ib);

    remainder_bits_d |= (ext_d << remainder_len);
    remainder_bits_a |= (ext_a << remainder_len);
    remainder_bits_z |= (ext_z << remainder_len);

    if (remainder_len + pc_ib >= 64) {
      // Write full word
      w64_work_bp[w64s_ready] =
        _pdep_u64(remainder_bits_a, 0x5555555555555555) |
        _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
        _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

      w64s_ready += 1;

      remainder_bits_a = remainder_bits_a >> 32;
      remainder_bits_z = remainder_bits_z >> 32;
      remainder_bits_d = remainder_bits_d >> 32;

      w64_work_bp[w64s_ready] =
        _pdep_u64(remainder_bits_a, 0x5555555555555555) |
        _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
        _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

      w64s_ready += 1;

      // Set up for next iteration
      remainder_bits_d = ext_d >> (64 - remainder_len);
      remainder_bits_a = ext_a >> (64 - remainder_len);
      remainder_bits_z = ext_z >> (64 - remainder_len);

      remainder_len = remainder_len + pc_ib - 64;
    } else {
      remainder_len += pc_ib;
    }
  }

  (*bp_state).remainder_bits_d  = remainder_bits_d;
  (*bp_state).remainder_bits_a  = remainder_bits_a;
  (*bp_state).remainder_bits_z  = remainder_bits_z;
  (*bp_state).remainder_len     = remainder_len;

  return w64s_ready;
}

size_t write_bp_chunk_final(
    bp_state_t *bp_state,
    uint8_t *out_buffer) {
  uint64_t *w64_work_bp   = (uint64_t *)out_buffer;

  uint64_t  remainder_bits_d  = (*bp_state).remainder_bits_d;
  uint64_t  remainder_bits_a  = (*bp_state).remainder_bits_a;
  uint64_t  remainder_bits_z  = (*bp_state).remainder_bits_z;
  size_t    w64s_ready        = 0;

  // Write full word
  w64_work_bp[w64s_ready] =
    _pdep_u64(remainder_bits_a, 0x5555555555555555) |
    _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
    _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

  w64s_ready += 1;

  remainder_bits_a = remainder_bits_a >> 32;
  remainder_bits_z = remainder_bits_z >> 32;
  remainder_bits_d = remainder_bits_d >> 32;

  w64_work_bp[w64s_ready] =
    _pdep_u64(remainder_bits_a, 0x5555555555555555) |
    _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
    _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

  w64s_ready += 1;

  return w64s_ready;
}

uint8_t escape_mask[2][256] =
  { { 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xdf, 0xdd, 0xdb, 0xdf, 0xd7, 0xd5, 0xdf, 0xd7, 0xff, 0xfd, 0xfb, 0xff, 0xdf, 0xdd, 0xff, 0xdf
    , 0xbf, 0xbd, 0xbb, 0xbf, 0xb7, 0xb5, 0xbf, 0xb7, 0xaf, 0xad, 0xab, 0xaf, 0xbf, 0xbd, 0xaf, 0xbf
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xbf, 0xbd, 0xbb, 0xbf, 0xff, 0xfd, 0xbf, 0xff
    , 0x7f, 0x7d, 0x7b, 0x7f, 0x77, 0x75, 0x7f, 0x77, 0x6f, 0x6d, 0x6b, 0x6f, 0x7f, 0x7d, 0x6f, 0x7f
    , 0x5f, 0x5d, 0x5b, 0x5f, 0x57, 0x55, 0x5f, 0x57, 0x7f, 0x7d, 0x7b, 0x7f, 0x5f, 0x5d, 0x7f, 0x5f
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0x7f, 0x7d, 0x7b, 0x7f, 0x77, 0x75, 0x7f, 0x77, 0xff, 0xfd, 0xfb, 0xff, 0x7f, 0x7d, 0xff, 0x7f
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xdf, 0xdd, 0xdb, 0xdf, 0xd7, 0xd5, 0xdf, 0xd7, 0xff, 0xfd, 0xfb, 0xff, 0xdf, 0xdd, 0xff, 0xdf
    , 0xbf, 0xbd, 0xbb, 0xbf, 0xb7, 0xb5, 0xbf, 0xb7, 0xaf, 0xad, 0xab, 0xaf, 0xbf, 0xbd, 0xaf, 0xbf
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xbf, 0xbd, 0xbb, 0xbf, 0xff, 0xfd, 0xbf, 0xff
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xdf, 0xdd, 0xdb, 0xdf, 0xd7, 0xd5, 0xdf, 0xd7, 0xff, 0xfd, 0xfb, 0xff, 0xdf, 0xdd, 0xff, 0xdf
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xff, 0xfd, 0xfb, 0xff, 0xff, 0xfd, 0xff, 0xff
    }
  , { 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xde, 0xdf, 0xda, 0xdb, 0xd6, 0xd7, 0xde, 0xdf, 0xfe, 0xff, 0xfa, 0xfb, 0xde, 0xdf, 0xfe, 0xff
    , 0xbe, 0xbf, 0xba, 0xbb, 0xb6, 0xb7, 0xbe, 0xbf, 0xae, 0xaf, 0xaa, 0xab, 0xbe, 0xbf, 0xae, 0xaf
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xbe, 0xbf, 0xba, 0xbb, 0xfe, 0xff, 0xbe, 0xbf
    , 0x7e, 0x7f, 0x7a, 0x7b, 0x76, 0x77, 0x7e, 0x7f, 0x6e, 0x6f, 0x6a, 0x6b, 0x7e, 0x7f, 0x6e, 0x6f
    , 0x5e, 0x5f, 0x5a, 0x5b, 0x56, 0x57, 0x5e, 0x5f, 0x7e, 0x7f, 0x7a, 0x7b, 0x5e, 0x5f, 0x7e, 0x7f
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0x7e, 0x7f, 0x7a, 0x7b, 0x76, 0x77, 0x7e, 0x7f, 0xfe, 0xff, 0xfa, 0xfb, 0x7e, 0x7f, 0xfe, 0xff
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xde, 0xdf, 0xda, 0xdb, 0xd6, 0xd7, 0xde, 0xdf, 0xfe, 0xff, 0xfa, 0xfb, 0xde, 0xdf, 0xfe, 0xff
    , 0xbe, 0xbf, 0xba, 0xbb, 0xb6, 0xb7, 0xbe, 0xbf, 0xae, 0xaf, 0xaa, 0xab, 0xbe, 0xbf, 0xae, 0xaf
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xbe, 0xbf, 0xba, 0xbb, 0xfe, 0xff, 0xbe, 0xbf
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xde, 0xdf, 0xda, 0xdb, 0xd6, 0xd7, 0xde, 0xdf, 0xfe, 0xff, 0xfa, 0xfb, 0xde, 0xdf, 0xfe, 0xff
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xfe, 0xff, 0xfa, 0xfb, 0xfe, 0xff, 0xfe, 0xff
    }
  };

void summarise(
    uint8_t *buffer,
    uint32_t *out_mask_d,
    uint32_t *out_mask_a,
    uint32_t *out_mask_z,
    uint32_t *out_mask_q,
    uint32_t *out_mask_b) {
#if defined AVX2_ENABLED
  __m256i v_in_data = *(__m256i *)buffer;
  __m256i v_bytes_of_comma      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(','));
  __m256i v_bytes_of_colon      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(':'));
  __m256i v_bytes_of_brace_a    = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('{'));
  __m256i v_bytes_of_brace_z    = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('}'));
  __m256i v_bytes_of_bracket_a  = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('['));
  __m256i v_bytes_of_bracket_z  = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(']'));
  __m256i v_bytes_of_quote      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('"'));
  __m256i v_bytes_of_backslash  = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('\\'));
  __m256i v_bytes_of_space      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(' '));
  __m256i v_bytes_of_tab        = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('\t'));
  __m256i v_bytes_of_cr         = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('\r'));
  __m256i v_bytes_of_lf         = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('\n'));

  uint32_t mask_comma     = (uint32_t)_mm256_movemask_epi8(v_bytes_of_comma     );
  uint32_t mask_colon     = (uint32_t)_mm256_movemask_epi8(v_bytes_of_colon     );
  uint32_t mask_brace_a   = (uint32_t)_mm256_movemask_epi8(v_bytes_of_brace_a   );
  uint32_t mask_brace_z   = (uint32_t)_mm256_movemask_epi8(v_bytes_of_brace_z   );
  uint32_t mask_bracket_a = (uint32_t)_mm256_movemask_epi8(v_bytes_of_bracket_a );
  uint32_t mask_bracket_z = (uint32_t)_mm256_movemask_epi8(v_bytes_of_bracket_z );
  uint32_t mask_space     = (uint32_t)_mm256_movemask_epi8(v_bytes_of_space     );
  uint32_t mask_tab       = (uint32_t)_mm256_movemask_epi8(v_bytes_of_tab       );
  uint32_t mask_cr        = (uint32_t)_mm256_movemask_epi8(v_bytes_of_cr        );
  uint32_t mask_lf        = (uint32_t)_mm256_movemask_epi8(v_bytes_of_lf        );

  *out_mask_d = mask_comma    | mask_colon;
  *out_mask_a = mask_brace_a  | mask_bracket_a;
  *out_mask_z = mask_brace_z  | mask_bracket_z;
  *out_mask_q = (uint32_t)_mm256_movemask_epi8(v_bytes_of_quote    );
  *out_mask_b = (uint32_t)_mm256_movemask_epi8(v_bytes_of_backslash);
#elif defined SSE42_ENABLED
  __m128i v_in_data_0 = *((__m128i *)buffer    );
  __m128i v_in_data_1 = *((__m128i *)buffer + 1);
  uint16_t *out_w32_mask_d = (uint16_t *)out_mask_d;
  uint16_t *out_w32_mask_a = (uint16_t *)out_mask_a;
  uint16_t *out_w32_mask_z = (uint16_t *)out_mask_z;
  uint16_t *out_w32_mask_q = (uint16_t *)out_mask_q;
  uint16_t *out_w32_mask_b = (uint16_t *)out_mask_b;
  out_w32_mask_d[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)":,", 2, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_d[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)":,", 2, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_a[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"{[", 2, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_a[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"{[", 2, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_z[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"]}", 2, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_z[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"]}", 2, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_q[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\"", 1, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_q[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\"", 1, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_b[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\\", 1, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_b[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\\", 1, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
#else
#error "Require AVX2_ENABLED or SSE42_ENABLED to be defined"
#endif
}

uint64_t bitwise_add(uint64_t a, uint64_t b, uint64_t *c) {
  uint64_t d = a + b + *c;

  *c = (d <= a) & 1;

  return d;
}

uint64_t process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint8_t *work_bits_of_d,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_a,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_z,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_q,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_b,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_e,       // Working buffer of minimum length ((in_length + 63) / 64)
    size_t *last_trailing_ones,
    size_t *quote_odds_carry,
    size_t *quote_evens_carry,
    uint64_t *quote_mask_carry,
    uint8_t *result_ib,
    uint8_t *result_a,
    uint8_t *result_z) {
  size_t m256_in_len = in_length / 32;
  size_t w64_out_len = in_length / 64;
  size_t w8_out_len  = in_length / 8;

  uint8_t  *w8_bits_of_b  = (uint8_t  *)work_bits_of_b;

  uint32_t *w32_bits_of_d = (uint32_t *)work_bits_of_d;
  uint32_t *w32_bits_of_a = (uint32_t *)work_bits_of_a;
  uint32_t *w32_bits_of_z = (uint32_t *)work_bits_of_z;
  uint32_t *w32_bits_of_q = (uint32_t *)work_bits_of_q;
  uint32_t *w32_bits_of_b = (uint32_t *)work_bits_of_b;

  uint64_t *w64_bits_of_d = (uint64_t *)work_bits_of_d;
  uint64_t *w64_bits_of_a = (uint64_t *)work_bits_of_a;
  uint64_t *w64_bits_of_z = (uint64_t *)work_bits_of_z;
  uint64_t *w64_bits_of_q = (uint64_t *)work_bits_of_q;
  uint64_t *w64_bits_of_e = (uint64_t *)work_bits_of_e;

  uint64_t *w64_result_ib = (uint64_t *)result_ib;
  uint64_t *w64_result_a  = (uint64_t *)result_a;
  uint64_t *w64_result_z  = (uint64_t *)result_z;

  uint64_t accum = 0;

  for (size_t i = 0; i < m256_in_len; ++i) {
    summarise(in_buffer + (i * 32),
      w32_bits_of_d + i,
      w32_bits_of_a + i,
      w32_bits_of_z + i,
      w32_bits_of_q + i,
      w32_bits_of_b + i);
  }

  for (size_t i = 0; i < w8_out_len; ++i) {
    char w8 = w8_bits_of_b[i];
    size_t j = (*last_trailing_ones) % 2;
    size_t k = (size_t)(uint8_t)w8;
    char w8e = escape_mask[j][k];
    work_bits_of_e[i] = w8e;
    *last_trailing_ones = _lzcnt_u64(~(int64_t)w8);
  }

  for (size_t i = 0; i < w64_out_len; ++i) {
    w64_bits_of_q[i] = w64_bits_of_e[i] & w64_bits_of_q[i];

    uint64_t w64_bits_of_q_word = w64_bits_of_q[i];

    uint64_t qas = _pdep_u64(0x5555555555555555 << ((*quote_odds_carry ) & 1), w64_bits_of_q_word);
    uint64_t qzs = _pdep_u64(0x5555555555555555 << ((*quote_evens_carry) & 1), w64_bits_of_q_word);

    uint64_t quote_mask = bitwise_add(qas, ~qzs, quote_mask_carry);

    uint64_t w64_d = quote_mask & w64_bits_of_d[i];
    uint64_t w64_a = quote_mask & w64_bits_of_a[i];
    uint64_t w64_z = quote_mask & w64_bits_of_z[i];

    w64_result_ib[i]  = w64_d | w64_a | w64_z;
    w64_result_a[i]   = w64_a;
    w64_result_z[i]   = w64_z;

    size_t pc = __builtin_popcountll(w64_bits_of_q[i]);
    *quote_odds_carry  += pc;
    *quote_evens_carry += pc;
  }

  return accum;
}
