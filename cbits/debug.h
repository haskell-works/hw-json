#ifdef AVX2_ENABLED
#include <immintrin.h>
#include <mmintrin.h>
#endif//AVX2_ENABLED

#include <stdint.h>
#include <stdio.h>

#ifdef AVX2_ENABLED
void print256_num(__m256i var);

void print128_num(__m128i var);

void fprint256_num(FILE *file, __m256i var);

void fprint128_num(FILE *file, __m128i var);
#endif//AVX2_ENABLED

inline void print_bits_8(uint8_t v) {
  char *digits = "01";

  for (int i = 0; i < 8; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void print_bits_16(uint16_t v) {
  char *digits = "01";

  for (int i = 0; i < 16; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void print_bits_32(uint32_t v) {
  char *digits = "01";

  for (int i = 0; i < 32; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void print_bits_64(uint64_t v) {
  char *digits = "01";

  for (int i = 0; i < 64; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void print_bits_128(__m128i v) {
  for (int i = 0; i < 2; ++i) {
    if (i > 0) {
      printf("-");
    }

    print_bits_64(_mm_extract_epi64(v, i));
  }
}

inline void print_bits_256(__m256i v) {
  for (int i = 0; i < 4; ++i) {
    if (i > 0) {
      printf("-");
    }

    print_bits_64(_mm256_extract_epi64(v, i));
  }
}
