#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <immintrin.h>
#include <mmintrin.h>

#include "debug.h"

void print256_num(__m256i var) {
  fprint256_num(stdout, var);
}

void fprint256_num(FILE *file, __m256i var) {
  uint8_t *val = (uint8_t*)&var;
  fprintf(file,
    "%02x %02x %02x %02x %02x %02x %02x %02x  %02x %02x %02x %02x %02x %02x %02x %02x  "
    "%02x %02x %02x %02x %02x %02x %02x %02x  %02x %02x %02x %02x %02x %02x %02x %02x"
    , val[ 0], val[ 1], val[ 2], val[ 3], val[ 4], val[ 5], val[ 6], val[ 7]
    , val[ 8], val[ 9], val[10], val[11], val[12], val[13], val[14], val[15]
    , val[16], val[17], val[18], val[19], val[20], val[21], val[22], val[23]
    , val[24], val[25], val[26], val[27], val[28], val[29], val[30], val[31]);
}

void print128_num(__m128i var) {
  fprint128_num(stdout, var);
}

void fprint128_num(FILE *file, __m128i var) {
  uint8_t *val = (uint8_t*)&var;
  fprintf(file,
    "%02x %02x %02x %02x %02x %02x %02x %02x  %02x %02x %02x %02x %02x %02x %02x %02x"
    , val[ 0], val[ 1], val[ 2], val[ 3], val[ 4], val[ 5], val[ 6], val[ 7]
    , val[ 8], val[ 9], val[10], val[11], val[12], val[13], val[14], val[15]);
}
