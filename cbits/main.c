#include "simd.h"

#include <stdio.h>
#include <string.h>
#include <immintrin.h>

int sm_main(
    int argc,
    char **argv);

int main(
    int argc,
    char **argv) {
  if (argc <= 1) {
    fprintf(stderr, "Require command\n");
    exit(1);
  }
  
  if (strcmp(argv[1], "sp") == 0) {
    main_spliced(argc - 1, argv + 1);
  } else if (strcmp(argv[1], "sm") == 0) {
    sm_main(argc - 1, argv + 1);
  } else {
    fprintf(stderr, "Unrecognised command: %s\n", argv[1]);
    exit(1);
  }

  return 0;
}

int sm_main(
    int argc,
    char **argv) {
  if (argc != 6) {
    fprintf(stderr, "./a.out <input-file> <output-ib-file> <output-bp-file> <output-op-file> <output-cl-file>\n");
    exit(1);
  }

  char *in_filename     = argv[1];
  char *ib_out_filename = argv[2];
  char *bp_out_filename = argv[3];
  char *op_out_filename = argv[4];
  char *cl_out_filename = argv[5];

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

  FILE *op_out = fopen(op_out_filename, "w");

  if (!op_out) {
    fprintf(stderr, "Failed to open op output file %s\n", op_out_filename);
    exit(1);
  }

  FILE *cl_out = fopen(cl_out_filename, "w");

  if (!cl_out) {
    fprintf(stderr, "Failed to open cl output file %s\n", cl_out_filename);
    exit(1);
  }

  uint8_t buffer[W8_BUFFER_SIZE];
  uint32_t phi_buffer[W8_BUFFER_SIZE];

  uint8_t ibs_buffer[W8_BUFFER_SIZE];
  uint8_t ops_buffer[W8_BUFFER_SIZE];
  uint8_t cls_buffer[W8_BUFFER_SIZE];

  // uint32_t result_ib[W8_BUFFER_SIZE];
  // uint32_t result_a [W8_BUFFER_SIZE];
  // uint32_t result_z [W8_BUFFER_SIZE];
  // uint64_t accum = 0;
  
  uint64_t remaining_bp_bits = 0;
  size_t remaining_bp_bits_len = 0;

  uint64_t out_bp_buffer[W32_BUFFER_SIZE * 2];

  size_t total_bytes_read = 0;
  uint32_t state = 0x03020100;

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

    uint32_t chunk_state = state;

    sm_process_chunk(buffer, bytes_read,
      &state,
      phi_buffer);

    sm_make_ib_bp_chunks(chunk_state, phi_buffer, bytes_read,
      ibs_buffer,
      ops_buffer,
      cls_buffer);

    size_t idx_bytes = (bytes_read + 7) / 8;

    fwrite(ibs_buffer, 1, idx_bytes, ib_out);

    size_t out_bp_bytes = sm_write_bp_chunk(
      ops_buffer,
      cls_buffer,
      idx_bytes,
      &remaining_bp_bits,
      &remaining_bp_bits_len,
      out_bp_buffer);

    fwrite(out_bp_buffer, out_bp_bytes, sizeof(uint64_t), bp_out);

    fflush(ib_out);
    fflush(bp_out);
  }

  sm_write_bp_chunk_final(remaining_bp_bits, remaining_bp_bits_len, out_bp_buffer);

  fprintf(stderr, "Final state %u\n", state);

  fwrite(out_bp_buffer, 2, sizeof(uint64_t), bp_out);

  fclose(in);
  fclose(ib_out);

  return 0;
}
