/* Copyright (c) 2026 Emily Pillmore. BSD-3-Clause; see LICENSE. */
#ifndef HS_BASE32_SIMD_H
#define HS_BASE32_SIMD_H
#include <stddef.h>
#include <stdint.h>
/* Backends: 0 scalar fallback, 1 SSSE3, 2 NEON, 3 AVX2. */
int hs_base32_simd_backend(void);
/* Return consumed input bytes. No tail, padding, or invalid block is written.
 * Source and destination must be distinct, nonoverlapping buffers; destination
 * capacities are len/5*8 (encode) and len/8*5 (decode). hex is 0 or 1. */
size_t hs_base32_simd_encode(const uint8_t *, size_t, uint8_t *, int hex);
size_t hs_base32_simd_decode(const uint8_t *, size_t, uint8_t *, int hex);
/* Known-valid symbols only, with trailing padding excluded. No validation. */
size_t hs_base32_simd_decode_typed(const uint8_t *, size_t, uint8_t *, int hex);
#endif
