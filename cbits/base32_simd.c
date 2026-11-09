/* Copyright (c) 2026 Emily Pillmore. BSD-3-Clause; see LICENSE. */
#include "base32_simd.h"
#include <string.h>

/* Each lane encodes ten bytes or decodes sixteen. All loads/stores are
 * bounded; invalid decode blocks are not committed. Target attributes keep
 * optional x86 instructions out of the baseline dispatcher. Force macros
 * only cap backend selection; they never bypass hardware/OS checks. */
#if !defined(HS_BASE32_SIMD_FORCE_SCALAR)
#if defined(__aarch64__) && defined(__ARM_NEON)
#define HS_BASE32_NEON
#include <arm_neon.h>
#elif defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
#define HS_BASE32_X86
#include <immintrin.h>
#endif
#endif

#if defined(HS_BASE32_NEON) || defined(HS_BASE32_X86)
static const uint8_t enc_a[16] = {
    0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9
};
static const uint8_t enc_b[16] = {
    255, 1, 255, 2, 3, 255, 4, 255, 255, 6, 255, 7, 8, 255, 9, 255
};
static const uint8_t dec_a[16] = {
    0, 1, 3, 4, 6, 8, 9, 11, 12, 14, 255, 255, 255, 255, 255, 255
};
static const uint8_t dec_b[16] = {
    1, 2, 4, 5, 7, 9, 10, 12, 13, 15, 255, 255, 255, 255, 255, 255
};
static const uint8_t dec_c[16] = {
    255, 3, 255, 6, 255, 255, 11, 255, 14, 255, 255, 255, 255, 255, 255, 255
};
#endif

#if defined(HS_BASE32_NEON)

static uint8x16_t load10(const uint8_t *src)
{
    uint8_t bytes[16] = {0};
    memcpy(bytes, src, 10);
    return vld1q_u8(bytes);
}

static void store10(uint8_t *dst, uint8x16_t value)
{
    uint8_t bytes[16];
    vst1q_u8(bytes, value);
    memcpy(dst, bytes, 10);
}

static size_t neon_encode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    static const int8_t shifts_a[16] = {
        -3,2,-1,4,1,-2,3,0, -3,2,-1,4,1,-2,3,0
    };
    static const int8_t shifts_b[16] = {
        0,-6,0,-4,-7,0,-5,0, 0,-6,0,-4,-7,0,-5,0
    };
    size_t done = 0;
    while (len - done >= 10) {
        const uint8x16_t input = load10(src + done);
        const uint8x16_t a = vqtbl1q_u8(input, vld1q_u8(enc_a));
        const uint8x16_t b = vqtbl1q_u8(input, vld1q_u8(enc_b));
        const uint8x16_t index = vandq_u8(vdupq_n_u8(31), vorrq_u8(
            vshlq_u8(a, vld1q_s8(shifts_a)), vshlq_u8(b, vld1q_s8(shifts_b))));
        const uint8x16_t high = vcgtq_u8(index, vdupq_n_u8(hex ? 9 : 25));
        const uint8x16_t offset = vbslq_u8(high,
            vdupq_n_u8(hex ? 55 : 24), vdupq_n_u8(hex ? 48 : 65));
        vst1q_u8(dst, vaddq_u8(index, offset));
        dst += 16;
        done += 10;
    }
    return done;
}

static size_t neon_decode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    static const int8_t shifts_a[16] = {3,6,4,7,5,3,6,4,7,5,0,0,0,0,0,0};
    static const int8_t shifts_b[16] = {-2,1,-1,2,0,-2,1,-1,2,0,0,0,0,0,0,0};
    static const int8_t shifts_c[16] = {0,-4,0,-3,0,0,-4,0,-3,0,0,0,0,0,0,0};
    size_t done = 0;
    while (len - done >= 16) {
        const uint8x16_t input = vld1q_u8(src + done);
        const uint8x16_t upper = vandq_u8(input, vdupq_n_u8(0xdf));
        const uint8x16_t letter = vsubq_u8(upper, vdupq_n_u8('A'));
        const uint8x16_t digit = vsubq_u8(input, vdupq_n_u8(hex ? '0' : '2'));
        const uint8x16_t is_letter = vcleq_u8(letter, vdupq_n_u8(hex ? 21 : 25));
        const uint8x16_t is_digit = vcleq_u8(digit, vdupq_n_u8(hex ? 9 : 5));
        if (vminvq_u8(vorrq_u8(is_letter, is_digit)) == 0) {
            break;
        }
        const uint8x16_t value = vbslq_u8(is_letter,
            vaddq_u8(letter, vdupq_n_u8(hex ? 10 : 0)),
            vaddq_u8(digit, vdupq_n_u8(hex ? 0 : 26)));
        const uint8x16_t a = vqtbl1q_u8(value, vld1q_u8(dec_a));
        const uint8x16_t b = vqtbl1q_u8(value, vld1q_u8(dec_b));
        const uint8x16_t c = vqtbl1q_u8(value, vld1q_u8(dec_c));
        store10(dst, vorrq_u8(vshlq_u8(a, vld1q_s8(shifts_a)),
            vorrq_u8(vshlq_u8(b, vld1q_s8(shifts_b)),
                     vshlq_u8(c, vld1q_s8(shifts_c)))));
        dst += 10;
        done += 16;
    }
    return done;
}
#elif defined(HS_BASE32_X86)

__attribute__((target("ssse3")))
static __m128i ssse3_mask(const uint8_t *bytes)
{
    const __m128i value = _mm_loadu_si128((const __m128i *)bytes);
    return value;
}

__attribute__((target("ssse3")))
static size_t ssse3_encode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    size_t done = 0;
    while (len - done >= 10) {
        uint8_t bytes[16] = {0};
        memcpy(bytes + 0, src + done + 0, 10);
        const __m128i value = _mm_loadu_si128((const __m128i *)bytes);
        __m128i output = _mm_setzero_si128();
        const __m128i a = _mm_shuffle_epi8(value, ssse3_mask(enc_a));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(a, 3),
            ssse3_mask((const uint8_t[16]){31,0,0,0,0,0,0,0,31,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(a, 2),
            ssse3_mask((const uint8_t[16]){0,0,0,0,0,31,0,0,0,0,0,0,0,31,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(a, 1),
            ssse3_mask((const uint8_t[16]){0,0,31,0,0,0,0,0,0,0,31,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            a,
            ssse3_mask((const uint8_t[16]){0,0,0,0,0,0,0,31,0,0,0,0,0,0,0,31})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 1),
            ssse3_mask((const uint8_t[16]){0,0,0,0,30,0,0,0,0,0,0,0,30,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 2),
            ssse3_mask((const uint8_t[16]){0,28,0,0,0,0,0,0,0,28,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 3),
            ssse3_mask((const uint8_t[16]){0,0,0,0,0,0,24,0,0,0,0,0,0,0,24,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 4),
            ssse3_mask((const uint8_t[16]){0,0,0,16,0,0,0,0,0,0,0,16,0,0,0,0})));
        const __m128i b = _mm_shuffle_epi8(value, ssse3_mask(enc_b));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(b, 7),
            ssse3_mask((const uint8_t[16]){0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(b, 6),
            ssse3_mask((const uint8_t[16]){0,3,0,0,0,0,0,0,0,3,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(b, 5),
            ssse3_mask((const uint8_t[16]){0,0,0,0,0,0,7,0,0,0,0,0,0,0,7,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(b, 4),
            ssse3_mask((const uint8_t[16]){0,0,0,15,0,0,0,0,0,0,0,15,0,0,0,0})));
        const __m128i high = _mm_cmpgt_epi8(output, _mm_set1_epi8(hex ? 9 : 25));
        const __m128i offset = _mm_or_si128(
            _mm_and_si128(high, _mm_set1_epi8(hex ? 55 : 24)),
            _mm_andnot_si128(high, _mm_set1_epi8(hex ? 48 : 65)));
        _mm_storeu_si128((__m128i *)dst, _mm_add_epi8(output, offset));
        dst += 16;
        done += 10;
    }
    return done;
}

__attribute__((target("ssse3")))
static size_t ssse3_decode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    size_t done = 0;
    while (len - done >= 16) {
        const __m128i input = _mm_loadu_si128((const __m128i *)(src + done));
        const __m128i upper = _mm_and_si128(input, _mm_set1_epi8((char)0xdf));
        const __m128i letters = _mm_and_si128(
            _mm_cmpgt_epi8(upper, _mm_set1_epi8('A' - 1)),
            _mm_cmpgt_epi8(_mm_set1_epi8(hex ? 'V' + 1 : 'Z' + 1), upper));
        const __m128i digits = _mm_and_si128(
            _mm_cmpgt_epi8(input, _mm_set1_epi8(hex ? '0' - 1 : '2' - 1)),
            _mm_cmpgt_epi8(_mm_set1_epi8(hex ? '9' + 1 : '7' + 1), input));
        if ((unsigned)_mm_movemask_epi8(_mm_or_si128(letters, digits))
            != 0xffffU) {
            break;
        }
        const __m128i value = _mm_or_si128(
            _mm_and_si128(letters,
                _mm_add_epi8(upper, _mm_set1_epi8(hex ? 10 - 'A' : -'A'))),
            _mm_and_si128(digits,
                _mm_add_epi8(input, _mm_set1_epi8(hex ? -'0' : 26 - '2'))));
        __m128i output = _mm_setzero_si128();
        const __m128i a = _mm_shuffle_epi8(value, ssse3_mask(dec_a));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 3),
            ssse3_mask((const uint8_t[16]){248,0,0,0,0,248,0,0,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 4),
            ssse3_mask((const uint8_t[16]){0,0,240,0,0,0,0,240,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 5),
            ssse3_mask((const uint8_t[16]){0,0,0,0,224,0,0,0,0,224,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 6),
            ssse3_mask((const uint8_t[16]){0,192,0,0,0,0,192,0,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(a, 7),
            ssse3_mask((const uint8_t[16]){0,0,0,128,0,0,0,0,128,0,0,0,0,0,0,0})));
        const __m128i b = _mm_shuffle_epi8(value, ssse3_mask(dec_b));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(b, 2),
            ssse3_mask((const uint8_t[16]){7,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(b, 1),
            ssse3_mask((const uint8_t[16]){0,0,15,0,0,0,0,15,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            b,
            ssse3_mask((const uint8_t[16]){0,0,0,0,31,0,0,0,0,31,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(b, 1),
            ssse3_mask((const uint8_t[16]){0,62,0,0,0,0,62,0,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_slli_epi16(b, 2),
            ssse3_mask((const uint8_t[16]){0,0,0,124,0,0,0,0,124,0,0,0,0,0,0,0})));
        const __m128i c = _mm_shuffle_epi8(value, ssse3_mask(dec_c));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(c, 4),
            ssse3_mask((const uint8_t[16]){0,15,0,0,0,0,15,0,0,0,0,0,0,0,0,0})));
        output = _mm_or_si128(output, _mm_and_si128(
            _mm_srli_epi16(c, 3),
            ssse3_mask((const uint8_t[16]){0,0,0,7,0,0,0,0,7,0,0,0,0,0,0,0})));
        uint8_t bytes[16];
        _mm_storeu_si128((__m128i *)bytes, output);
        memcpy(dst + 0, bytes + 0, 10);
        dst += 10;
        done += 16;
    }
    return done;
}

__attribute__((target("avx2")))
static __m256i avx2_mask(const uint8_t *bytes)
{
    const __m128i value = _mm_loadu_si128((const __m128i *)bytes);
    return _mm256_broadcastsi128_si256(value);
}

__attribute__((target("avx2")))
static size_t avx2_encode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    size_t done = 0;
    while (len - done >= 20) {
        uint8_t bytes[32] = {0};
        memcpy(bytes + 0, src + done + 0, 10);
        memcpy(bytes + 16, src + done + 10, 10);
        const __m256i value = _mm256_loadu_si256((const __m256i *)bytes);
        __m256i output = _mm256_setzero_si256();
        const __m256i a = _mm256_shuffle_epi8(value, avx2_mask(enc_a));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(a, 3),
            avx2_mask((const uint8_t[16]){31,0,0,0,0,0,0,0,31,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(a, 2),
            avx2_mask((const uint8_t[16]){0,0,0,0,0,31,0,0,0,0,0,0,0,31,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(a, 1),
            avx2_mask((const uint8_t[16]){0,0,31,0,0,0,0,0,0,0,31,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            a,
            avx2_mask((const uint8_t[16]){0,0,0,0,0,0,0,31,0,0,0,0,0,0,0,31})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 1),
            avx2_mask((const uint8_t[16]){0,0,0,0,30,0,0,0,0,0,0,0,30,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 2),
            avx2_mask((const uint8_t[16]){0,28,0,0,0,0,0,0,0,28,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 3),
            avx2_mask((const uint8_t[16]){0,0,0,0,0,0,24,0,0,0,0,0,0,0,24,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 4),
            avx2_mask((const uint8_t[16]){0,0,0,16,0,0,0,0,0,0,0,16,0,0,0,0})));
        const __m256i b = _mm256_shuffle_epi8(value, avx2_mask(enc_b));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(b, 7),
            avx2_mask((const uint8_t[16]){0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(b, 6),
            avx2_mask((const uint8_t[16]){0,3,0,0,0,0,0,0,0,3,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(b, 5),
            avx2_mask((const uint8_t[16]){0,0,0,0,0,0,7,0,0,0,0,0,0,0,7,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(b, 4),
            avx2_mask((const uint8_t[16]){0,0,0,15,0,0,0,0,0,0,0,15,0,0,0,0})));
        const __m256i high = _mm256_cmpgt_epi8(output, _mm256_set1_epi8(hex ? 9 : 25));
        const __m256i offset = _mm256_or_si256(
            _mm256_and_si256(high, _mm256_set1_epi8(hex ? 55 : 24)),
            _mm256_andnot_si256(high, _mm256_set1_epi8(hex ? 48 : 65)));
        _mm256_storeu_si256((__m256i *)dst, _mm256_add_epi8(output, offset));
        dst += 32;
        done += 20;
    }
    return done;
}

__attribute__((target("avx2")))
static size_t avx2_decode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    size_t done = 0;
    while (len - done >= 32) {
        const __m256i input = _mm256_loadu_si256((const __m256i *)(src + done));
        const __m256i upper = _mm256_and_si256(input, _mm256_set1_epi8((char)0xdf));
        const __m256i letters = _mm256_and_si256(
            _mm256_cmpgt_epi8(upper, _mm256_set1_epi8('A' - 1)),
            _mm256_cmpgt_epi8(_mm256_set1_epi8(hex ? 'V' + 1 : 'Z' + 1), upper));
        const __m256i digits = _mm256_and_si256(
            _mm256_cmpgt_epi8(input, _mm256_set1_epi8(hex ? '0' - 1 : '2' - 1)),
            _mm256_cmpgt_epi8(_mm256_set1_epi8(hex ? '9' + 1 : '7' + 1), input));
        if ((unsigned)_mm256_movemask_epi8(_mm256_or_si256(letters, digits))
            != 0xffffffffU) {
            break;
        }
        const __m256i value = _mm256_or_si256(
            _mm256_and_si256(letters,
                _mm256_add_epi8(upper, _mm256_set1_epi8(hex ? 10 - 'A' : -'A'))),
            _mm256_and_si256(digits,
                _mm256_add_epi8(input, _mm256_set1_epi8(hex ? -'0' : 26 - '2'))));
        __m256i output = _mm256_setzero_si256();
        const __m256i a = _mm256_shuffle_epi8(value, avx2_mask(dec_a));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 3),
            avx2_mask((const uint8_t[16]){248,0,0,0,0,248,0,0,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 4),
            avx2_mask((const uint8_t[16]){0,0,240,0,0,0,0,240,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 5),
            avx2_mask((const uint8_t[16]){0,0,0,0,224,0,0,0,0,224,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 6),
            avx2_mask((const uint8_t[16]){0,192,0,0,0,0,192,0,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(a, 7),
            avx2_mask((const uint8_t[16]){0,0,0,128,0,0,0,0,128,0,0,0,0,0,0,0})));
        const __m256i b = _mm256_shuffle_epi8(value, avx2_mask(dec_b));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(b, 2),
            avx2_mask((const uint8_t[16]){7,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(b, 1),
            avx2_mask((const uint8_t[16]){0,0,15,0,0,0,0,15,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            b,
            avx2_mask((const uint8_t[16]){0,0,0,0,31,0,0,0,0,31,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(b, 1),
            avx2_mask((const uint8_t[16]){0,62,0,0,0,0,62,0,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_slli_epi16(b, 2),
            avx2_mask((const uint8_t[16]){0,0,0,124,0,0,0,0,124,0,0,0,0,0,0,0})));
        const __m256i c = _mm256_shuffle_epi8(value, avx2_mask(dec_c));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(c, 4),
            avx2_mask((const uint8_t[16]){0,15,0,0,0,0,15,0,0,0,0,0,0,0,0,0})));
        output = _mm256_or_si256(output, _mm256_and_si256(
            _mm256_srli_epi16(c, 3),
            avx2_mask((const uint8_t[16]){0,0,0,7,0,0,0,0,7,0,0,0,0,0,0,0})));
        uint8_t bytes[32];
        _mm256_storeu_si256((__m256i *)bytes, output);
        memcpy(dst + 0, bytes + 0, 10);
        memcpy(dst + 10, bytes + 16, 10);
        dst += 20;
        done += 32;
    }
    return done;
}

#endif

int hs_base32_simd_backend(void)
{
#if defined(HS_BASE32_NEON)
    return 2;
#elif defined(HS_BASE32_X86)
#if !defined(HS_BASE32_SIMD_FORCE_SSSE3)
    /* Compiler CPU queries include OS support for AVX register state. */
    if (__builtin_cpu_supports("avx2")) {
        return 3;
    }
#endif
    return __builtin_cpu_supports("ssse3") ? 1 : 0;
#else
    return 0;
#endif
}

size_t hs_base32_simd_encode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
#if defined(HS_BASE32_NEON)
    return neon_encode(src, len, dst, hex);
#elif defined(HS_BASE32_X86)
    const int backend = hs_base32_simd_backend();
    if (backend == 3) {
        return avx2_encode(src, len, dst, hex);
    }
    if (backend == 1) {
        return ssse3_encode(src, len, dst, hex);
    }
#endif
    (void)src;
    (void)len;
    (void)dst;
    (void)hex;
    return 0;
}

size_t hs_base32_simd_decode(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
#if defined(HS_BASE32_NEON)
    return neon_decode(src, len, dst, hex);
#elif defined(HS_BASE32_X86)
    const int backend = hs_base32_simd_backend();
    if (backend == 3) {
        return avx2_decode(src, len, dst, hex);
    }
    if (backend == 1) {
        return ssse3_decode(src, len, dst, hex);
    }
#endif
    (void)src;
    (void)len;
    (void)dst;
    (void)hex;
    return 0;
}
