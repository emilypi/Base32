/* Copyright (c) 2026 Emily Pillmore. BSD-3-Clause; see LICENSE. */
#include "base32_simd.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__unix__) || defined(__APPLE__)
#include <sys/mman.h>
#include <unistd.h>
#endif

static const char *alphabets[2] = {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "0123456789ABCDEFGHIJKLMNOPQRSTUV"
};

static void reference(const uint8_t *src, size_t len, uint8_t *dst, int hex)
{
    unsigned bits = 0, buffer = 0;
    size_t out = 0;
    for (size_t i = 0; i < len; ++i) {
        buffer = (buffer << 8) | src[i];
        bits += 8;
        while (bits >= 5) {
            bits -= 5;
            dst[out++] = (uint8_t)alphabets[hex][(buffer >> bits) & 31];
        }
    }
    if (bits) {
        dst[out] = (uint8_t)alphabets[hex][(buffer << (5 - bits)) & 31];
    }
}

static void boundaries(int hex)
{
    for (size_t len = 0; len <= 1024; ++len) {
        for (size_t offset = 0; offset < 16; ++offset) {
            size_t capacity = (len / 5) * 8;
            uint8_t *allocation = malloc(len + offset + (len == 0));
            uint8_t *output = malloc(capacity + offset + 1);
            uint8_t *expected = malloc((len * 8 + 4) / 5 + 1);
            assert(allocation && output && expected);
            uint8_t *src = allocation + offset;
            uint8_t *dst = output + offset;
            for (size_t i = 0; i < len; ++i) {
                src[i] = (uint8_t)(i * 131 + len * 17);
            }
            memset(output, 0xa5, capacity + offset + 1);
            reference(src, len, expected, hex);
            size_t read = hs_base32_simd_encode(src, len, dst, hex);
            assert(read <= len && read % 5 == 0);
            if (hs_base32_simd_backend() && len >= 20) {
                assert(read != 0);
            }
            size_t written = read / 5 * 8;
            assert(memcmp(dst, expected, written) == 0);
            for (size_t i = written; i <= capacity; ++i) {
                assert(dst[i] == 0xa5);
            }
            for (size_t i = 0; i < offset; ++i) {
                assert(output[i] == 0xa5);
            }
            /* Decode from an allocation that ends exactly at the input end. */
            uint8_t *encoded = malloc(written + offset + (written == 0));
            uint8_t *decoded = malloc(read + offset + 1);
            assert(encoded && decoded);
            memcpy(encoded + offset, expected, written);
            memset(decoded, 0xa5, read + offset + 1);
            size_t used = hs_base32_simd_decode(encoded + offset, written,
                                               decoded + offset, hex);
            assert(used <= written && used % 8 == 0);
            assert(used == (hs_base32_simd_backend() ? written : 0));
            assert(memcmp(decoded + offset, src, used / 8 * 5) == 0);
            for (size_t i = used / 8 * 5; i <= read; ++i) {
                assert(decoded[offset + i] == 0xa5);
            }
            for (size_t i = 0; i < offset; ++i) {
                assert(decoded[i] == 0xa5);
            }
            free(encoded);
            free(decoded);
            free(allocation);
            free(output);
            free(expected);
        }
    }
}

static void malformed(int hex)
{
    uint8_t payload[80], encoded[128], changed[128], decoded[80];
    for (size_t i = 0; i < sizeof payload; ++i) {
        payload[i] = (uint8_t)(i * 73);
    }
    reference(payload, sizeof payload, encoded, hex);
    for (size_t pos = 0; pos < sizeof encoded; ++pos) {
        for (unsigned byte = 0; byte < 256; ++byte) {
            unsigned upper = byte & ~32U;
            int valid = (upper >= 'A' && upper <= (hex ? 'V' : 'Z')) ||
                (byte >= (hex ? '0' : '2') && byte <= (hex ? '9' : '7'));
            if (valid) {
                continue;
            }
            memcpy(changed, encoded, sizeof changed);
            changed[pos] = (uint8_t)byte;
            memset(decoded, 0xa5, sizeof decoded);
            size_t used = hs_base32_simd_decode(changed, sizeof changed, decoded, hex);
            assert(used <= pos && used % 8 == 0);
            assert(memcmp(decoded, payload, used / 8 * 5) == 0);
            for (size_t i = used / 8 * 5; i < sizeof decoded; ++i) {
                assert(decoded[i] == 0xa5);
            }
        }
    }
    for (size_t i = 0; i < sizeof encoded; ++i) {
        if (encoded[i] >= 'A' && encoded[i] <= 'Z') {
            encoded[i] |= 32;
        }
    }
    size_t used = hs_base32_simd_decode(encoded, sizeof encoded, decoded, hex);
    assert(used == (hs_base32_simd_backend() ? sizeof encoded : 0));
    assert(memcmp(decoded, payload, used / 8 * 5) == 0);
}

static void guarded(int hex)
{
#if defined(__unix__) || defined(__APPLE__)
    size_t page = (size_t)sysconf(_SC_PAGESIZE);
    uint8_t *src = mmap(NULL, page * 2, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANON, -1, 0);
    uint8_t *dst = mmap(NULL, page * 2, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANON, -1, 0);
    assert(src != MAP_FAILED && dst != MAP_FAILED);
    assert(mprotect(src + page, page, PROT_NONE) == 0);
    assert(mprotect(dst + page, page, PROT_NONE) == 0);
    for (size_t len = 0; len <= 160; ++len) {
        size_t capacity = len / 5 * 8;
        memset(src + page - len, 42, len);
        hs_base32_simd_encode(src + page - len, len, dst + page - capacity, hex);
        capacity = len / 8 * 5;
        memset(src + page - len, hex ? '0' : 'A', len);
        hs_base32_simd_decode(src + page - len, len, dst + page - capacity, hex);
    }
    assert(munmap(src, page * 2) == 0);
    assert(munmap(dst, page * 2) == 0);
#else
    (void)hex;
#endif
}

int main(int argc, char **argv)
{
    if (argc == 2) {
        assert(hs_base32_simd_backend() == atoi(argv[1]));
    } else {
        assert(argc == 1);
    }
    for (int hex = 0; hex < 2; ++hex) {
        boundaries(hex);
        malformed(hex);
        guarded(hex);
    }
    printf("Native Base32 tests passed (backend %d)\n", hs_base32_simd_backend());
    return 0;
}
