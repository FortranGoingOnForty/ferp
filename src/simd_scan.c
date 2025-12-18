/*
 * SIMD character scanning for FERP
 * Uses ARM NEON on Apple Silicon, scalar fallback otherwise
 */

#include <stdint.h>
#include <stddef.h>

#if defined(__ARM_NEON) || defined(__ARM_NEON__)
#include <arm_neon.h>
#define USE_NEON 1
#else
#define USE_NEON 0
#endif

/*
 * Find first occurrence of character 'needle' in buffer starting at 'start'.
 * Returns position (0-indexed) or -1 if not found.
 * Scans in 16-byte chunks using SIMD when available.
 */
int64_t simd_find_char(const char *buf, int64_t len, int64_t start, char needle) {
    if (start >= len) return -1;

    const char *p = buf + start;
    int64_t pos = start;

#if USE_NEON
    /* Use NEON for bulk scanning */
    uint8x16_t vneedle = vdupq_n_u8((uint8_t)needle);

    /* Align to 16-byte boundary */
    while (pos < len && ((uintptr_t)p & 15)) {
        if (*p == needle) return pos;
        p++; pos++;
    }

    /* SIMD scan 16 bytes at a time */
    while (pos + 16 <= len) {
        uint8x16_t chunk = vld1q_u8((const uint8_t *)p);
        uint8x16_t cmp = vceqq_u8(chunk, vneedle);

        /* Check if any byte matched */
        if (vmaxvq_u8(cmp)) {
            /* Find which byte matched */
            for (int i = 0; i < 16; i++) {
                if (p[i] == needle) return pos + i;
            }
        }
        p += 16;
        pos += 16;
    }
#endif

    /* Scalar fallback for remainder */
    while (pos < len) {
        if (*p == needle) return pos;
        p++; pos++;
    }

    return -1;
}

/*
 * Find first occurrence of 2-character sequence in buffer.
 * Returns position (0-indexed) or -1 if not found.
 */
int64_t simd_find_char2(const char *buf, int64_t len, int64_t start, char c1, char c2) {
    if (start >= len - 1) return -1;

    int64_t pos = start;

#if USE_NEON
    uint8x16_t vc1 = vdupq_n_u8((uint8_t)c1);
    const char *p = buf + start;

    /* Align to 16-byte boundary */
    while (pos < len - 1 && ((uintptr_t)p & 15)) {
        if (p[0] == c1 && p[1] == c2) return pos;
        p++; pos++;
    }

    /* SIMD scan for first character */
    while (pos + 16 <= len - 1) {
        uint8x16_t chunk = vld1q_u8((const uint8_t *)p);
        uint8x16_t cmp = vceqq_u8(chunk, vc1);

        if (vmaxvq_u8(cmp)) {
            /* Check each potential match */
            for (int i = 0; i < 16 && pos + i < len - 1; i++) {
                if (p[i] == c1 && p[i + 1] == c2) return pos + i;
            }
        }
        p += 16;
        pos += 16;
    }
#endif

    /* Scalar fallback */
    const char *p2 = buf + pos;
    while (pos < len - 1) {
        if (p2[0] == c1 && p2[1] == c2) return pos;
        p2++; pos++;
    }

    return -1;
}

/*
 * Count occurrences of character in buffer (useful for line counting)
 */
int64_t simd_count_char(const char *buf, int64_t len, char needle) {
    int64_t count = 0;
    const char *p = buf;
    const char *end = buf + len;

#if USE_NEON
    uint8x16_t vneedle = vdupq_n_u8((uint8_t)needle);
    uint8x16_t vcount = vdupq_n_u8(0);
    int batch = 0;

    /* Align */
    while (p < end && ((uintptr_t)p & 15)) {
        if (*p++ == needle) count++;
    }

    /* SIMD count */
    while (p + 16 <= end) {
        uint8x16_t chunk = vld1q_u8((const uint8_t *)p);
        uint8x16_t cmp = vceqq_u8(chunk, vneedle);
        /* -1 for match, 0 for no match; negate to get 1/0 */
        vcount = vsubq_u8(vcount, cmp);
        p += 16;
        batch++;

        /* Prevent overflow - accumulate every 255 iterations */
        if (batch == 255) {
            count += vaddvq_u8(vcount);
            vcount = vdupq_n_u8(0);
            batch = 0;
        }
    }
    count += vaddvq_u8(vcount);
#endif

    /* Scalar remainder */
    while (p < end) {
        if (*p++ == needle) count++;
    }

    return count;
}
