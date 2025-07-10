/********************************************************************************
 * sfc64_ctx.h - Thread-safe Small Fast Counting RNG, 64-bit variant 
 * Original algorithm by Chris Doty-Humphrey, public-domain implementation
 * 
 * Author: Mounir IDRASSI <mounir.idrassi@amcrypto.jp>
 * Date: 2025-07-10
 * License: Public Domain (CC0)
 * 
 * This file is part of PrattScript source code.
 * 
 ********************************************************************************/

#ifndef SFC64_CTX_H_
#define SFC64_CTX_H_

#include <stdint.h>

/* MSVC: use the intrinsic in <intrin.h> */
#if defined(_MSC_VER)
  #include <intrin.h>
  #pragma intrinsic(_rotl64)
  #define rotl64(x, k) _rotl64(x, k)

/* Clang/GCC (since ~4.8) support __builtin_rotateleft64 / __builtin_rotl64? */
#elif defined(__clang__) || defined(__GNUC__)
  /* __has_builtin is a Clang extension, so guard it */
  #if defined(__has_builtin)
    #if __has_builtin(__builtin_rotateleft64)
      #define rotl64(x, k) __builtin_rotateleft64(x, k)
    #elif __has_builtin(__builtin_rotl64)
      #define rotl64(x, k) __builtin_rotl64(x, k);
      }
    #else
      /* fallback if neither builtin is detected */
      static inline uint64_t rotl64(uint64_t x, unsigned k) {
        return (x << k) | (x >> (64 - k));
      }
    #endif
  #else
    /* no __has_builtin: assume C compiler will optimize the shift/or into a ROL */
    static inline uint64_t rotl64(uint64_t x, unsigned k) {
      return (x << k) | (x >> (64 - k));
    }
  #endif

#else
  /* any other compiler: portable fallback */
  static inline uint64_t rotl64(uint64_t x, unsigned k) {
    return (x << k) | (x >> (64 - k));
  }
#endif

typedef struct {
    uint64_t a, b, c, d;
} SFC64Context;

/* SplitMix64 – expands a 64-bit seed into well-scrambled words */
static uint64_t splitmix64(uint64_t *x) {
    uint64_t z = (*x += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

/**
 * Initialize the context from a single 64-bit seed.
 * Performs the mandatory 12-round burn-in.
 */
static void sfc64_init(SFC64Context *ctx, uint64_t seed) {
    ctx->a = splitmix64(&seed);
    ctx->b = splitmix64(&seed);
    ctx->c = splitmix64(&seed);
    ctx->d = 1;  /* counter must be non-zero */
    for (int i = 0; i < 12; ++i) {
        /* call sfc64_next inline below */
        uint64_t tmp = ctx->a + ctx->b + ctx->d++;
        ctx->a = ctx->b ^ (ctx->b >> 11);
        ctx->b = ctx->c + (ctx->c << 3);
        ctx->c = rotl64(ctx->c, 24) + tmp;
    }
}

/**
 * Generate the next 64-bit output.
 */
static uint64_t sfc64_next(SFC64Context *ctx) {
    uint64_t tmp = ctx->a + ctx->b + ctx->d++;
    ctx->a = ctx->b ^ (ctx->b >> 11);
    ctx->b = ctx->c + (ctx->c << 3);
    ctx->c = rotl64(ctx->c, 24) + tmp;
    return tmp;
}

/**
 * Generate a double in [0,1), using 53 random bits.
 */
static double sfc64_double(SFC64Context *ctx) {
    return (sfc64_next(ctx) >> 11) * (1.0 / 9007199254740992.0);
}

#endif /* SFC64_CTX_H_ */
