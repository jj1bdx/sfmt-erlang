/* sfmt_nif.c: C NIF SFMT API functions for Erlang */
/* based on SFMT-1.3.3 and sfmt-extstate 0.1.0_RELEASE */
/* 
   Copyright (c) 2010 Kenji Rikitake. All rights reserved.

   Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
   University. All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

   * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.
   * Neither the name of the Hiroshima University nor the names of
   its contributors may be used to endorse or promote products
   derived from this software without specific prior written
   permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "sfmt_nif.h"

/* sfmt-extstate code */

#if defined(HAVE_SSE2)

/* SSE2 assembly language code */

/**
 * This function represents the recursion formula.
 * @param a a 128-bit part of the interal state array
 * @param b a 128-bit part of the interal state array
 * @param c a 128-bit part of the interal state array
 * @param d a 128-bit part of the interal state array
 * @param mask 128-bit mask
 * @return output
 */
static PRE_ALWAYS __m128i mm_recursion(__m128i *a, __m128i *b, 
				   __m128i c, __m128i d, __m128i mask) {
    __m128i v, x, y, z;
    
    x = _mm_load_si128(a);
    y = _mm_srli_epi32(*b, SR1);
    z = _mm_srli_si128(c, SR2);
    v = _mm_slli_epi32(d, SL1);
    z = _mm_xor_si128(z, x);
    z = _mm_xor_si128(z, v);
    x = _mm_slli_si128(x, SL2);
    y = _mm_and_si128(y, mask);
    z = _mm_xor_si128(z, x);
    z = _mm_xor_si128(z, y);
    return z;
}

/**
 * This function fills the internal state array with pseudorandom
 * integers.
 * @param intstate internal state array
 */
static inline void gen_rand_all(w128_t *intstate) {
    int i;
    __m128i r, r1, r2, mask;
    mask = _mm_set_epi32(MSK4, MSK3, MSK2, MSK1);

    r1 = _mm_load_si128(&intstate[N - 2].si);
    r2 = _mm_load_si128(&intstate[N - 1].si);
    for (i = 0; i < N - POS1; i++) {
	r = mm_recursion(&intstate[i].si, &intstate[i + POS1].si, r1, r2, mask);
	_mm_store_si128(&intstate[i].si, r);
	r1 = r2;
	r2 = r;
    }
    for (; i < N; i++) {
	r = mm_recursion(&intstate[i].si, &intstate[i + POS1 - N].si, r1, r2, mask);
	_mm_store_si128(&intstate[i].si, r);
	r1 = r2;
	r2 = r;
    }
}

/**
 * This function fills the user-specified array with pseudorandom
 * integers.
 *
 * @param array an 128-bit array to be filled by pseudorandom numbers.  
 * @param size number of 128-bit pesudorandom numbers to be generated.
 * @param intstate internal state array
 */
static inline void gen_rand_array(w128_t *array, int size, w128_t *intstate) {
    int i, j;
    __m128i r, r1, r2, mask;
    mask = _mm_set_epi32(MSK4, MSK3, MSK2, MSK1);

    r1 = _mm_load_si128(&intstate[N - 2].si);
    r2 = _mm_load_si128(&intstate[N - 1].si);
    for (i = 0; i < N - POS1; i++) {
	r = mm_recursion(&intstate[i].si, &intstate[i + POS1].si, r1, r2, mask);
	_mm_store_si128(&array[i].si, r);
	r1 = r2;
	r2 = r;
    }
    for (; i < N; i++) {
	r = mm_recursion(&intstate[i].si, &array[i + POS1 - N].si, r1, r2, mask);
	_mm_store_si128(&array[i].si, r);
	r1 = r2;
	r2 = r;
    }
    /* main loop */
    for (; i < size - N; i++) {
	r = mm_recursion(&array[i - N].si, &array[i + POS1 - N].si, r1, r2,
			 mask);
	_mm_store_si128(&array[i].si, r);
	r1 = r2;
	r2 = r;
    }
    for (j = 0; j < 2 * N - size; j++) {
	r = _mm_load_si128(&array[j + size - N].si);
	_mm_store_si128(&intstate[j].si, r);
    }
    for (; i < size; i++) {
	r = mm_recursion(&array[i - N].si, &array[i + POS1 - N].si, r1, r2,
			 mask);
	_mm_store_si128(&array[i].si, r);
	_mm_store_si128(&intstate[j++].si, r);
	r1 = r2;
	r2 = r;
    }
}

#else /* !defined(HAVE_SSE2) */

/**
 * This function simulates SIMD 128-bit right shift by the standard C.
 * The 128-bit integer given in in is shifted by (shift * 8) bits.
 * This function simulates the LITTLE ENDIAN SIMD.
 * @param out the output of this function
 * @param in the 128-bit data to be shifted
 * @param shift the shift value
 */
static inline void rshift128(w128_t *out, w128_t const *in, int shift) {
    uint64_t th, tl, oh, ol;

    th = ((uint64_t)in->u[3] << 32) | ((uint64_t)in->u[2]);
    tl = ((uint64_t)in->u[1] << 32) | ((uint64_t)in->u[0]);

    oh = th >> (shift * 8);
    ol = tl >> (shift * 8);
    ol |= th << (64 - shift * 8);
    out->u[1] = (uint32_t)(ol >> 32);
    out->u[0] = (uint32_t)ol;
    out->u[3] = (uint32_t)(oh >> 32);
    out->u[2] = (uint32_t)oh;
}

/**
 * This function simulates SIMD 128-bit left shift by the standard C.
 * The 128-bit integer given in in is shifted by (shift * 8) bits.
 * This function simulates the LITTLE ENDIAN SIMD.
 * @param out the output of this function
 * @param in the 128-bit data to be shifted
 * @param shift the shift value
 */

static inline void lshift128(w128_t *out, w128_t const *in, int shift) {
    uint64_t th, tl, oh, ol;

    th = ((uint64_t)in->u[3] << 32) | ((uint64_t)in->u[2]);
    tl = ((uint64_t)in->u[1] << 32) | ((uint64_t)in->u[0]);

    oh = th << (shift * 8);
    ol = tl << (shift * 8);
    oh |= tl >> (64 - shift * 8);
    out->u[1] = (uint32_t)(ol >> 32);
    out->u[0] = (uint32_t)ol;
    out->u[3] = (uint32_t)(oh >> 32);
    out->u[2] = (uint32_t)oh;
}

/**
 * This function represents the recursion formula.
 * @param r output
 * @param a a 128-bit part of the internal state array
 * @param b a 128-bit part of the internal state array
 * @param c a 128-bit part of the internal state array
 * @param d a 128-bit part of the internal state array
 */
static inline void do_recursion(w128_t *r, w128_t *a, w128_t *b, w128_t *c,
				w128_t *d) {
    w128_t x;
    w128_t y;

    lshift128(&x, a, SL2);
    rshift128(&y, c, SR2);
    r->u[0] = a->u[0] ^ x.u[0] ^ ((b->u[0] >> SR1) & MSK1) ^ y.u[0] 
	^ (d->u[0] << SL1);
    r->u[1] = a->u[1] ^ x.u[1] ^ ((b->u[1] >> SR1) & MSK2) ^ y.u[1] 
	^ (d->u[1] << SL1);
    r->u[2] = a->u[2] ^ x.u[2] ^ ((b->u[2] >> SR1) & MSK3) ^ y.u[2] 
	^ (d->u[2] << SL1);
    r->u[3] = a->u[3] ^ x.u[3] ^ ((b->u[3] >> SR1) & MSK4) ^ y.u[3] 
	^ (d->u[3] << SL1);
}

/**
 * This function fills the internal state array with pseudorandom
 * integers.
 * @param intstate internal state array
 */
static inline void gen_rand_all(w128_t *intstate) {
    int i;
    w128_t *r1, *r2;

    r1 = &intstate[N - 2];
    r2 = &intstate[N - 1];
    for (i = 0; i < N - POS1; i++) {
	do_recursion(&intstate[i], &intstate[i], &intstate[i + POS1], r1, r2);
	r1 = r2;
	r2 = &intstate[i];
    }
    for (; i < N; i++) {
	do_recursion(&intstate[i], &intstate[i], &intstate[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &intstate[i];
    }
}

/**
 * This function fills the user-specified array with pseudorandom
 * integers.
 *
 * @param array an 128-bit array to be filled by pseudorandom numbers.  
 * @param size number of 128-bit pseudorandom numbers to be generated.
 * @param intstate internal state array
 */
static inline void gen_rand_array(w128_t *array, int size, w128_t *intstate) {
    int i, j;
    w128_t *r1, *r2;

    r1 = &intstate[N - 2];
    r2 = &intstate[N - 1];
    for (i = 0; i < N - POS1; i++) {
	do_recursion(&array[i], &intstate[i], &intstate[i + POS1], r1, r2);
	r1 = r2;
	r2 = &array[i];
    }
    for (; i < N; i++) {
	do_recursion(&array[i], &intstate[i], &array[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &array[i];
    }
    for (; i < size - N; i++) {
	do_recursion(&array[i], &array[i - N], &array[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &array[i];
    }
    for (j = 0; j < 2 * N - size; j++) {
	intstate[j] = array[j + size - N];
    }
    for (; i < size; i++, j++) {
	do_recursion(&array[i], &array[i - N], &array[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &array[i];
	intstate[j] = array[i];
    }
}

#endif /* defined(HAVE_SSE2) */

/** a parity check vector which certificate the period of 2^{MEXP} */
static uint32_t parity[4] = {PARITY1, PARITY2, PARITY3, PARITY4};

/**
 * This function certificate the period of 2^{MEXP}
 * @param intstate internal state array
 */
static void period_certification(w128_t *intstate) {
    int inner = 0;
    int i, j;
    uint32_t work;
    uint32_t *intstate32;

    intstate32 = &intstate[0].u[0];

    for (i = 0; i < 4; i++)
	inner ^= intstate32[i] & parity[i];
    for (i = 16; i > 0; i >>= 1)
	inner ^= inner >> i;
    inner &= 1;
    /* check OK */
    if (inner == 1) {
	return;
    }
    /* check NG, and modification */
    for (i = 0; i < 4; i++) {
	work = 1;
	for (j = 0; j < 32; j++) {
	    if ((work & parity[i]) != 0) {
		intstate32[i] ^= work;
		return;
	    }
	    work = work << 1;
	}
    }
}

/**
 * This function returns the identification string.
 * The string shows the word size, the Mersenne exponent,
 * and all parameters of this generator.
 */
static const char *get_idstring(void) {
    return IDSTR;
}

/**
 * This function returns the minimum size of array used for \b
 * fill_array32() function.
 * @return minimum size of array used for fill_array32() function.
 */
static int get_min_array_size32(void) {
    return N32;
}

/**
 * This function represents a function used in the initialization
 * by init_by_array
 * @param x 32-bit integer
 * @return 32-bit integer
 */
inline static uint32_t func1(uint32_t x) {
    return (x ^ (x >> 27)) * (uint32_t)1664525UL;
}

/**
 * This function represents a function used in the initialization
 * by init_by_array
 * @param x 32-bit integer
 * @return 32-bit integer
 */
inline static uint32_t func2(uint32_t x) {
    return (x ^ (x >> 27)) * (uint32_t)1566083941UL;
}

/**
 * This function initializes the internal state array with a 32-bit
 * integer seed.
 * Execution of this function guarantees that the internal state
 * array is correctly initialized.
 *
 * @param seed a 32-bit integer used as the seed.
 * @param intstate internal state array
 */
static void init_gen_rand(uint32_t seed, w128_t *intstate) {
    int i;
    uint32_t *intstate32;

    intstate32 = &intstate[0].u[0];

    intstate32[0] = seed;
    for (i = 1; i < N32; i++) {
	intstate32[i] = 1812433253UL * (intstate32[i - 1] 
					    ^ (intstate32[i - 1] >> 30))
	    + i;
    }
    period_certification(&intstate[0]);
}

/**
 * This function initializes the internal state array,
 * with an array of 32-bit integers used as the seeds
 *
 * Execution of this function guarantees that the internal state
 * array is correctly initialized.
 *
 * @param init_key the array of 32-bit integers, used as a seed.
 * @param key_length the length of init_key.
 * @param intstate internal state array
 */
static void init_by_array(uint32_t *init_key, int key_length, w128_t *intstate) {
    int i, j, count;
    uint32_t r;
    int lag;
    int mid;
    int size = N32;
    uint32_t *intstate32;

    intstate32 = &intstate[0].u[0];

    if (size >= 623) {
	lag = 11;
    } else if (size >= 68) {
	lag = 7;
    } else if (size >= 39) {
	lag = 5;
    } else {
	lag = 3;
    }
    mid = (size - lag) / 2;

    memset(&intstate[0], 0x8b, (N32 * 4));

    if (key_length + 1 > N32) {
	count = key_length + 1;
    } else {
	count = N32;
    }
    r = func1(intstate32[0] ^ intstate32[mid] 
	      ^ intstate32[N32 - 1]);
    intstate32[mid] += r;
    r += key_length;
    intstate32[mid + lag] += r;
    intstate32[0] = r;

    count--;
    for (i = 1, j = 0; (j < count) && (j < key_length); j++) {
	r = func1(intstate32[i] ^ intstate32[(i + mid) % N32] 
		  ^ intstate32[(i + N32 - 1) % N32]);
	intstate32[(i + mid) % N32] += r;
	r += init_key[j] + i;
	intstate32[(i + mid + lag) % N32] += r;
	intstate32[i] = r;
	i = (i + 1) % N32;
    }
    for (; j < count; j++) {
	r = func1(intstate32[i] ^ intstate32[(i + mid) % N32] 
		  ^ intstate32[(i + N32 - 1) % N32]);
	intstate32[(i + mid) % N32] += r;
	r += i;
	intstate32[(i + mid + lag) % N32] += r;
	intstate32[i] = r;
	i = (i + 1) % N32;
    }
    for (j = 0; j < N32; j++) {
	r = func2(intstate32[i] + intstate32[(i + mid) % N32] 
		  + intstate32[(i + N32 - 1) % N32]);
	intstate32[(i + mid) % N32] ^= r;
	r -= i;
	intstate32[(i + mid + lag) % N32] ^= r;
	intstate32[i] = r;
	i = (i + 1) % N32;
    }

    period_certification(&intstate[0]);

}

#if 0
static ErlNifFunc nif_funcs[] = {
    {"do_recursion", 4, erl_do_recursion}
};

ERL_NIF_INIT(iconverl, nif_funcs, load, NULL, NULL, NULL)

#endif
