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

/* prototypes */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

static ERL_NIF_TERM sfmt_nif_randlist_to_intstate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_intstate_to_randlist(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_intstate_to_randlist_float(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_gen_rand_all(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_gen_rand_list32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_gen_rand_list_float(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_init_gen_rand(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_init_by_list32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_gen_rand32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_gen_rand_float(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_get_idstring(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sfmt_nif_get_min_array_size32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* prototypes of sfmt-extstate functions */
#if defined(HAVE_SSE2)
static PRE_ALWAYS __m128i mm_recursion(__m128i *a, __m128i *b,
                                   __m128i c, __m128i d, __m128i mask);
#else /* !defined(HAVE_SSE2) */
static inline void rshift128(w128_t *out, w128_t const *in, int shift);
static inline void lshift128(w128_t *out, w128_t const *in, int shift);
static inline void do_recursion(w128_t *r, w128_t *a, w128_t *b, w128_t *c, w128_t *d);
#endif /* defined(HAVE_SSE2) */
static inline void gen_rand_all(w128_t *intstate);
static inline void gen_rand_array(w128_t *array, int size, w128_t *intstate);
static void period_certification(w128_t *intstate);
static const char *get_idstring(void);
static int get_min_array_size32(void);
static void init_gen_rand(uint32_t seed, w128_t *intstate);
static void init_by_array(uint32_t *init_key, int key_length, w128_t *intstate);

static ErlNifFunc nif_funcs[] = {
    {"randlist_to_intstate", 1, sfmt_nif_randlist_to_intstate},
    {"intstate_to_randlist", 1, sfmt_nif_intstate_to_randlist},
    {"intstate_to_randlist_float", 1, sfmt_nif_intstate_to_randlist_float},
    {"gen_rand_all", 1, sfmt_nif_gen_rand_all},
    {"gen_rand_list32", 2, sfmt_nif_gen_rand_list32},
    {"gen_rand_list_float", 2, sfmt_nif_gen_rand_list_float},
    {"init_gen_rand", 1, sfmt_nif_init_gen_rand},
    {"init_by_list32", 1, sfmt_nif_init_by_list32},
    // {"gen_rand32", 1, sfmt_nif_gen_rand32},
    // {"gen_rand_float", 1, sfmt_nif_gen_rand_float},
    {"get_idstring", 0, sfmt_nif_get_idstring},
    {"get_min_array_size32", 0, sfmt_nif_get_min_array_size32}
};

ERL_NIF_INIT(sfmt, nif_funcs, load, reload, upgrade, unload)

/* atom variables */
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_error1;
static ERL_NIF_TERM atom_error2;
static ERL_NIF_TERM atom_error3;
static ERL_NIF_TERM atom_error_sfmt_nomem;
static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* initializing atoms */
    atom_error = enif_make_atom(env,"error");
    atom_error1 = enif_make_atom(env,"error1");
    atom_error2 = enif_make_atom(env,"error2");
    atom_error3 = enif_make_atom(env,"error3");
    atom_error_sfmt_nomem = enif_make_atom(env,"error_sfmt_nomem");
    atom_ok = enif_make_atom(env,"ok");

    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

static ERL_NIF_TERM
sfmt_nif_randlist_to_intstate(ErlNifEnv *env, int argc, 
			      const ERL_NIF_TERM argv[])
{ /* ([list of N32 elements]) */
    w128_t *i;
    unsigned int j;
    ERL_NIF_TERM head, tail, r;

    if (!enif_get_list_length(env, argv[0], &j)
        || j != N32) {
	return enif_make_badarg(env);
    }

    i = (w128_t *)enif_make_new_binary(env, (N32 * 4), &r);

    if (!enif_get_list_cell(env, argv[0], &head, &tail)
	|| !enif_get_uint(env, head, &i[0].u[0])
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !enif_get_uint(env, head, &i[0].u[1])
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !enif_get_uint(env, head, &i[0].u[2])
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !enif_get_uint(env, head, &i[0].u[3])) {
	return enif_make_badarg(env);
    }

    for (j = 1; j < (N32 / 4); j++) {
	if (!enif_get_list_cell(env, tail, &head, &tail)
	    || !enif_get_uint(env, head, &i[j].u[0])
	    || !enif_get_list_cell(env, tail, &head, &tail)
	    || !enif_get_uint(env, head, &i[j].u[1])
	    || !enif_get_list_cell(env, tail, &head, &tail)
	    || !enif_get_uint(env, head, &i[j].u[2])
	    || !enif_get_list_cell(env, tail, &head, &tail)
	    || !enif_get_uint(env, head, &i[j].u[3])) {
	return enif_make_badarg(env);
	}
    }

    if (!enif_is_empty_list(env, tail)) {
	return enif_make_badarg(env);
    }

    return r;
}

static ERL_NIF_TERM
sfmt_nif_intstate_to_randlist(ErlNifEnv *env, int argc, 
			      const ERL_NIF_TERM argv[])
{ /* (<<binary of (N32 * 4) bytes>>) */
   ErlNifBinary r;
   w128_t *p;
   ERL_NIF_TERM l[N32];
   int j, k;

   if (!enif_inspect_binary(env, argv[0], &r)
       || r.size != (N32 * 4)) {
      return enif_make_badarg(env);
   }
   p = (w128_t *)r.data;

   for (j = 0, k = 0; j < (N32 / 4); j++, k += 4) {
      l[k] = enif_make_uint(env, p[j].u[0]); 
      l[k + 1] = enif_make_uint(env, p[j].u[1]); 
      l[k + 2] = enif_make_uint(env, p[j].u[2]); 
      l[k + 3] = enif_make_uint(env, p[j].u[3]); 
   }

   return enif_make_list_from_array(env, l, N32);
}

static ERL_NIF_TERM
sfmt_nif_intstate_to_randlist_float(ErlNifEnv *env, int argc, 
				    const ERL_NIF_TERM argv[])
{ /* (<<binary of (N32 * 4) bytes>>) */
    ErlNifBinary r;
    w128_t *p;
    ERL_NIF_TERM l[N32];
    int j, k;

    if (!enif_inspect_binary(env, argv[0], &r)
        || r.size != (N32 * 4)) {
	return enif_make_badarg(env);
    }
    p = (w128_t *)r.data;

    for (j = 0, k = 0; j < (N32 / 4); j++, k += 4) {
	l[k] = enif_make_double(env, FLOAT_CONST*p[j].u[0]); 
	l[k + 1] = enif_make_double(env, FLOAT_CONST*p[j].u[1]); 
	l[k + 2] = enif_make_double(env, FLOAT_CONST*p[j].u[2]); 
	l[k + 3] = enif_make_double(env, FLOAT_CONST*p[j].u[3]);
    }

    return enif_make_list_from_array(env, l, N32);
}

static ERL_NIF_TERM
sfmt_nif_gen_rand_all(ErlNifEnv *env, int argc, 
		      const ERL_NIF_TERM argv[])
{ /* (<<binary of (N32 * 4) bytes) */
    ErlNifBinary p;
    ERL_NIF_TERM r;
    w128_t *q;

    if (!enif_inspect_binary(env, argv[0], &p)
        || p.size != (N32 * 4)) {
	return enif_make_badarg(env);
    }

    /* make a new binary object first */
    q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
    /* copy the original data first before manipulating */
    memcpy(q, p.data, N32 * 4);
    /* the new (mutable) q has the new random data */
    gen_rand_all(q);
    
    return r;
}

static ERL_NIF_TERM
sfmt_nif_gen_rand_list32(ErlNifEnv *env,
			 int argc, const ERL_NIF_TERM argv[])
{ /* (size, intstate()) */
    unsigned int size, req;
    ErlNifBinary p;
    ERL_NIF_TERM *terms;
    ERL_NIF_TERM r, list;
    w128_t *array, *q;
    int j, k;
    
    if (!enif_get_uint(env, argv[0], &req)) {
       return enif_make_badarg(env);
    }
    
    if (req < N32) {
       size = N32;
    } else {
       size = req + (4 - (req % 4)) % 4;
    }

    if (!enif_inspect_binary(env, argv[1], &p)
        || p.size != (N32 * 4)) {
       return enif_make_badarg(env);
    }
    
    /* list terms */
    terms = (ERL_NIF_TERM *) enif_alloc(size * sizeof(ERL_NIF_TERM *));
    if (NULL == terms) {
	return atom_error_sfmt_nomem;
    }
    /* working area for PRNG computation */
    array = (w128_t *) enif_alloc(size * 4);
    if (NULL == array) {
	return atom_error_sfmt_nomem;
    }

    /* make a new binary object first */
    q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
    /* copy the original data first before manipulating */
    memcpy(q, p.data, N32 * 4);
    /* the new (mutable) q has the new random data */
    /* size is for w128_t */
    gen_rand_array(array, size / 4, q);

    /* generate the list terms from the result array */
    for (j = 0, k = 0; j < (size / 4); j++, k += 4) {
	terms[k] = enif_make_uint(env, array[j].u[0]); 
	terms[k + 1] = enif_make_uint(env, array[j].u[1]); 
	terms[k + 2] = enif_make_uint(env, array[j].u[2]); 
	terms[k + 3] = enif_make_uint(env, array[j].u[3]); 
    }
    
    list = enif_make_list_from_array(env, terms, req);

    /* freeing objects already converted into another ERL_NIF_TERM */
    enif_free(array);
    enif_free(terms);

    return enif_make_tuple2(env, list, r);
}

static ERL_NIF_TERM
sfmt_nif_gen_rand_list_float(ErlNifEnv *env,
			 int argc, const ERL_NIF_TERM argv[])
{ /* (size, intstate()) */
   unsigned int size, req;
   ErlNifBinary p;
   ERL_NIF_TERM *terms;
   ERL_NIF_TERM r, list;
   w128_t *array, *q;
   int j, k;
    
   if (!enif_get_uint(env, argv[0], &req)) {
      return enif_make_badarg(env);
   }
    
   if (req < N32) {
      size = N32;
   } else {
      size = req + (4 - (req % 4)) % 4;
   }
    
   if (!enif_inspect_binary(env, argv[1], &p)
       || p.size != (N32 * 4)) {
      return enif_make_badarg(env);
   }
    
   /* list terms */
   terms = (ERL_NIF_TERM *) enif_alloc(size * sizeof(ERL_NIF_TERM *));
   if (NULL == terms) {
      return atom_error_sfmt_nomem;
   }
   /* working area for PRNG computation */
   array = (w128_t *) enif_alloc(size * 4);
   if (NULL == array) {
      return atom_error_sfmt_nomem;
   }

   /* make a new binary object first */
   q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
   /* copy the original data first before manipulating */
   memcpy(q, p.data, N32 * 4);
   /* the new (mutable) q has the new random data */
   /* size is for w128_t */
   gen_rand_array(array, size / 4, q);

   /* generate the list terms from the result array */
   for (j = 0, k = 0; j < (size / 4); j++, k += 4) {
      terms[k] = enif_make_double(env, FLOAT_CONST*array[j].u[0]); 
      terms[k + 1] = enif_make_double(env, FLOAT_CONST*array[j].u[1]); 
      terms[k + 2] = enif_make_double(env, FLOAT_CONST*array[j].u[2]); 
      terms[k + 3] = enif_make_double(env, FLOAT_CONST*array[j].u[3]); 
   }
    
   list = enif_make_list_from_array(env, terms, req);

   /* freeing objects already converted into another ERL_NIF_TERM */
   enif_free(array);
   enif_free(terms);

   return enif_make_tuple2(env, list, r);
}


static ERL_NIF_TERM sfmt_nif_init_gen_rand(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* (integer_seed) */
    unsigned int seed;
    ERL_NIF_TERM r;
    w128_t *q;

    if (!enif_get_uint(env, argv[0], &seed)) {
	return enif_make_badarg(env);
    }
    q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
    init_gen_rand(seed, q);

    return r;
}

static ERL_NIF_TERM sfmt_nif_init_by_list32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{ /* ([arbitrary_length_of_integer_list]) */
    w128_t *q;
    uint32_t *il;
    unsigned int j, size;
    ERL_NIF_TERM head, tail, r;

    if (!enif_get_list_length(env, argv[0], &size)
        || size == 0) {
	return enif_make_badarg(env);
    }

    /* init list */
    il = (uint32_t *) enif_alloc(size * 4);
    if (NULL == il) {
	return atom_error_sfmt_nomem;
    }

    if (!enif_get_list_cell(env, argv[0], &head, &tail)
	|| !enif_get_uint(env, head, &il[0])) {
	return enif_make_badarg(env);
    }
    for (j = 1; j < size; j++) {
	if (!enif_get_list_cell(env, tail, &head, &tail)
	    || !enif_get_uint(env, head, &il[j])) {
	return enif_make_badarg(env);
	}
    }
    if (!enif_is_empty_list(env, tail)) {
	return enif_make_badarg(env);
    }

    q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
    init_by_array(il, size, q);

    /* freeing objects already converted into another ERL_NIF_TERM */
    enif_free(il);

    return r;
}

static ERL_NIF_TERM 
sfmt_nif_gen_rand32(ErlNifEnv *env, 
		    int argc, const ERL_NIF_TERM argv[])
{ /* ({integer_index, intstate()}) */
    int arity, idx;
    ErlNifBinary p;
    ERL_NIF_TERM r;
    ERL_NIF_TERM bin_in;
    const ERL_NIF_TERM *tuple_args;
    w128_t *q;
    
    if (enif_get_tuple(env, argv[0], &arity, &tuple_args)) {
       if(arity != 2
	  || !enif_get_int(env, tuple_args[0], &idx)
	  || !enif_inspect_binary(env, tuple_args[1], &p)
	  || p.size != (N32 * 4))
	  return enif_make_badarg(env);
       else 
	  bin_in = tuple_args[1];
    } else if (!enif_inspect_binary(env, argv[0], &p)
	       || p.size != (N32 * 4)) {
       return enif_make_badarg(env);
    } else {
       idx = 0;
       bin_in = argv[0];
    }

    if (idx >= N32) {
        /* make a new binary object first */
	/* (remember p is NOT mutable) */
	q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
	/* copy the original data first before manipulating */
	memcpy(q, p.data, N32 * 4);
	/* the new (mutable) q has the new random data */
	gen_rand_all(q);
        /* {table[0], {1, copied_table} */
        return enif_make_tuple2(env,
				enif_make_uint(env, q[0].u[0]),
				enif_make_tuple2(env,
						 enif_make_int(env, 1),
						 r));
    } else {
	q = (w128_t *) p.data;
	/* {table[idx], {idx + 1, copied_table} */
	return enif_make_tuple2(env,
				enif_make_uint(env, 
					       q[(idx / 4)].u[(idx % 4)]),
				enif_make_tuple2(env,
						 enif_make_int(env, idx + 1),
						 bin_in));
    }
}

static ERL_NIF_TERM 
sfmt_nif_gen_rand_float(ErlNifEnv *env, 
		       int argc, const ERL_NIF_TERM argv[])
{ /* ({integer_index, intstate()}) */
   int arity, idx;
   ErlNifBinary p;
   ERL_NIF_TERM r;
   ERL_NIF_TERM bin_in;
   const ERL_NIF_TERM *tuple_args;
   w128_t *q;
    
   if (enif_get_tuple(env, argv[0], &arity, &tuple_args)) {
      if(arity != 2
	 || !enif_get_int(env, tuple_args[0], &idx)
	 || !enif_inspect_binary(env, tuple_args[1], &p)
	 || p.size != (N32 * 4))
	 return enif_make_badarg(env);
      else 
	 bin_in = tuple_args[1];
   } else if (!enif_inspect_binary(env, argv[0], &p)
	      || p.size != (N32 * 4)) {
      return enif_make_badarg(env);
   } else {
      idx = 0;
      bin_in = argv[0];
   }

   if (idx >= N32) {
      /* make a new binary object first */
      /* (remember p is NOT mutable) */
      q = (w128_t *) enif_make_new_binary(env, (N32 * 4), &r);
      /* copy the original data first before manipulating */
      memcpy(q, p.data, N32 * 4);
      /* the new (mutable) q has the new random data */
      gen_rand_all(q);
      /* {table[0], {1, copied_table} */
      return enif_make_tuple2(env,
			      enif_make_double(env, FLOAT_CONST*q[0].u[0]),
			      enif_make_tuple2(env,
					       enif_make_int(env, 1),
					       r));
   } else {
      q = (w128_t *) p.data;
      /* {table[idx], {idx + 1, copied_table} */
      return enif_make_tuple2(env,
			      enif_make_double(env, FLOAT_CONST*
					     q[(idx / 4)].u[(idx % 4)]),
			      enif_make_tuple2(env,
					       enif_make_int(env, idx + 1),
					       bin_in));
   }
}


static ERL_NIF_TERM
sfmt_nif_get_idstring(ErlNifEnv *env, int argc, 
			      const ERL_NIF_TERM argv[])
{ /* () */
    return enif_make_string(env, get_idstring(), ERL_NIF_LATIN1);
}

static ERL_NIF_TERM
sfmt_nif_get_min_array_size32(ErlNifEnv *env, int argc, 
			      const ERL_NIF_TERM argv[])
{ /* () */
    return enif_make_uint(env, (unsigned int) get_min_array_size32());
}

/* sfmt-extstate static C function code follows */

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

