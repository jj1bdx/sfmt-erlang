# sfmt-erlang: SIMD-oriented Fast Mersenne Twister (SFMT) for Erlang

* Version 0.7_RELEASE 25-MAR-2012 (README.md revised 26-MAY-2011)
* Edited and written by Kenji Rikitake (Kyoto University)
* Email contact: <kenji.rikitake@acm.org>

Copyright (c) 2010-2012 Kenji Rikitake and Kyoto University. All rights
reserved.

Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
University. All rights reserved.

See LICENSE.txt for the license (new/simplified BSD license). (Note:
license of rebar is Apache 2.0 License.)

This software is based on SFMT ver. 1.3.3 (SIMD oriented Fast Mersenne
Twister(SFMT)) by Mutsuo Saito (Hiroshima University) and Makoto
Matsumoto (Hiroshima University)

## Details of SFMT algorithm

See <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/index.html>

* new module sfmt607 and sfmt216091 added (from 0.5.1_RELEASE)
* new module sfmt4253 added (from 0.5.3_RELEASE)
* new module sfmt86243 added (from 0.6.1_BETA)

## Supported SFMT PRNG periods

* sfmt607: (2^607 - 1)
* sfmt4253: (2^4253 - 1)
* sfmt: (2^19937 - 1)
* sfmt86243: (2^86243 - 1)
* sfmt216091: (2^216091 - 1)

## C NIFs based on SFMT 1.3.3 added (from 0.3.0_RELEASE)

* See `c_src/sfmt_nif.c` for the details
* Also refer to sfmt-extstate at <http://github.com/jj1bdx/sfmt-extstate>
* The version number of this NIF is 101 (see `NIF_LOAD_INFO` macro value)

## Wichmann-Hill 2006 algorithm code also included (from 0.6.0_RELEASE)

* See `src/random_wh06.erl` for the details
* Reference for the algorithm:

    B. A. Wichmann and I. D. Hill,
    "Generating good pseudo-random numbers",
    Computational Statistics & Data Analysis 51 (2006) 1614-1622.    

* See `src/random_wh06_int.erl` for a bigint version (by Michael Truog)

    Note: this bigint version is not tested yet (no eunit test case)

## Simple test of 512x512 pixmap included (from 0.6.2_RELEASE)

* see the files under `reference_texts/pbm_512_512/` for the details

## Notable bugfixes

* Catched up with the latest rebar configuration file (from 0.7_RELEASE)
* Dynamically building ebin/sfmt.app (from 0.7_RELEASE)
* PDIC_SEED now named differently for each period (bugfix, from 0.5.2_RELEASE)

## Tested platforms

* FreeBSD/i386 9.0-RELEASE with Erlang/OTP R15B

## Building 

* Use GNU make and then

    make compile

(Note: on FreeBSD, GNU make should be invoked as `gmake`.)

The build script is Basho's rebar at <https://github.com/basho/rebar>,
which will be automatically fetched under the directory `support/`.

## Documentation

* For the HTML documentation files of the Erlang source 

    make doc

    The documentation will be accessible at `doc/index.html`.

* For the HTML documentation files of the C NIF source (Note: Doxygen required)

    make c_doc

    The documentation will be accessible at `c_doc/index.html`.

## Testing

* For unit testing with EUnit, do

    make eunit

* For testing the speed of 100 times of invoking 100000 `gen_rand32/1` function, do 

    make speed

    (Previously this was 1000 times x 100000 invocations, but that was practically too slow.)

## API compatible with the random module

    seed0, seed/0, seed/3, uniform/0, uniform/1, uniform_s/1, uniform_s/3 

## TODO

* No more new feature; more load testing needed
* More documentation
* Code upgrading/reloading behavior check

## Code authors:

* Kenji Rikitake
* Mutsuo Saito
* Makoto Matsumoto
* Dan Gudmundsson
* Michael Truog

## THANKS to:

* Dave "dizzyd" Smith
* Tuncer Ayaz
* Tim Bates (random_mt.erl implementator of Mersenne Twister)
* Dan Gudmundsson
* Richard O'Keefe

## ACKNOWLEDGMENTS

During the compatibility test of this software, Kenji Rikitake
used the supercomputer service provided by Academic Center for
Computing and Media Studies (ACCMS), Kyoto University.

Erlang Solutions kindly gave Kenji Rikitake
an opportunity to give a presentation
about this software at Erlang Factory SF Bay 2011.

## Notes on refactoring

* Speedup by NIF: ~40 times faster than the pure Erlang code
  (when fully inline-optimized (see rebar.config))
* For the pure-Erlang code: writing `++` (append) operators by ring buffer loops
  (as a pair of lists consuming the head elements, and the corresponding accumulators)
  made the code ~50% faster; the pure Erlang code available under `reference_texts/`
* `gen_rand32/1`, `gen_rand32_max/2`, and `gen_rand_float/1` use Erlang lists 
  (Dan Gudmudsson showed the list version is faster)
* SSE2 code and options in sfmt-extstate were removed due to causing crash of Erlang BEAM
  (and even if the SSE2 code was enabled the performance increase would be minimal)
