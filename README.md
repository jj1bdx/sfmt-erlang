# sfmt-erlang: SIMD-oriented Fast Mersenne Twister (SFMT) for Erlang

* Version 0.3.2_RELEASE 14-JUL-2010
* Edited and written by Kenji Rikitake (Kyoto University)
* Email contact: kenji.rikitake AT acm.org (change AT to @ for the email address)

Copyright (c) 2010 Kenji Rikitake and Kyoto University. All rights
reserved.

Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
University. All rights reserved.

See LICENSE.txt for the license (new/simplified BSD license).
(Note: license of rebar is Apache 2.0 License.)

This software is based on SFMT ver. 1.3.3
(SIMD oriented Fast Mersenne Twister(SFMT))
by 
Mutsuo Saito (Hiroshima University) and
Makoto Matsumoto (Hiroshima University)

Details of SFMT algorithm is accessible on the Web from:
http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/index.html

# C NIFs based on SFMT 1.3.3 added from 0.3.0_RELEASE

* reference: sfmt-extstate at http://github.com/jj1bdx/sfmt-extstate
* see c_src/sfmt_nif.[ch] for the details
* speedup: ~40 times faster than the pure Erlang code (when fully inline-optimized (see rebar.config))
* Pure Erlang code available under reference_texts/
* gen_rand32/1 and gen_rand_float/1 use Erlang lists (Dan Gudmudsson showed the list version is faster)

# Tested platforms

* FreeBSD/i386 7.3-RELEASE with Erlang/OTP R14A
* RedHat Enterprise Linux AS V4 of x86_64 with Erlang/OTP R14A compiled by gcc 3.4.6 
  (on the Thin Cluster of the Kyoto University ACCMS Supercomputer System)

# Building 

The build script is Basho's rebar at http://hg.basho.com/rebar/ 
(which requires Erlang/OTP to run)

# API compatible with the random module

    seed0, seed/0, seed/3, uniform/0, uniform/1, uniform_s/1, uniform_s/3 

# Refactoring note of the pure-Erlang code

* Rewriting ++ (append) operators by ring buffer loops
(as a pair of lists consuming the head elements, and the corresponding accumulators)
made the code ~50% faster

# TODO

* Documentation
* NIF reload/upgrade/upload code fix; not yet fully tested

# Code authors:

* Mutsuo Saito
* Makoto Matsumoto
* Kenji Rikitake
* Dan Gudmundsson

# THANKS to:

* Dave "dizzyd" Smith
* Tuncer Ayaz
* Tim Bates (random_mt.erl implementation of Mersenne Twister)
* Dan Gudmundsson

# ACKNOWLEDGMENT

During the compatibility test of this software, Kenji Rikitake
used the supercomputer service provided by Academic Center for
Computing and Media Studies (ACCMS), Kyoto University.

[End of README]
