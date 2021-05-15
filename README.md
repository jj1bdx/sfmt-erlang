[//]: # (-*- coding: utf-8 -*-)

# sfmt-erlang: SIMD-oriented Fast Mersenne Twister (SFMT) for Erlang

* Version 0.14.0 15-MAY-2021
* Edited and written by Kenji Rikitake (Kenji Rikitake Professional Engineer's Office)
* Email contact: <kenji.rikitake@acm.org>

## Build and test commands

* Building: `rebar3 compile`
* C NIF Documentation: `doxygen` (requires Doxygen 1.9.1 or later)
* Erlang Documentation: `rebar3 edoc`
* Testing: `rebar3 ct`
* Execution speed benchmark: execute `rebar3 shell` and run `sfmt_tests:test_speed()` and `sfmt_pure_tests:test_speed()`
* Cleaning up: `rebar3 clean`

## Use rebar3 to build since 0.14.0

* Since 14.0, erlang.mk and mix.exs are removed. Use rebar3 to build.

## Use 0.13.2 and later for OTP 23

* erlang.mk is updated to stop using `-lerl_interface`. This is required for a successful build in OTP 23.

## Security notice regarding the PHP mt_seed() vulnerability

Ambionics Security published [an internal state retrieval algorithm of PHP `mt_rand()`](https://www.ambionics.io/blog/php-mt-rand-prediction) on 6-JAN-2020. sfmt-erlang uses the same seed-to-internal-state initialization algorithm at the function `init_gen_rand/1`.

For reducting the possibility of the internal state revelation, use `init_by_list32/1` instead, better combined with `rand:uniform/1`. [Raimo Niskanen published a piece of code for this purpose](http://erlang.org/pipermail/erlang-questions/2018-July/095875.html).

*Note well that sfmt-erlang has no cryptographic security guarantee and MUST NOT be used for security purposes such as password generation.*

Also: Version 0.13.0 and 0.13.1 Erlang and C code files are identical. Users have no need to upgrade.

Thanks to Shiro Kawai for the notification of the seed initialization algorithm issue. 

## Travis CI build status for the master branch

[![Build Status](https://travis-ci.org/jj1bdx/sfmt-erlang.svg?branch=master)](https://travis-ci.org/jj1bdx/sfmt-erlang)

## License

Copyright (c) 2010-2021 Kenji Rikitake and Kyoto University. All rights
reserved.

Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
University. All rights reserved.

See the file `LICENSE` for the license (new/simplified BSD license).

## Details of SFMT algorithm

This software is based on SFMT ver. 1.3.3 (SIMD oriented Fast Mersenne
Twister(SFMT)) by Mutsuo Saito (Hiroshima University) and Makoto Matsumoto
(Hiroshima University). See
<http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/index.html> for the further details.

Note well: only (2^19937 - 1) period is supported.

## Available modules

* module `sfmt`: SFMT with C NIFs
* module `sfmt_pure`: SFMT with pure Erlang
* See [CHANGES.md](https://github.com/jj1bdx/sfmt-erlang/blob/master/CHANGES.md) for the detail of miscellaneous changes

## C NIFs

* Compiler requirement: C99 support (modern Clang and gcc will do): `<inttypes.h>` required
* See `c_src/sfmt_nif.c` for the details
* Also refer to sfmt-extstate at <http://github.com/jj1bdx/sfmt-extstate>
* The version number of this NIF is 101 (see `NIF_LOAD_INFO` macro value)

## Tested platforms

* macOS 10.15.7 with Erlang/OTP 24.0
* Ubuntu 20.10 with Erlang/OTP 24.0
* sfmt-erlang will work with older OTP versions

## hex.pm support

* hex is supported through rebar3\_hex plugin
* NIF building errors fixed (0.12.7 and later)
* Package name: `sfmt`
* Note: all builds including C and Erlang source compilation are done with rebar3

## API compatible with the rand module

    seed/3, uniform/0, uniform/1, uniform_s/1, uniform_s/3 

## On HiPE usage

* HiPE is removed from OTP 24, no longer configured
* sfmt module is NIFnized so does not coexist with HiPE
* On the other hand, `sfmt_pure` module can be compiled with HiPE or `+native "+{hipe, [o3]}"` erlc compile option, which will result in 40% to 100% speedup on 64-bit machines

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
* Michael Chmielewski
* David Whitlock
* Pierre Fenoll

## THANKS to:

* Dave "dizzyd" Smith
* Tuncer Ayaz
* Tim Bates (random\_mt.erl implementator of Mersenne Twister)
* Richard O'Keefe
* Yurii Rashkovskii
* Kostis Sagonas
* Michael Gebetsroither
* Shunichi Shinohara
* Loïc Hoguin
* @timCF of GitHub
* Shiro Kawai

## ACKNOWLEDGMENTS

During the compatibility test of this software, Kenji Rikitake
used the supercomputer service provided by Academic Center for
Computing and Media Studies (ACCMS), Kyoto University.

Erlang Solutions kindly gave Kenji Rikitake
an opportunity to give a presentation
about this software at Erlang Factory SF Bay 2011.

Pepabo R&D Institute of GMO Pepabo, Inc.
kindly gives the financial support for this project.
