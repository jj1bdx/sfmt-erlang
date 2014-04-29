# sfmt-erlang: SIMD-oriented Fast Mersenne Twister (SFMT) for Erlang

* Version 0.9.1\_BETA 29-APR-2014
* Edited and written by Kenji Rikitake (Kenji Rikitake Professional Engineer's Office)
* Email contact: <kenji.rikitake@acm.org>

Copyright (c) 2010-2014 Kenji Rikitake and Kyoto University. All rights
reserved.

Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
University. All rights reserved.

See the file `LICENSE` for the license (new/simplified BSD license). (Note:
license of rebar is Apache 2.0 License.)

This software is based on SFMT ver. 1.3.3 (SIMD oriented Fast Mersenne
Twister(SFMT)) by Mutsuo Saito (Hiroshima University) and Makoto
Matsumoto (Hiroshima University)

## Details of SFMT algorithm

See <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/index.html>

* Period: only (2^19937 - 1) period is supported

## Available modules

* module `sfmt`: SFMT with C NIFs
* module `sfmt_pure`: SFMT with pure Erlang
* See [CHANGES.md](https://github.com/jj1bdx/sfmt-erlang/blob/master/CHANGES.md) for the detail of miscellaneous changes

## C NIFs

* See `c_src/sfmt_nif.c` for the details
* Also refer to sfmt-extstate at <http://github.com/jj1bdx/sfmt-extstate>
* The version number of this NIF is 101 (see `NIF_LOAD_INFO` macro value)

## Tested platforms

* FreeBSD/amd64 10-STABLE with Erlang/OTP 17.0
* OS X 10.9.2 Mavericks with Erlang/OTP 17.0
* CentOS 6.5 (amd64, on Virtualbox running on OS X 10.9.2) with Erlang/OTP 17.0

## Building 

* Use GNU/BSD make and then

    make compile

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
* Tim Bates (random\_mt.erl implementator of Mersenne Twister)
* Dan Gudmundsson
* Richard O'Keefe
* Yurii Rashkovskii
* Kostis Sagonas
* Michael Gebetsroither
* Shunichi Shinohara

## ACKNOWLEDGMENTS

During the compatibility test of this software, Kenji Rikitake
used the supercomputer service provided by Academic Center for
Computing and Media Studies (ACCMS), Kyoto University.

Erlang Solutions kindly gave Kenji Rikitake
an opportunity to give a presentation
about this software at Erlang Factory SF Bay 2011.
