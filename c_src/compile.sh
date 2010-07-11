#!/bin/sh
CFLAGS="-O3 -finline-functions -fomit-frame-pointer -fno-strict-aliasing --param max-inline-insns-single=1800 --param inline-unit-growth=500 --param large-function-growth=900 -Wmissing-prototypes -Wall -std=c99"; export CFLAGS
#
gcc44 $CFLAGS -fPIC -shared -o sfmt_nif.so sfmt_nif.c -I /usr/local/lib/erlang/usr/include/
