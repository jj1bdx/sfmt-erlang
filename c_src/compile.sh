#!/bin/sh
gcc44 -fPIC -shared -o sfmt_nif.so sfmt_nif.c -I /usr/local/lib/erlang/usr/include/
