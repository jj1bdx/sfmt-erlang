#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(fprof),
    code:load_file(sfmt),
    code:load_file(sfmt607_tests),
    code:load_file(sfmt4253_tests),
    code:load_file(sfmt216091_tests),
    code:load_file(sfmt_tests),

    fprof:trace(start),
    sfmt607_tests:test_short_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "short_607.txt"}),

    fprof:trace(start),
    sfmt4253_tests:test_short_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "short_4253.txt"}),

    fprof:trace(start),
    sfmt_tests:test_short_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "short_19937.txt"}),

    fprof:trace(start),
    sfmt216091_tests:test_short_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "short_216091.txt"}),

    io:format("end of fprof.escript~n"),
    ok.


