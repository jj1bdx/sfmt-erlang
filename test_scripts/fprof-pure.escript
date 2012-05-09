#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(fprof),
    code:load_file(sfmt),
    code:load_file(sfmt_pure),
    code:load_file(sfmt_tests),

    fprof:trace(start),
    sfmt_tests:test_short_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "short_nif.txt"}),

    fprof:trace(start),
    sfmt_pure_tests:test_short_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "short_pure.txt"}),

    io:format("end of fprof.escript~n"),
    ok.


