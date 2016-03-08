#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(fprof),
    code:load_file(sfmt),
    code:load_file(sfmt_pure),
    code:load_file(sfmt_tests),

    eprof:start(),
    eprof:start_profiling([self()]),
    sfmt_tests:test_short_speed(),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),

    eprof:start(),
    eprof:start_profiling([self()]),
    sfmt_pure_tests:test_short_speed(),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),

    io:format("end of eprof.escript~n"),
    ok.


