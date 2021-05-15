#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(sfmt),
    code:load_file(sfmt_tests),
    code:load_file(sfmt_pure),
    code:load_file(sfmt_pure_tests),
    io:format("~nSpeed test begins:~n"),
    sfmt_tests:test_speed(),
    sfmt_pure_tests:test_speed(),
    init:stop(),
    ok.


