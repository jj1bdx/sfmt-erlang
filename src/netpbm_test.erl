%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @doc netpbm pattern generator for testing random numbers
%% @reference <a href="http://github.com/jj1bdx/sfmt-erlang">GitHub page
%% for sfmt-erlang</a>
%% @copyright 2011 Kenji Rikitake and Kyoto University.

%% Copyright (c) 2011 Kenji Rikitake and Kyoto University. All rights
%% reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:
%%
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above
%%       copyright notice, this list of conditions and the following
%%       disclaimer in the documentation and/or other materials provided
%%       with the distribution.
%%     * Neither the names of the Kyoto University 
%%       nor the names of its contributors may be used to
%%       endorse or promote products derived from this software without
%%       specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(netpbm_test).

-export([test/0, pgm_output/3]).

%% @doc dumping reference output for the regression test

pgm_output_list1(0, _) ->
    io:format("~n");
pgm_output_list1(X, F) ->
    io:format("~p ", [trunc(F() * 2)]),
    pgm_output_list1(X-1, F).

pgm_output_list2(0, _) ->
    ok;
pgm_output_list2(Y, F) ->
    F(),
    pgm_output_list2(Y-1, F).

pgm_output(X, Y, F) when is_integer(X), is_integer(Y) ->
    io:format("~p ~p~n", [X, Y]),
    pgm_output_list2(Y, fun() -> pgm_output_list1(X, F) end),
    ok.

test() ->
    io:format("P1~n# random:uniform/0~n"),
    random:seed(),
    pgm_output(512,512,fun random:uniform/0),
    io:format("P1~n# random_wh06:uniform/0~n"),
    random_wh06:seed(),
    pgm_output(512,512,fun random_wh06:uniform/0),
    io:format("P1~n# sfmt:uniform/0~n"),
    sfmt:seed(),
    pgm_output(512,512,fun sfmt:uniform/0),
    ok.
    
