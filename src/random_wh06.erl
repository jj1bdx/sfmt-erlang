%% Modified version of random module
%% to use Wichmann-Hill algorithm published on 2006
%% which succeeds the old AS183 algorithm in 1982.

%% Copyright (c) 2010 Kenji Rikitake All rights reserved.

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(random_wh06).

%% Reasonable random number generator.
%%  The method is attributed to B. A. Wichmann and I. D. Hill
%%  See "Generating good pseudo-random numbers",
%%  Computational Statistics & Data Analysis 51 (2006) 1614-1622.

-export([seed/0, seed/1, seed/4,
	 uniform/0, uniform/1,
	 uniform_s/1, uniform_s/2, seed0/0,
	 next_sequence/1]).

%%-----------------------------------------------------------------------
%% The type of the state

-type ran() :: {integer(), integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

-spec seed0() -> ran().

seed0() ->
    {123456789, 345678901, 567890123, 789012345}.

%% seed()
%%  Seed random number generation with default values

-spec seed() -> ran().

seed() ->
    reseed(seed0()).

%% seed({A1, A2, A3, A4}) 
%%  Seed random number generation 

-spec seed({integer(), integer(), integer(), integer()}) -> 'undefined' | ran().

seed({A1, A2, A3, A4}) ->
    seed(A1, A2, A3, A4).

%% seed(A1, A2, A3, A4) 
%%  Seed random number generation 

-spec seed(integer(), integer(), integer(), integer()) -> 'undefined' | ran().

%% zero is prohibited for each seed element
%% (by Richard O'Keefe)
seed(A1, A2, A3, A4) ->
    put(random_wh06_seed,
	{abs(A1) rem 2147483578 + 1,
	 abs(A2) rem 2147483542 + 1,
	 abs(A3) rem 2147483422 + 1,
	 abs(A4) rem 2147483122 + 1}).

-spec reseed(ran()) -> ran().

reseed({A1, A2, A3, A4}) ->
    case seed(A1, A2, A3, A4) of
	undefined -> seed0();
	{_,_,_,_} = Tuple -> Tuple
    end.	

%% uniform()
%%  Returns a random float between 0 and 1.

-spec uniform() -> float().

uniform() ->
    {A1, A2, A3, A4} = case get(random_wh06_seed) of
			   undefined -> seed0();
			   Tuple -> Tuple
		       end,
    B1 = 11600 * (A1 rem 185127) -
	 10379 * (A1 div 185127),
    B2 = 47003 * (A2 rem 45688) -
	 10479 * (A2 div 45688),
    B3 = 23000 * (A3 rem 93368) -
	 19423 * (A3 div 93368),
    B4 = 33000 * (A4 rem 65075) -
	 8123 * (A4 div 65075),

    C1 = if
	     B1 < 0 -> B1 + 2147483579;
	     true -> B1
	 end,
    C2 = if
	     B2 < 0 -> B2 + 2147483543;
	     true -> B2
	 end,
    C3 = if
	     B3 < 0 -> B3 + 2147483423;
	     true -> B3
	 end,
    C4 = if
	     B4 < 0 -> B4 + 2147483123;
	     true -> B4
	 end,

    put(random_wh06_seed, {C1, C2, C3, C4}),

    R = (C1 * 0.0000000004656613022697297188506231646486) +
	(C2 * 0.0000000004656613100759859932486569933169) +
	(C3 * 0.0000000004656613360968421314794009471615) +
	(C4 * 0.0000000004656614011489951998100056779817),
    R - trunc(R).

%% uniform(N) -> I
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

-spec uniform(pos_integer()) -> pos_integer().

uniform(N) when is_integer(N), N >= 1 ->
    trunc(uniform() * N) + 1.

%%% Functional versions

%% uniform_s(State) -> {F, NewState}
%%  Returns a random float between 0 and 1.

-spec uniform_s(ran()) -> {float(), ran()}.

uniform_s({A1, A2, A3, A4}) ->
    B1 = 11600 * (A1 rem 185127) -
	 10379 * (A1 div 185127),
    B2 = 47003 * (A2 rem 45688) -
	 10479 * (A2 div 45688),
    B3 = 23000 * (A3 rem 93368) -
	 19423 * (A3 div 93368),
    B4 = 33000 * (A4 rem 65075) -
	 8123 * (A4 div 65075),

    C1 = if
	     B1 < 0 -> B1 + 2147483579;
	     true -> B1
	 end,
    C2 = if
	     B2 < 0 -> B2 + 2147483543;
	     true -> B2
	 end,
    C3 = if
	     B3 < 0 -> B3 + 2147483423;
	     true -> B3
	 end,
    C4 = if
	     B4 < 0 -> B4 + 2147483123;
	     true -> B4
	 end,

    R = (C1 * 0.0000000004656613022697297188506231646486) +
	(C2 * 0.0000000004656613100759859932486569933169) +
	(C3 * 0.0000000004656613360968421314794009471615) +
	(C4 * 0.0000000004656614011489951998100056779817),

    {R - trunc(R), {C1, C2, C3, C4}}.

%% uniform_s(N, State) -> {I, NewState}
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

-spec uniform_s(pos_integer(), ran()) -> {integer(), ran()}.

uniform_s(N, State0) when is_integer(N), N >= 1 ->
    {F, State1} = uniform_s(State0),
    {trunc(F * N) + 1, State1}.

%% generating another seed for multiple sequences
%% from a given seed changing the first two parameters

-spec next_sequence(ran()) -> ran().

next_sequence({A1, A2, A3, A4}) ->
    B1 = 46340 * (A1 rem 46341) - (41639 * (A1 div 46341)),
    B2 = 22000 * (A2 rem 97612) - (19543 * (A2 div 97612)),
    C1 = if
	     B1 < 0 -> B1 + 2147483579;
	     true -> B1
	 end,
    C2 = if
	     B2 < 0 -> B2 + 2147483543;
	     true -> B2
	 end,
    {C1, C2, A3, A4}.
