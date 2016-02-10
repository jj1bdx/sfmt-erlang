%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @author Mutsuo Saito
%% @author Makoto Matsumoto
%% @author Dan Gudmundsson
%% @doc SIMD-oriented Fast Mersenne Twister (SFMT).
%% The module provides skeleton functions for the NIFs
%% and the interface functions.
%% The module defines a PRNG of period ((2^19937) - 1).
%% @reference <a href="http://github.com/jj1bdx/sfmt-erlang">GitHub page
%% for sfmt-erlang</a>
%% @copyright 2010-2016 Kenji Rikitake and Kyoto University.
%% Copyright (c) 2006, 2007 Mutsuo Saito, Makoto Matsumoto and
%% Hiroshima University.

%% Copyright (c) 2010-2016 Kenji Rikitake and Kyoto University. All rights
%% reserved.
%%
%% Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
%% University. All rights reserved.
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
%%     * Neither the names of the Hiroshima University and the Kyoto
%%       University nor the names of its contributors may be used to
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

-module(sfmt).

-on_load(load_nif/0).

-export([
	 gen_rand_all/1,
	 gen_rand_list32/2,
	 get_idstring/0,
	 get_min_array_size32/0,
	 get_lib_refc/0,
	 init_gen_rand/1,
	 init_by_list32/1,
	 gen_rand32/1,
	 seed0/0,
	 seed/0,
	 seed/1,
	 seed/3,
     uint32_to_float/1,
	 uniform/0,
	 uniform/1,
	 uniform_s/1,
	 uniform_s/2
	 ]).

%% Internal conversion between the internal state table
%% and the external representation of randlist
%% (i.e., a list of N32 integer elements)
-export([randlist_to_intstate/1, intstate_to_randlist/1]).

%% SFMT period parameters
%% details on SFMT-1.3.3 source code
%%
%% Mersenne Exponent. The period of the sequence
%%  is a multiple of 2^MEXP-1.
-define(MEXP, 19937).
%% SFMT generator has an internal state array of 128-bit integers,
%% and N is its size.
%% -define(N, ((?MEXP div 128) + 1)).
-define(N, 156).
%% N32 is the size of internal state array when regarded as an array
%% of 32-bit integers.
%% -define(N32, (?N * 4)).
-define(N32, 624).
%% for init_by_list32/1:
%% LAG =
%% 	if
%% 	    ?N32 >= 623 ->
%% 		11;
%% 	    ?N32 >= 68 ->
%% 		7;
%% 	    ?N32 >= 39 ->
%% 		5;
%% 	    ?N32 ->
%% 		3
%% 	end,
%% MID = (?N32 - LAG) div 2
-define(LAG, 11).
-define(MID, 306).
%% the pick up position of the array.
-define(POS1, 122).
%% the parameter of shift left as four 32-bit registers.
-define(SL1, 18).
%% the parameter of shift left as one 128-bit register.
%% The 128-bit integer is shifted by (SL2 * 8) bits.
-define(SL2, 1).
%% the parameter of shift right as four 32-bit registers.
-define(SR1, 11).
%% the parameter of shift right as one 128-bit register.
%% The 128-bit integer is shifted by (SL2 * 8) bits.
-define(SR2, 1).
%% A bitmask, used in the recursion.  These parameters are introduced
%% to break symmetry of SIMD.
-define(MSK1, 16#dfffffef).
-define(MSK2, 16#ddfecb7f).
-define(MSK3, 16#bffaffff).
-define(MSK4, 16#bffffff6).
%% These definitions are part of a 128-bit period certification vector.
-define(PARITY1, 16#00000001).
-define(PARITY2, 16#00000000).
-define(PARITY3, 16#00000000).
-define(PARITY4, 16#13c9e684).
%% identification string for the algorithm
-define(IDSTR, "SFMT-19937:122-18-1-11-1:dfffffef-ddfecb7f-bffaffff-bffffff6").

%% NIF version number (on the load_info argument of load_nif/2)
-define(NIF_LOAD_INFO, 101).

%% NIF loading error macros
%% (see the crypto module's crypto.c)
-define(nif_stub, nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

%% @type w128() = [integer()].
%% An 128-bit integer represented by four 32-bit unsigned integers.
%% Note: the number of elements is four (4).

%% -type w128() :: [integer()].

%% @type randlist() = [integer()].
%% A list of N 128-bit integers for the portable representation of
%% the internal state table,
%% represented as multiple concatenation of four 32-bit unsigned integers.
%% Note: the number of elements is the same as N32 (624).

-type randlist() :: [integer()].

%% @type intstate() = binary().
%% A binary representation of N 128-bit integers for the internal state table,
%% represented as multiple concatenation of four 32-bit unsigned integers.
%% Note: the number of the binary bytes is the same as N32*4 (2496).
%% Note well: intstate() is <em>not</em> portable among different host architectures
%% due to the endianness issue.
%% Use randlist() for transferring the internal state between the processes.
%%
%% The Internal state format in details:
%% list of 32-bit unsigned ints,
%% with the following format of
%% little-endian 128-bit format
%% e.g., a 128-bit <code>X = [X0, X1, X2, X3]</code>
%% where in C
%% <pre><code>
%% /* begin */
%% union X {
%% 	uint32_t u[4];
%% };
%% /* end */
%% </code></pre>
%% and a 128-bit list is a flat concatenation
%% of 128-bit numbers.

-type intstate() :: binary().

%% @doc filling the internal state array with SFMT PRNG.
%%
%% Recursion algorithm for gen_rand_all and gen_rand_list32:
%%
%% <pre><code>
%% a[]: output array (of S w128() elements)
%% i[]: internal state (of N w128() elements)
%% (For gen_rand_all, S =:= N)
%% (For gen_rand_list32, S &gt;= N)
%% r(a, b, c, d): do_recursion/4 function (of 4 w128() arguments)
%% (The indexes are in C notation ([0 ... J-1] for J elements))
%%
%% a[0] = r(i[0], i[POS1],   i[N-2], i[N-1]);
%% a[1] = r(i[1], i[POS1+1], i[N-1], a[0]);
%% a[2] = r(i[2], i[POS1+2], a[0],   a[1]);
%% ...
%% a[(N-POS1)-1] = r(i[(N-POS1)-1], i[N-1], a[(N-POS1)-3], a[(N-POS1)-2]);
%% a[(N-POS1)]   = r(i[(N-POS1)],   a[0],   a[(N-POS1)-2], a[(N-POS1)-1]);
%% a[(N-POS1)+1] = r(i[(N-POS1)+1], a[1],   a[(N-POS1)-1], a[(N-POS1)]);
%% ...
%% a[N-1] = r(i[N-1], a[POS1-1], a[N-3], a[N-2]);
%% % assignments from here only applicable to gen_rand_list32
%% a[N]   = r(a[0],   a[POS1],   a[N-2], a[N-1]);
%% a[N+1] = r(a[1],   a[POS1+1], a[N-1], a[N]);
%% ...
%% a[X]   = r(a[X-N], a[X-(N-POS1)], a[X-1], a[X-2]);
%% ...
%% a[S-1] = r(a[(S-N)-1], a[S-(N-POS1)-1], a[S-2], a[S-3]);
%%
%% Use the last N <code>w128()</code> elements of <code>a[]</code>
%% for the new internal state <code>ni[]</code>,
%% i.e.,
%% ni[0] = a[S-N], ni[1] = a[S-N+1], ... ni[N-1] = a[S-1].
%% </code></pre>

-spec gen_rand_all(Intstate::intstate()) -> intstate().

gen_rand_all(_) -> ?nif_stub.

%% @doc generating the 32-bit integer list of PRNG,
%%      where length of the list is Size
%%      with the updated internal state.

-spec gen_rand_list32(Size::integer(), Intstate::intstate()) -> {[integer()], intstate()}.

gen_rand_list32(_, _) -> ?nif_stub.

%% @doc returns SFMT identification string.
%% (Note: NIFnized)

-spec get_idstring() -> string().

get_idstring() -> ?nif_stub.

%% @doc returns array size of internal state.
%% (Note: NIFnized)

-spec get_min_array_size32() -> integer().

get_min_array_size32() -> ?nif_stub.

%% @doc returns NIF library reference count.
%% (Note: NIFnized)

-spec get_lib_refc() -> integer().

get_lib_refc() -> ?nif_stub.

%% @doc generates an internal state from an integer seed.
%% (Note: NIFnized)

-spec init_gen_rand(Seed::integer()) -> intstate().

init_gen_rand(_) -> ?nif_stub.

%% @doc generates an internal state from a list of 32-bit integers.
%% (Note: NIFnized)

-spec init_by_list32(Seedlist::[integer()]) -> intstate().

init_by_list32(_) -> ?nif_stub.

%% @doc converts a valid internal state from a list of N32 32-bit integers.
%% (Note: NIFnized)

-spec randlist_to_intstate(Randlist::randlist()) -> intstate().

randlist_to_intstate(_) -> ?nif_stub.

%% @doc converts an internal state table to a list of N32 32-bit integers.
%% (Note: NIFnized)

-spec intstate_to_randlist(Intstate::intstate()) -> randlist().

intstate_to_randlist(_) -> ?nif_stub.

%% @doc Converts a 32-bit unsigned integer N to
%%      a float number X
%%      where 0.0 &lt; X &lt; 1.0;
%%      X = (N + 0.5) * (1.0/4294967296).

%% (1 / ((2 ^ 32)) (for (0, 1)-interval conversion)
-define(FLOAT_CONST, (1.0/4294967296.0)).

-spec uint32_to_float([0..4294967295]) -> float().

uint32_to_float(N) when is_integer(N) ->
        ((N + 0.5) * ?FLOAT_CONST).

%% @type ran_sfmt() = {non_neg_integer(), intstate()}.
%% This type represents an internal state for random number generator.

-type ran_sfmt() :: {non_neg_integer(), intstate()}.

%% @doc generates a 32-bit random number from the given ran_sfmt().
%% (Note: once nifnized, but the speed of list-based code is faster)

-spec gen_rand32(RS::ran_sfmt()|intstate()) -> {integer(), ran_sfmt()}.

gen_rand32({[H|T], I}) ->
    {H, {T, I}};
gen_rand32({_, I}) ->
    gen_rand32(I);
gen_rand32(I) when is_binary(I) ->
    I2 = gen_rand_all(I),
    [H|T] = intstate_to_randlist(I2),
    {H, {T, I2}}.

%% compatible funtions to the random module in stdlib

%% entry in the process dictionary
-define(PDIC_SEED, sfmt_seed).

%% @doc Returns the default internal state.

-spec seed0() -> ran_sfmt().

seed0() ->
    I = init_gen_rand(1234),
    {?N32, I}.

%% @doc Initialize the process dictionary with seed0/0.

-spec seed() -> ran_sfmt().

seed() ->
    Seed = seed0(),
    case put(?PDIC_SEED, Seed) of
	undefined -> Seed;
	Old ->       Old
    end.

%% @doc Puts the seed computed from the given integer
%%      as a single-element list by init_by_list32/1
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state.

-spec seed(integer()|[integer()]|
          {integer(), integer(), integer()}) -> ran_sfmt().

seed(N) when is_integer(N) ->
    I = init_by_list32([N]),
    RS = {?N32, I},
    put(?PDIC_SEED, RS);

%% @doc Puts the seed computed from the given integer list by init_by_list32/1
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state.

seed(L) when is_list(L), is_integer(hd(L)) ->
    I = init_by_list32(L),
    RS = {?N32, I},
    put(?PDIC_SEED, RS);

%% @doc Puts the seed computed from given three integers as a tuple
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state.

seed({A1, A2, A3}) ->
    seed([A1, A2, A3]).

%% @doc Puts the seed computed from given three integers
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state.

-spec seed(integer(), integer(), integer()) -> ran_sfmt().

seed(A1, A2, A3) ->
    seed([A1, A2, A3]).

%% @doc Returns a uniformly-distributed float random number X
%%      where X is in the range of 0.0 &lt; X &lt; 1.0
%%      and updates the internal state in the process dictionary.

-spec uniform() -> float().

uniform() ->
    % if random number list doesn't exist
    % the corresponding internal state must be initialized
    RS = case get(?PDIC_SEED) of
		   undefined ->
		       seed0();
		   Val -> Val
	       end,
    {X, NRS} = gen_rand32(RS),
    % convert to (0.0, 1.0) interval
    put(?PDIC_SEED, NRS),
    uint32_to_float(X).

%% @doc Returns a uniformly-distributed integer random number X
%%      where X is in the range of 1 =&lt; X =&lt; N
%%      and updates the internal state in the process dictionary.
%% Note: the pigeonhole principle is applied to the output;
%% this function will retry generating the base 32-bit unsigned integer
%% number sequence if the result does not guarantee the
%% equally-probabilistic results between the given range of integers.

-define(TWOPOW32, 16#100000000).

-spec uniform(integer()) -> integer().

uniform(N) when is_integer(N), N >= 1, N < ?TWOPOW32 ->
    % if random number list doesn't exist
    % the corresponding internal state must be initialized
    RS = case get(?PDIC_SEED) of
           undefined ->
               seed0();
           Val -> Val
           end,
    {V, NRS} = uniform_s(N, RS),
    put(?PDIC_SEED, NRS),
    V.

%% @doc With a given state,
%%      Returns a uniformly-distributed float random number X
%%      and a new state
%%      where X is in the range of 0.0 &lt; X &lt; 1.0.

-spec uniform_s(RS::ran_sfmt()) -> {float(), ran_sfmt()}.

uniform_s(RS) ->
    {X, NRS} = gen_rand32(RS),
    {uint32_to_float(X), NRS}.

%% @doc Returns a uniformly-distributed integer random number X
%%      and a new state
%%      where X is in the range of 1 =&lt; X =&lt; N.
%% Note: the pigeonhole principle is applied to the output;
%% this function will retry generating the base 32-bit unsigned integer
%% number sequence if the result does not guarantee the
%% equally-probabilistic results between the given range of integers.

-spec uniform_s(integer(), ran_sfmt()) -> {integer(), ran_sfmt()}.

uniform_s(Max, RS) when is_integer(Max), Max >= 1 ->
    Limit = ?TWOPOW32 - (?TWOPOW32 rem Max),
    uniform_s(Max, Limit, RS).

uniform_s(M, L, RS) ->
    {V, NRS} = gen_rand32(RS),
    case V < L of
        true -> {(V rem M) + 1, NRS};
        false -> uniform_s(M, L, NRS)
    end.

%% On-load callback

%% @doc Loading NIF shared library file, used at the on-load callback.

load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    erlang:load_nif(filename:join(PrivDir, "sfmt_nif"), ?NIF_LOAD_INFO).

%% end of the module
