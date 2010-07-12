%% Module sfmt
%% SIMD-oriented Fast Mersenne Twister (SFMT)

%% Copyright (c) 2010 Kenji Rikitake. All rights reserved.
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
%%     * Neither the name of the Hiroshima University nor the names of
%%       its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written
%%       permission.
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
	 init_gen_rand/1,
	 init_by_list32/1,
	 randlist_to_intstate/1,
	 intstate_to_randlist/1,
	 gen_rand32/1,
	 seed0/0,
	 seed/0,
	 seed/1,
	 seed/3,
	 uniform/0,
	 uniform/1,
	 uniform_s/1,
	 uniform_s/2
	 ]).

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

%% SFMT calculation masks
-define(BITMASK32, 16#ffffffff).
-define(BITMASK64, 16#ffffffffffffffff).

%% internal state format: 
%% list of 32-bit unsigned ints,
%% with the following format of
%% little-endian 128-bit format
%% e.g., a 128-bit X = [X0, X1, X2, X3]
%% where in C
%% /* begin */
%% union X {
%% 	uint32_t u[4];
%% };
%% /* end */
%% and 128-bit list is a flat concatenation
%% of 128-bit number
%%
%% @type w128() = [integer(), integer(), integer(), integer()].
%% @type intstate() = [w128()].

%% @spec rshift128(w128(), integer()) -> w128().
%% @doc SIMD 128-bit right shift simulation for little endian SIMD
%%      of Shift*8 bits
%% @note no longer required

%% @spec lshift128(w128(), integer()) -> w128().
%% @doc SIMD 128-bit left shift simulation for little endian SIMD
%%      of Shift*8 bits
%% @note no longer required

%% @spec do_recursion(w128(), w128(), w128(), w128()) -> w128().
%% @doc the recursion formula operation of SFMT
%% @note no longer required

%% Recursion algorithm for gen_rand_all and gen_rand_list32:
%%
%% a[]: output array (of S w128() elements)
%% i[]: internal state (of N w128() elements)
%% (For gen_rand_all, S =:= N)
%% (For gen_rand_list32, S >= N)
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
%% Use the last N w128() elements of a[] for the new internal state ni[]
%% i.e., 
%% ni[0] = a[S-N], ni[1] = a[S-N+1], ... ni[N-1] = a[S-1].

%% @spec gen_rand_all(intstate()) - > intstate().
%% @doc filling the internal state array with SFMT PRNG

gen_rand_all(_) -> error_nifnized.

%% @spec gen_rand_list32(integer(), intstate()) - > {[integer()], intstate()}.
%% @doc generating the 32-bit integer list of PRNG,
%%      where length of the list is Size
%%      with the updated internal state

gen_rand_list32(_, _) -> error_nifnized.

%% @spec get_idstring() -> string().
%% @doc returns SFMT identification string
%% @note NIFnized

get_idstring() -> error_nifnized.

%% @spec get_min_array_size32() -> integer().
%% @doc returns array size of internal state
%% @note NIFnized

get_min_array_size32() -> error_nifnized.

%% @spec init_gen_rand(integer()) -> intstate().
%% @doc generates an internal state from an integer seed
%% @note NIFnized

init_gen_rand(_) -> error_nifnized.

%% @spec init_by_list32([integer()]) -> intstate().
%% @doc generates an internal state from a list of 32-bit integers
%% @note NIFnized

init_by_list32(_) -> error_nifnized.

%%

randlist_to_intstate(_) -> error_nifnized.

intstate_to_randlist(_) -> error_nifnized.

gen_rand32(_) -> error_nifnized.

%% Note: ran_sfmt() -> {integer(), intstate()}

%% @spec gen_rand32(ran_sfmt()|intstate) -> {integer(), ran_sfmt()}.
%% @doc generates a 32-bit random number from the given ran_sfmt()
%% @note NIFnized

%% compatible funtions to the random module in stdlib

%% entry in the process dictionary
-define(PDIC_SEED, sfmt_seed).
%% (1 / ((2 ^ 32) - 1)) (for [0, 1]-interval conversion)
-define(FLOAT_CONST, (1.0/4294967295.0)).

%% @spec seed0() -> ran_sfmt()
%% @doc Returns the default internal state

seed0() ->
    I = init_gen_rand(1234),
    {?N32, I}.

%% @spec seed() -> ran_sfmt()
%% @doc Initialize the process dictionary with seed0/0

seed() ->
    Seed = seed0(),
    case put(?PDIC_SEED, Seed) of
	undefined -> Seed;
	Old ->       Old
    end.

%% @spec seed(integer()) -> ran_sfmt()
%% @doc Puts the seed computed from the given integer list by init_gen_rand/1
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state

seed(N) when is_integer(N) ->
    I = init_gen_rand(N),
    RS = {?N32, I},
    put(?PDIC_SEED, RS);

%% @spec seed([integer()]) -> ran_sfmt()
%% @doc Puts the seed computed from the given integer list by init_by_list32/1
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state

seed(L) when is_list(L), is_integer(hd(L)) ->
    I = init_by_list32(L),
    RS = {?N32, I},
    put(?PDIC_SEED, RS);

%% @spec seed({integer(), integer(), integer()}) -> ran_sfmt()
%% @doc Puts the seed computed from given three integers as a tuple
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state

seed({A1, A2, A3}) ->
    seed([A1, A2, A3]).

%% @spec seed(integer(), integer(), integer()) -> ran_sfmt()
%% @doc Puts the seed computed from given three integers
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state

seed(A1, A2, A3) ->
    seed([A1, A2, A3]).

%% @spec uniform() -> float()
%% @doc Returns a uniformly-distributed float random number X
%%      where (X >= 0.0) and (X =< 1.0)
%%      and updates the internal state in the process dictionary

uniform() -> 
    % if random number list doesn't exist
    % the corresponding internal state must be initialized
    RS = case get(?PDIC_SEED) of
		   undefined ->
		       seed0();
		   Val -> Val
	       end,
    {X, NRS} = gen_rand32(RS),
    % divided by 2^32 - 1
    put(?PDIC_SEED, NRS),
    X * ?FLOAT_CONST.

%% @spec uniform(N) -> integer()
%% @doc Returns a uniformly-distributed integer random number X
%%      where (X >= 1) and (X =< N)
%%      and updates the internal state in the process dictionary

uniform(N) when N >= 1 ->
    trunc(uniform() * N) + 1.

%% @spec uniform(ran_sfmt()) -> float()
%% @doc With a given state,
%%      Returns a uniformly-distributed float random number X
%%      where (X >= 0.0) and (X =< 1.0)
%%      and a new state

uniform_s(RS) ->
    {X, NRS} = gen_rand32(RS),
    {X * ?FLOAT_CONST, NRS}.

%% @spec uniform(integer(), ran_sfmt()) -> (integer(), ran_sfmt()} 
%%      Returns a uniformly-distributed integer random number X
%%      where (X >= 1) and (X =< N)
%%      and a new state

uniform_s(N, RS) ->
    {X, NRS} = gen_rand32(RS),
    {trunc(X * ?FLOAT_CONST * N) + 1, NRS}.
    
%% On-load callback

load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, priv);
		  Path ->
		      Path
	      end,		  
    erlang:load_nif(filename:join(PrivDir, sfmt_nif),0).

%% end of the module    
