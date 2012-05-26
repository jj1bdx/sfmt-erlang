%% Module sfmt_pure
%% SIMD-oriented Fast Mersenne Twister (SFMT)
%% in pure Erlang

%% Copyright (c) 2010-2012 Kenji Rikitake and Kyoto University.
%% All rights reserved.
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

-module(sfmt_pure).

-export([
	 gen_rand_all/1,
	 gen_rand_list32/2,
	 get_idstring/0,
	 get_min_array_size32/0,
	 init_gen_rand/1,
	 init_by_list32/1,
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

-type w128() :: [integer()].
-type intstate() :: [w128()].
-type ran_sfmt() :: {[integer()], intstate()}.

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

%% @doc SIMD 128-bit right shift simulation for little endian SIMD
%%      of Shift*8 bits
-spec rshift128(w128(), integer()) -> w128().

rshift128(In, Shift) ->
    [I0, I1, I2, I3] = In,
    TH = (I3 bsl 32) bor (I2), 
    TL = (I1 bsl 32) bor (I0),
    OH = (TH bsr (Shift * 8)) band ?BITMASK64,
    OL = (TL bsr (Shift * 8) bor (TH bsl (64 - (Shift * 8))))
	band ?BITMASK64,
    [OL band ?BITMASK32, OL bsr 32, 
     OH band ?BITMASK32, OH bsr 32].

%% @doc SIMD 128-bit left shift simulation for little endian SIMD
%%      of Shift*8 bits
-spec lshift128(w128(), integer()) -> w128().

lshift128(In, Shift) ->
    [I0, I1, I2, I3] = In,
    TH = (I3 bsl 32) bor (I2), 
    TL = (I1 bsl 32) bor (I0),
    OL = (TL bsl (Shift * 8)) band ?BITMASK64,
    OH = (TH bsl (Shift * 8) bor (TL bsr (64 - (Shift * 8))))
	band ?BITMASK64,
    [OL band ?BITMASK32, OL bsr 32, 
     OH band ?BITMASK32, OH bsr 32].

%% @doc the recursion formula operation of SFMT
-spec do_recursion(w128(), w128(), w128(), w128()) -> w128().

do_recursion(A, B, C, D) ->
    [A0, A1, A2, A3] = A,
    [B0, B1, B2, B3] = B,
    % [C0, C1, C2, C3] = C,
    [D0, D1, D2, D3] = D,
    [X0, X1, X2, X3] = lshift128(A, ?SL2),
    [Y0, Y1, Y2, Y3] = rshift128(C, ?SR2),
    [
     A0 bxor X0 bxor ((B0 bsr ?SR1) band ?MSK1) bxor Y0
        bxor ((D0 bsl ?SL1) band ?BITMASK32),
     A1 bxor X1 bxor ((B1 bsr ?SR1) band ?MSK2) bxor Y1
        bxor ((D1 bsl ?SL1) band ?BITMASK32),
     A2 bxor X2 bxor ((B2 bsr ?SR1) band ?MSK3) bxor Y2
        bxor ((D2 bsl ?SL1) band ?BITMASK32),
     A3 bxor X3 bxor ((B3 bsr ?SR1) band ?MSK4) bxor Y3
        bxor ((D3 bsl ?SL1) band ?BITMASK32)
     ].

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

%% To avoid appending two lists, 
%% a and b of r(a, b, c, d) form ring buffers
%% (e.g., Int and AccInt, IntP and AccIntP,
%%  of gen_rand_recursion/8)
%% This makes the algorithm simpler and faster

gen_rand_recursion(0, Acc, _, _, _, _, _, _) ->
    lists:reverse(Acc);
gen_rand_recursion(K, Acc, Int, AccInt, [], AccIntP, R, Q) ->
    gen_rand_recursion(K, Acc, Int, AccInt,
		       lists:reverse(AccIntP),
		       [],
		       R, Q);
gen_rand_recursion(K, Acc, [], AccInt, IntP, AccIntP, R, Q) ->
    gen_rand_recursion(K, Acc, 
		       lists:reverse(AccInt),
		       [],
		       IntP, AccIntP, R, Q);
gen_rand_recursion(K, Acc, Int, 
		   AccInt, IntP, AccIntP,
		   [R0, R1, R2, R3],
		   [Q0, Q1, Q2, Q3]) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | IntPN ] = IntP,
    [X0, X1, X2, X3] = do_recursion([A0, A1, A2, A3],
				    [B0, B1, B2, B3],
				    [R0, R1, R2, R3],
				    [Q0, Q1, Q2, Q3]),
    gen_rand_recursion(K - 4,
		       [X3 | [X2 | [X1 | [X0 | Acc]]]],
		       IntN, 
		       [X3 | [X2 | [X1 | [X0 | AccInt]]]],
		       IntPN,
		       [X3 | [X2 | [X1 | [X0 | AccIntP]]]],
		       [Q0, Q1, Q2, Q3],
		       [X0, X1, X2, X3]).

%% @doc filling the internal state array with SFMT PRNG
-spec gen_rand_all(intstate()) -> intstate().

gen_rand_all(Int) ->
    [T3, T2, T1, T0, S3, S2, S1, S0 | _] = lists:reverse(Int),
    gen_rand_recursion(?N32, [], Int, [],
		       lists:nthtail(?POS1 * 4, Int), [],
		       [S0, S1, S2, S3], [T0, T1, T2, T3]).

%% @doc generating the 32-bit integer list of PRNG,
%%      where length of the list is Size
%%      with the updated internal state
-spec gen_rand_list32(integer(), intstate()) -> {[integer()], intstate()}.

gen_rand_list32(Size, Int) when Size >= ?N32, Size rem 4 =:= 0 ->
    [T3, T2, T1, T0, S3, S2, S1, S0 | _] = lists:reverse(Int),
    A2 = gen_rand_recursion(Size, [], Int, [],
			    lists:nthtail(?POS1 * 4, Int), [],
			    [S0, S1, S2, S3], [T0, T1, T2, T3]),
    Int2 = lists:nthtail(Size - ?N32, A2),
    {A2, Int2}.

period_modification_rec1(Parity, I) ->
    period_modification_rec1(0, Parity, I).

period_modification_rec1(true, _, I) ->
    {I, true};
period_modification_rec1(32, _, I) ->
    {I, false};
period_modification_rec1(X, Parity, I) ->
    Work = 1 bsl X,
    case (Work band Parity =/= 0) of
	true -> 
	    period_modification_rec1(true, Parity, I bxor Work);
	false ->
	    period_modification_rec1(X + 1, Parity, I)
    end.

period_modification(Int) ->
    [I0, I1, I2, I3 | IR ] = Int,
    {NI0, F0} = period_modification_rec1(?PARITY1, I0),
    {NI1, F1} = period_modification_rec1(?PARITY2, I1),
    {NI2, F2} = period_modification_rec1(?PARITY3, I2),
    {NI3, F3} = period_modification_rec1(?PARITY4, I3),
    if
	F0 =:= true ->
	    [NI0, I1, I2, I3 | IR];
	F1 =:= true ->
	    [I0, NI1, I2, I3 | IR];
	F2 =:= true ->
	    [I0, I1, NI2, I3 | IR];
	F3 =:= true ->
	    [I0, I1, I2, NI3 | IR];
	true ->
	    Int
    end.

period_certification(Int) ->
    [I0, I1, I2, I3 | _ ] = Int,
    In0 = (I0 band ?PARITY1) bxor 
	(I1 band ?PARITY2) bxor
	(I2 band ?PARITY3) bxor	
	(I3 band ?PARITY4),
    In1 = In0 bxor (In0 bsr 16),
    In2 = In1 bxor (In1 bsr 8),
    In3 = In2 bxor (In2 bsr 4),
    In4 = In3 bxor (In3 bsr 2),
    In5 = In4 bxor (In4 bsr 1),
    Inner = In5 band 1,
    case Inner of
	1 ->
	    Int;
	0 ->
	    period_modification(Int)
    end.

%% @doc returns SFMT identification string
-spec get_idstring() -> string().

get_idstring() ->
    ?IDSTR.

%% @doc returns array size of internal state
-spec get_min_array_size32() -> integer().

get_min_array_size32() ->
    ?N32.

func1(X) ->
    ((X bxor (X bsr 27)) * 1664525) band ?BITMASK32.

func2(X) ->
    ((X bxor (X bsr 27)) * 1566083941) band ?BITMASK32.

init_gen_rand_rec1(?N32, Acc) ->
    lists:reverse(Acc);
init_gen_rand_rec1(I, Acc) ->
    [H | _] = Acc,
    init_gen_rand_rec1(
      I + 1, 
      [((1812433253 * (H bxor (H bsr 30))) + I) band ?BITMASK32 | Acc]).

%% @doc generates an internal state from an integer seed
-spec init_gen_rand(integer()) -> intstate().

init_gen_rand(Seed) ->
    period_certification(init_gen_rand_rec1(1, [Seed])).

init_by_list32_rec1(0, I, _, A) ->
    {I, A};
init_by_list32_rec1(K, I, [], A) ->
    R = func1(array:get(I, A) bxor
		  array:get((I + ?MID) rem ?N32, A) bxor
		  array:get((I + ?N32 - 1) rem ?N32, A)),
    A2 = array:set((I + ?MID) rem ?N32,
		   (array:get((I + ?MID) rem ?N32, A) + R) band ?BITMASK32,
		   A),
    R2 = (R + I) band ?BITMASK32,
    A3 = array:set((I + ?MID + ?LAG) rem ?N32,
		 (array:get((I + ?MID + ?LAG) rem ?N32, A2) + R2) band ?BITMASK32,
		 A2),
    A4 = array:set(I, R2, A3),
    I2 = (I + 1) rem ?N32,
    init_by_list32_rec1(K - 1, I2, [], A4);
init_by_list32_rec1(K, I, Key, A) ->
    R = func1(array:get(I, A) bxor
		  array:get((I + ?MID) rem ?N32, A) bxor
		  array:get((I + ?N32 - 1) rem ?N32, A)),
    A2 = array:set((I + ?MID) rem ?N32,
		   (array:get((I + ?MID) rem ?N32, A) + R) band ?BITMASK32,
		   A),
    [H|T] = Key,
    R2 = (R + H + I) band ?BITMASK32,
    A3 = array:set((I + ?MID + ?LAG) rem ?N32,
		   (array:get((I + ?MID + ?LAG) rem ?N32, A2) + R2) band ?BITMASK32,
		   A2),
    A4 = array:set(I, R2, A3),
    I2 = (I + 1) rem ?N32,
    init_by_list32_rec1(K - 1, I2, T, A4).

init_by_list32_rec2(0, _, A) ->
    A;
init_by_list32_rec2(K, I, A) ->
    R = func2((array:get(I, A) +
		  array:get((I + ?MID) rem ?N32, A) +
		  array:get((I + ?N32 - 1) rem ?N32, A)) band ?BITMASK32),
    A2 = array:set((I + ?MID) rem ?N32,
		   (array:get((I + ?MID) rem ?N32, A) bxor R),
		   A),
    R2 = (R - I) band ?BITMASK32,
    A3 = array:set((I + ?MID + ?LAG) rem ?N32,
		   (array:get((I + ?MID + ?LAG) rem ?N32, A2) bxor R2),
		   A2),
    A4 = array:set(I, R2, A3),
    I2 = (I + 1) rem ?N32,
    init_by_list32_rec2(K - 1, I2, A4).

%% @doc generates an internal state from a list of 32-bit integers
-spec init_by_list32([integer()]) -> intstate().

init_by_list32(Key) ->
    Keylength = length(Key),
    
    A = array:new(?N32, {default, 16#8b8b8b8b}),

    Count =
	if
	    Keylength + 1 > ?N32 ->
		Keylength + 1;
	    true ->
		?N32
	end,
    R = func1(array:get(0, A) bxor
		  array:get(?MID, A) bxor
		  array:get(?N32 - 1, A)),
    A2 = array:set(?MID,
		   (array:get(?MID, A) + R) band ?BITMASK32,
		   A),
    R2 = (R + Keylength) band ?BITMASK32,
    A3 = array:set(?MID + ?LAG,
		   (array:get(?MID + ?LAG, A2) + R2) band ?BITMASK32,
		   A2),
    A4 = array:set(0, R2, A3),

    Count1 = Count - 1,
    {I1, A5} = init_by_list32_rec1(Count1, 1, Key, A4),

    period_certification(
      array:to_list(
	init_by_list32_rec2(?N32, I1, A5))).

%%%%
%% functions from here will not be NIFnized
%%%%

%% Note: ran_sfmt() -> {[integer()], intstate()}
%% intstate() content may be changed by NIFnization

%% @doc generates a 32-bit random number from the given ran_sfmt()
-spec gen_rand32(intstate()) -> {integer(), ran_sfmt()};
                (ran_sfmt()) -> {integer(), ran_sfmt()}.

gen_rand32(L) when is_list(L), length(L) =:= ?N32 ->
    % when intstate() is directly passed
    [H|T] = L,
    {H, {T, L}};
gen_rand32({[], I}) ->
    I2 = gen_rand_all(I),
    % this operation is intstate() type dependent
    [H|T] = I2,
    {H, {T, I2}};
gen_rand32({R, I}) ->
    [H|T] = R,
    {H, {T, I}}.

%% compatible funtions to the random module in stdlib

%% entry in the process dictionary
-define(PDIC_SEED, sfmt_seed).
%% (1 / ((2 ^ 32) - 1)) (for [0, 1]-interval conversion)
-define(FLOAT_CONST, (1.0/4294967295.0)).

%% @spec seed0() -> ran_sfmt()
%% @doc Returns the default internal state

seed0() ->
    I = init_gen_rand(1234),
    % this operation is intstate() type dependent
    R = I,
    {R, I}.

%% @spec seed() -> ran_sfmt()
%% @doc Initialize the process dictionary with seed0/0,
%%      and return seed0/0 value.

seed() ->
    RS = seed0(),
    put(?PDIC_SEED, RS),
    RS.

%% @spec seed(integer()) -> ran_sfmt()
%% @doc Puts the seed computed from the given integer
%%      as a single-element list by init_by_list32/1
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state

seed(N) when is_integer(N) ->
    I = init_by_list32([N]),
    % this operation is intstate() type dependent
    R = I,
    RS = {R, I},
    put(?PDIC_SEED, RS);

%% @spec seed([integer()]) -> ran_sfmt()
%% @doc Puts the seed computed from the given integer list by init_by_list32/1
%%      and puts the internal state into the process dictionary
%%      and initializes the random number list with the internal state
%%      and returns the old internal state

seed(L) when is_list(L), is_integer(hd(L)) ->
    I = init_by_list32(L),
    % this operation is intstate() type dependent
    R = I,
    RS = {R, I},
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
%%      where `(X >= 0.0)' and `(X =< 1.0)'
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
%%      where `(X >= 1)' and `(X =< N)'
%%      and updates the internal state in the process dictionary

uniform(N) when N >= 1 ->
    trunc(uniform() * N) + 1.

%% @spec uniform_s(ran_sfmt()) -> float()
%% @doc With a given state,
%%      Returns a uniformly-distributed float random number X
%%      where `(X >= 0.0)' and `(X =< 1.0)'
%%      and a new state

uniform_s(RS) ->
    {X, NRS} = gen_rand32(RS),
    {X * ?FLOAT_CONST, NRS}.

%%      Returns a uniformly-distributed integer random number X
%%      where (X >= 1) and (X =< N)
%%      and a new state
-spec uniform_s(integer(), ran_sfmt()) -> {integer(), ran_sfmt()}.

uniform_s(N, RS) ->
    {X, NRS} = gen_rand32(RS),
    {trunc(X * ?FLOAT_CONST * N) + 1, NRS}.
    
%% end of the module    
