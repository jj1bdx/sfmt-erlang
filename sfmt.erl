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

-export([
	 gen_rand_all/1,
	 gen_rand_list32/2,
	 get_idstring/0,
	 get_min_array_size32/0,
	 init_gen_rand/1,
	 init_by_list32/1,
	 gen_rand32/2
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

rshift128(In, Shift) ->
    [I0, I1, I2, I3] = In,
    TH = (I3 bsl 32) bor (I2), 
    TL = (I1 bsl 32) bor (I0),
    OH = (TH bsr (Shift * 8)) band ?BITMASK64,
    OL = (TL bsr (Shift * 8) bor (TH bsl (64 - (Shift * 8))))
	band ?BITMASK64,
    [OL band ?BITMASK32, OL bsr 32, 
     OH band ?BITMASK32, OH bsr 32].

lshift128(In, Shift) ->
    [I0, I1, I2, I3] = In,
    TH = (I3 bsl 32) bor (I2), 
    TL = (I1 bsl 32) bor (I0),
    OL = (TL bsl (Shift * 8)) band ?BITMASK64,
    OH = (TH bsl (Shift * 8) bor (TL bsr (64 - (Shift * 8))))
	band ?BITMASK64,
    [OL band ?BITMASK32, OL bsr 32, 
     OH band ?BITMASK32, OH bsr 32].

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

%% still buggy - need to fix!

gen_rand_all_rec1(Acc, Int, [], R, Q) ->
    {Acc, Int, R, Q};
gen_rand_all_rec1(Acc, Int, IntP,
		  [R0, R1, R2, R3],
		  [Q0, Q1, Q2, Q3]) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | IntPN ] = IntP,
    [X0, X1, X2, X3] = do_recursion([A0, A1, A2, A3],
				   [B0, B1, B2, B3],
				   [R0, R1, R2, R3],
				   [Q0, Q1, Q2, Q3]),
    gen_rand_all_rec1([X3 | [X2 | [X1 | [X0 | Acc]]]],
		      IntN, IntPN,
		      [Q0, Q1, Q2, Q3],
		      [X0, X1, X2, X3]).

gen_rand_all_rec2(Acc, [], _, _, _) ->
    lists:reverse(Acc);
gen_rand_all_rec2(Acc, Int, IntP,
		  [R0, R1, R2, R3],
		  [Q0, Q1, Q2, Q3]) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | IntPN ] = IntP,
    [X0, X1, X2, X3] = do_recursion([A0, A1, A2, A3],
				   [B0, B1, B2, B3],
				   [R0, R1, R2, R3],
				   [Q0, Q1, Q2, Q3]),
    gen_rand_all_rec2([X3 | [X2 | [X1 | [X0 | Acc]]]],
		      IntN, 
		      IntPN ++ [X0, X1, X2, X3],
		      [Q0, Q1, Q2, Q3],
		      [X0, X1, X2, X3]).

%% returns internal state table
gen_rand_all(Int) ->
    [T3, T2, T1, T0, S3, S2, S1, S0 | _] = lists:reverse(Int),
    {Acc, IntB, U1, U2} = 
	gen_rand_all_rec1([], Int, lists:nthtail(?POS1 * 4, Int),
			  [S0, S1, S2, S3], [T0, T1, T2, T3]),
    NewIntP = lists:reverse(Acc),
    gen_rand_all_rec2(Acc, IntB, NewIntP, U1, U2).

gen_rand_list32_rec1(0, Acc, _, _, _, _) ->
    lists:reverse(Acc);
gen_rand_list32_rec1(K, Acc, Int, IntP,
		     [R0, R1, R2, R3],
		     [Q0, Q1, Q2, Q3]) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | IntPN ] = IntP,
    [X0, X1, X2, X3] = do_recursion([A0, A1, A2, A3],
				   [B0, B1, B2, B3],
				   [R0, R1, R2, R3],
				   [Q0, Q1, Q2, Q3]),
    gen_rand_list32_rec1(K - 4, 
			 [X3 | [X2 | [X1 | [X0 | Acc]]]],
			 IntN ++ [X0, X1, X2, X3],
			 IntPN ++ [X0, X1, X2, X3],
			 [Q0, Q1, Q2, Q3],
			 [X0, X1, X2, X3]).

gen_rand_list32(Size, Int) when Size >= ?N32, Size rem 4 =:= 0 ->
    A = gen_rand_all(Int),
    RevA = lists:reverse(A),
    [T3, T2, T1, T0, S3, S2, S1, S0 | _] = RevA,
    A2 = gen_rand_list32_rec1(
	   Size - ?N32,
	   RevA, A, lists:nthtail(?POS1 * 4, A),
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

get_idstring() ->
    ?IDSTR.

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

gen_rand32([], Int) ->
    Int2 = gen_rand_all(Int),
    [H|T] = Int2,
    {H, T, Int2};
gen_rand32(Randlist, Int) ->
    [H|T] = Randlist,
    {H, T, Int}.

