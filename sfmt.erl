%% Module sfmt (for SFMT-based RNG)

-module(sfmt).

-export([
	 rshift128/2,
	 lshift128/2,
	 do_recursion/4,
	 gen_rand_all/1,
	 gen_rand_list32/2,
	 period_certification/1,
	 get_idstring/0,
	 get_min_array_size/0,
	 func1/1,
	 func2/1,
	 init_gen_rand/2
	 ]).

-include("sfmt.hrl").

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

%%%% incomplete

%% period_certification(Int) ->
%%     [I0, I1, I2, I3 | Ileft] = Int,
%%     In0 = (I0 band ?PARITY1) bxor (I1 band ?PARITY2) bxor
%% 	(I2 band ?PARITY3) bxor	(I3 band ?PARITY4),
%%     In1 = (In0 bsr 16) bxor (In0 bsr 8) bxor
%% 	(In0 bsr 4) bxor (In0 bsr 2) bxor (In0 bsr 1),
%%     Inner = In1 band 1,
%%     case Inner of
%% 	1 ->
%% 	    Int;
%% 	0 ->
%% 	    1 band ?PARITY1

get_idstring() ->
    ?IDSTR.

get_min_array_size32() ->
    ?N32.

func1(X) ->
    ((X bxor (X bsr 27)) * 1664525) band ?BITMASK32.

func2(X) ->
    ((X bxor (X bsr 27)) * 1566083941) band ?BITMASK32.

init_gen_rand_rec1(?N32, Acc) ->
    list:reverse(Acc);
init_gen_rand_rec1(I, Acc) ->
    [H | _] = Acc,
    init_gen_rand_rec1(I + 1, 
		       [(1812433253 * (H bxor (H bsr 30))) + I | Acc]).

init_gen_rand(Seed) ->
    period_certification(init_gen_rand_rec1(1, [Seed])).
