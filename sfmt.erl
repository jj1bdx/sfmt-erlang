%% Module sfmt (for SFMT-based RNG)

-module(sfmt).

-export([
	 rshift128/2,
	 lshift128/2,
	 do_recursion/4,
	 gen_rand_all/1
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

gen_rand_all_rec1(Acc, Int, [], R1, R2) ->
    {Acc, Int, R1, R2};
gen_rand_all_rec1(Acc, Int, IntP, R1, R2) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | IntPN ] = IntP,
    NI = do_recursion([A0, A1, A2, A3],
		      [B0, B1, B2, B3],
		      R1,
		      R2),
    gen_rand_all_rec1([lists:reverse(NI)|Acc], IntN, IntPN, R2, NI).

gen_rand_all_rec2(Acc, [], _, _, _) ->
    Acc;
gen_rand_all_rec2(Acc, Int, NewIntP, R1, R2) ->
    [A0, A1, A2, A3 | IntN ] = Int,
    [B0, B1, B2, B3 | NewIntPN ] = NewIntP,
    NI = do_recursion([A0, A1, A2, A3],
		      [B0, B1, B2, B3],
		      R1,
		      R2),
    RevNI = lists:reverse(NI),
    gen_rand_all_rec1([RevNI|Acc], IntN, [RevNI|NewIntPN], R2, NI).

gen_rand_all(Int) ->
    [R23, R22, R21, R20, R13, R12, R11, R10 | _] = lists:reverse(Int),
    {Acc, IntB, R1B, R2B} = 
	gen_rand_all_rec1([], Int, lists:nthtail(?POS1 * 4, Int),
			  [R10, R11, R12, R13], [R20, R21, R22, R23]),
    [R1B0, R1B1, R1B2, R1B3] = R1B,
    [R2B0, R2B1, R2B2, R2B3] = R2B,
    NewIntP = lists:reverse(Acc),
    lists:reverse(
      gen_rand_all_rec2(Acc, IntB, NewIntP,
			[R1B0, R1B1, R1B2, R1B3], [R2B0, R2B1, R2B2, R2B3])).

    

