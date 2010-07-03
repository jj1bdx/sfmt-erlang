%% Module sfmt (for SFMT-based RNG)

-module(sfmt).

-export([
	 rshift128/2,
	 lshift128/2,
	 do_recursion/4
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
     A1 bxor X1 bxor ((B1 bsr ?SR1) band ?MSK1) bxor Y1
        bxor ((D1 bsl ?SL1) band ?BITMASK32),
     A2 bxor X2 bxor ((B2 bsr ?SR1) band ?MSK1) bxor Y2
        bxor ((D2 bsl ?SL1) band ?BITMASK32),
     A3 bxor X3 bxor ((B3 bsr ?SR1) band ?MSK1) bxor Y3
        bxor ((D3 bsl ?SL1) band ?BITMASK32)
     ].

