-module(summed_output).
-export([init/1, dispbar/2]).
-define(BARNUM, 20).
-define(BARLEN, 50).

init([LI, LN]) ->
    A = array:new([{size, ?BARNUM},{default, 0}]),
    LIN = list_to_integer(atom_to_list(LI)),
    Out = main(LIN,list_to_integer(atom_to_list(LN))),
    A2 = listup(Out, A),
    lists:map(fun(J) ->
		      V = array:get(J, A2),
		      io:format("~s",[dispbar(V, LIN)]),
		      io:format("~p ~p~n", 
				[
				 J / ?BARNUM, V]) end,
	      lists:seq(0, ?BARNUM - 1)).

dispbar(J, LI) ->
    Len = float(J) / LI * ?BARLEN,
    barstr(?BARLEN, Len, []).
  
barstr(0, _, Str) ->
    lists:reverse(Str);
barstr(N, Len, Str) ->
    case N >= Len of
	true ->
	    NewStr = Str ++ [32];
	false ->
	    NewStr = Str ++ [$*]
    end,
    barstr(N-1, Len, NewStr).

listup([], AccA) ->
    AccA;
listup([H|T], AccA) ->
    NewA = setarray(H, AccA),
    listup(T, NewA).

setarray(X, OldA) ->
  I = trunc(X * float(?BARNUM)),
  V = array:get(I, OldA),
  array:set(I, V + 1, OldA).

main(I, N) ->
    sfmt:seed(),
    lists:map(fun(_) ->
			  genrand(N) end, lists:seq(1, I)).

genrand(N) ->
    lists:sum(lists:map(fun(_) ->
				sfmt:uniform() end, lists:seq(1, N))) / N.
