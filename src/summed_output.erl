-module(summed_output).
-export([init/1]).
-define(BARNUM, 10).

init([LI, LN]) ->
    A = array:new([{size, ?BARNUM},{default, 0}]),
    Out = main(list_to_integer(atom_to_list(LI)),
		list_to_integer(atom_to_list(LN))),
    A2 = listup(Out, A),
    lists:map(fun(J) ->
		      io:format("~p ~p~n", 
				[J / ?BARNUM, array:get(J, A2)]) end,
	      lists:seq(0, ?BARNUM - 1)).

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
