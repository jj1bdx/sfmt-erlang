-module(summed_output).
-export([init2/1, init/1]).

init2(I) ->
    io:format("~p~n", [I]).

init([LI, LN]) ->
    io:format("~p~n",
	      [main(list_to_integer(atom_to_list(LI)),
		    list_to_integer(atom_to_list(LN)))]).

main(I, N) ->
    sfmt:seed(),
    lists:map(fun(_) ->
			  genrand(N) end, lists:seq(1, I)).

genrand(N) ->
    lists:sum(lists:map(fun(_) ->
				sfmt:uniform() end, lists:seq(1, N))) / N.
