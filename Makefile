# for both BSD/GNU Make

.PHONY: compile clean c_doc doc eunit speed

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
       ||support/getrebar||echo false"`

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

c_doc:
	doxygen

doc: 
	$(REBAR) doc

eunit:
	$(REBAR) eunit

dialyzer:
	dialyzer src/*.erl

firsttime-dialyzer:
	dialyzer --build_plt --apps kernel stdlib erts mnesia eunit crypto

# N: 19937, for periods (2^N - 1)

speed:
	erl -pa ./ebin -noshell -s sfmt_tests test_speed -s init stop
	erl -pa ./ebin -noshell -s sfmt_pure_tests test_speed -s init stop
