# GNU Make dependent

.PHONY: compile clean c_doc doc eunit speed

REBAR=$(shell sh -c "PATH='$(PATH)':support which rebar||support/getrebar||echo false")

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

speed:
	erl -pa ./ebin -noshell -s sfmt_tests test_speed -s init stop
