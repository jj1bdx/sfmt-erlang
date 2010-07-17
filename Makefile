# GNU Make dependent

.PHONY: compile clean eunit speed

REBAR=$(shell sh -c 'PATH=$(PATH):support which rebar||support/getrebar||echo false')

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

eunit:
	$(REBAR) eunit

speed:
	erl -pa ./ebin -noshell -s sfmt_tests test_speed -s init stop
