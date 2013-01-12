all: check-deps
	@./rebar compile

deps:
	@./rebar get-deps

check-deps:
	@./rebar check-deps
	
clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

run: erl -boot start_sasl +P 2000000 -config ybot.config -s ybot

test: test-eunit 

test-eunit:
	@./rebar eunit skip_deps=true
