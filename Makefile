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

run:
	erl -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin -config ybot.config -boot start_sasl +P 2000000 -s ybot

test: test-eunit 

test-eunit:
	@./rebar eunit skip_deps=true
