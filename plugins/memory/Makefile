.PHONY: all clean test eunit deps doc dialyzer

PROJECT=$(basename $(CURDIR))
REBAR=rebar
DIALYZER=dialyzer

DIALYZER_OPTS =
#DIALYZER_OPTS = -Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

# Dependencies that should be included in a cached dialyzer PLT file.
#DIALYZER_DEPS = deps/*/ebin
DIALYZER_DEPS = deps/lager/ebin \
                deps/ibrowse/ebin \
	        deps/mochijson2/ebin \
		deps/cowboy/ebin

DEPS_PLT = $(PROJECT).plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       erts \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       observer \
                       public_key \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       tools \
                       webtool \
                       xmerl

EUNIT_FLAGS=-sasl errlog_type error

all: deps compile

deps:
	$(REBAR) get-deps

compile:

ifeq ($(NODEPS),true)
	$(REBAR) compile skip_deps=true
else
	$(REBAR) compile
endif

clean:
	$(REBAR) clean

test: all
	rm -rf .eunit
	$(REBAR) eunit skip_deps=true

eunit:

ifeq ($(SUITES),) # comma separated module names
	ERL_FLAGS="$(EUNIT_FLAGS)" $(REBAR) eunit skip_deps=true
else
	ERL_FLAGS="$(EUNIT_FLAGS)" $(REBAR) eunit skip_deps=true suites=$(SUITES)
endif

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: ~/.dialyzer_plt
	$(DIALYZER) $(DIALYZER_OPTS) -r ebin
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	$(DIALYZER) $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	$(DIALYZER) --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	$(REBAR) doc skip_deps=true
