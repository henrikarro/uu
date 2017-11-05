default: run

MODULE = hello_world

.PHONY: run
run: compile
	erl -noshell -eval "$(MODULE):run()" -s init stop

.PHONY: compile
compile:
	erlc -Werror +debug_info $(MODULE).erl

.PHONY: dialyzer
dialyzer: compile
	dialyzer -Wunmatched_returns $(MODULE).beam

.PHONY: eunit
eunit: compile
	erl -noshell -eval "eunit:test($(MODULE))" -s init stop

.PHONY: proper
proper: compile
	erl -noshell -pa $(PROPER_EBIN) -eval "proper:module($(MODULE))" -s init stop

.PHONY: clean
clean:
	$(RM) *.beam erl_crash.dump
