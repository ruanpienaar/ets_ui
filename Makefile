.PHONY: release compile get-deps test clean dialyzer rebar3

compile: rebar3 get-deps
	@./rebar3 compile

get-deps: rebar3
	@./rebar3 get-deps

test: rebar3
	@./rebar3 eunit ct

clean: rebar3
	@./rebar3 clean

dialyzer: compile rebar3
	@./rebar3 dialyzer

shell: compile rebar3
	@./rebar3 shell --config config/sys.config

rebar3:
	@ls rebar3 || wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
