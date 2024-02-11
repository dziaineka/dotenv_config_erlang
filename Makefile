.PHONY: format check_format xref dialyzer lint test check

format:
	rebar3 fmt

check_format:
	rebar3 fmt --check

xref:
	rebar3 xref

dialyzer:
	rebar3 dialyzer

lint:
	rebar3 lint

test:
	rebar3 eunit

check: check_format xref lint dialyzer test
