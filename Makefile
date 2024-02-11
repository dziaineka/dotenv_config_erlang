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

check: check_format xref lint dialyzer
