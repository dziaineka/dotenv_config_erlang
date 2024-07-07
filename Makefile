.PHONY: format check_format xref dialyzer lint test check docs

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

docs:
	rebar3 ex_doc

publish:
	rebar3 hex publish --repo hexpm

clean_caches:
	rm -rf _build
	rm -rf ~/.cache/rebar3
