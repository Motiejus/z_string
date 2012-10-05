REBAR := $(shell which rebar || echo ./rebar)
REBAR_URL := https://github.com/downloads/basho/rebar/rebar

.PHONY: compile test

compile: $(REBAR)
	$(REBAR) get-deps compile

test: compile
	$(REBAR) eunit -v skip_deps=true

./rebar:
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar
