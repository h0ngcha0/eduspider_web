.PHONY: rel deps

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

rel: deps compile
	@./rebar generate

relforce: deps compile
	@./rebar generate force=1

clean:
	@./rebar clean

distclean: clean relclean
	@./rebar delete-deps

relclean:
	rm -rf rel/eduspider

stage : rel
	cd rel/eduspider/lib && \
	rm -rf wiki_creole-* eduspider-* && \
	ln -s ../../../lib/wiki_creole && \
	ln -s ../../../lib/eduspider_web && \
	ln -s ../../../lib/eduspider_core

test:
	./rebar skip_deps=true eunit

console:
	exec erl -sname eduspider -pa $(PWD)/lib/*/ebin -boot start_sasl -config gen/files/sys.config  -s reloader -s eduspider_web -s eduspider_core

webstart: all console

devstart: compile console

