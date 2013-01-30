all: compile

deps:
	./rebar get-deps
	make -C deps/socketio deps
	perl -i.bak -pe 's{^#! /bin/sh\b}{#! /bin/bash}' deps/socketio/deps/ossp_uuid/scripts/prepare.sh

compile: deps
	./rebar compile
	# Ensure a clean build:
	make -C deps/socketio/deps/ossp_uuid
	make -C deps/socketio/deps/proper
	(cd deps/socketio && ./rebar compile)

compile-just-QW:
	./rebar compile skip_deps=true

console: compile-just-QW
	erl -pa ebin -pa ../quest/ebin -pa deps/socketio/ebin deps/socketio/deps/misultin/ebin deps/socketio/deps/ossp_uuid/ebin deps/socketio/deps/jsx/ebin deps/socketio/deps/gproc/ebin	


run: compile-just-QW
	erl -pa ../quest_webboard/ebin -pa ../quest/ebin -pa deps/socketio/ebin deps/socketio/deps/misultin/ebin deps/socketio/deps/ossp_uuid/ebin deps/socketio/deps/jsx/ebin deps/socketio/deps/gproc/ebin  -s quest_webboard


clean:
	./rebar clean skip_deps=true

distclean:
	./rebar clean
	rm -rf deps/

.PHONY: all compile deps clean distclean
