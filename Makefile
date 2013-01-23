
all: compile

compile:
	./rebar compile skip_deps=true

console:
	erl -pa ebin -eval 'c:l(quest).'

