
all: compile

compile:
	./rebar compile skip_deps=true

console: compile
	erl -pa ebin -eval 'c:l(quest).'

start-server: compile
	erl -pa ebin -eval 'application:start(quest).' -name "quests@$${MY_IP:-127.0.0.1}" -setcookie questcookie

start-client:
	erl -pa ebin -eval "pong = net_adm:ping('quests@$${SERVER_IP:-127.0.0.1}'), {module,quest} = c:l(quest)." -name "$${USER}@$${MY_IP:-127.0.0.1}" -setcookie questcookie
