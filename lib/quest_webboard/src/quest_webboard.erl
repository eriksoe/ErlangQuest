-module(quest_webboard).

-export([start/0]).

start_app(App) ->
    case application:start(App) of
        ok -> ok;
        {already_started, _} -> ok;
        {error, {not_started, OtherApp}} ->
            start_app(OtherApp),
            start_app(App)
    end.

start() ->
%%     ensure_started(sasl),
%%     ensure_started(gproc),
%%     ensure_started(misultin),
%%     ensure_started(socketio),
    start_app(quest_webboard),
    ok.
