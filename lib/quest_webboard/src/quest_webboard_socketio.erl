-module(quest_webboard_socketio).

-behaviour(gen_event).

%% API
-export([start_link/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%% socketio callbacks
-export([handle_request/3]).

-include_lib("socketio/include/socketio.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link() ->
    io:format("DB| quest_webboard_socketio:start_link()\n"),
    {ok, Pid} = socketio_listener:start([{http_port, 8000},
                                         {default_http_handler,?MODULE}]),
    {ok, Pid} = socketio_listener:start([{http_port, 8000},
                                         {default_http_handler,?MODULE}]),

    EventMgr = socketio_listener:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    {ok,Pid}.
%%     gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
%% add_handler() ->
%%     gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({client,Pid}, State) ->
    io:format("DB| connected: ~p\n", [Pid]),
    EventMgr = socketio_client:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    socketio_client:send(Pid, #msg{ content = "Hello from server!" }),
    {ok, State};
handle_event({disconnect,Pid}, State) ->
    io:format("DB| disconnected: ~p\n", [Pid]),
    {ok, State};
handle_event({message, Client, Msg=#msg{}}, State) ->
    error_logger:info_msg("DB| got message: ~p\n", [Msg]),
    handle_sio_message(Client, Msg),
    {ok, State};
handle_event({'EXIT', _, normal}, State) ->
    {ok, State};
handle_event(OtherEvent, State) ->
    error_logger:error_msg("~s got unexpected event: ~p\n", [?MODULE, OtherEvent]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    error_logger:error_msg("~s got unexpected call: ~p\n", [?MODULE, _Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    error_logger:error_msg("~s got unexpected message: ~p\n", [?MODULE, _Info]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_request('GET', [], Req) ->
    Req:file(www_filepath("index.html"));
handle_request('GET', ["quest_webboard.js"=F], Req) ->
    Req:file(www_filepath(F));
handle_request(Method, [Path], Req) ->
    case re:run(Path, "[A-Za-z0-9_]*\.(png|jpg)", [{capture,none}]) of
        match ->
            io:format("DB| file path=~p\n", [www_filepath(Path)]),
            Req:file(www_filepath(Path));
        nomatch ->
            error_logger:warning_msg("~s: Got request for unknown resource ~s:~p\n",
                                     [?MODULE, Method, Path]),
            Req:respond(404)
    end;
handle_request(Method, Path, Req) ->
    error_logger:warning_msg("~s: Got request for unknown resource ~s:~p\n",
                             [?MODULE, Method, Path]),
    Req:respond(404).

handle_sio_message(Client, Msg) ->
    io:format("DB| handle_sio_message: ~p\n", [Msg]),
    case Msg#msg.content of
        [{<<"type">>, <<"replay">>},
         {<<"from">>, TS}] ->
            replay_quest_log_from(TS, Client);
        MsgContent ->
            error_logger:warning_msg("Unexpected message: ~p\n", [MsgContent])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

www_filepath(Filename) ->
    PrivDir = code:priv_dir(quest_webboard),
    WwwDir  = filename:join(PrivDir, "www"),
    filename:join(WwwDir, Filename).

replay_quest_log_from(TS, Client) ->
    {ok, Ref} = quest_log:async_playback_from(TS),
    replay_quest_log_loop(Ref, Client).

replay_quest_log_loop(Ref, Client) ->
    receive
        {Ref, Msg} ->
            io:format("DB| replay_quest_log_loop: ~p\n", [Msg]),
            case Msg of
                eof -> ok; % Done.
                {error, Reason} ->
                    error({unable_to_replay, Reason});
                {log_item, {TS,Item}} ->
                    replay_item(Client, TS, Item),
                    replay_quest_log_loop(Ref, Client)
            end
    after 15000 ->
            error({timeout_while_replaying})
    end.

replay_item(Client, TS, {achieved, Username, QuestID, Achieved}) ->
    Points = lists:sum([P || {_,P} <- Achieved]),
    Variants = [a2b(V) || {V,_} <- Achieved],
    socketio_client:send(Client, #msg{json=true,
                                      content=[{<<"type">>, <<"achieved">>},
                                               {<<"user">>, a2b(Username)},
                                               {<<"quest">>, a2b(QuestID)},
                                               {<<"points">>, Points},
                                               {<<"variants">>, Variants}]}).

a2b(Atom) -> atom_to_binary(Atom, utf8).
