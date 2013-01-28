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
    {ok, State};
handle_event({disconnect,Pid}, State) ->
    io:format("DB| disconnected: ~p\n", [Pid]),
    {ok, State};
handle_event({message, Client, Msg=#msg{}}, State) ->
    io:format("DB| got message: ~p\n", [Msg]),
    {ok, State};
handle_event(OtherEvent, State) ->
    error_logger:error_msg("~s got unexpected message: ~p\n", [?MODULE, OtherEvent]),
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
    Reply = ok,
    {ok, Reply, State}.

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
handle_request(Method, Path, Req) ->
    error_logger:warning_msg("~s: Got request for unknown resource ~s:~p\n",
                             [Method, Path]),
    Req:respond(404).

%%%===================================================================
%%% Internal functions
%%%===================================================================

www_filepath(Filename) ->
    PrivDir = code:priv_dir(quest_webboard),
    WwwDir  = filename:join(PrivDir, "www"),
    filename:join(WwwDir, Filename).
