-module(quest_log).

-behaviour(gen_server).

%% API
-export([start_link/0, log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          disklog
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    case disk_log:open([{name, quest_log},
                        {file, "questlog.dat"},
                        {repair, true},
                        {head, 'quest_log'},
                        {mode, read_write},
                        {notify, true}]) of
        {ok, Log} -> ok;
        {repaired, Log, _, _} -> ok
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Log}, []).

log(Event) ->
    gen_server:call(?SERVER, {log, Event}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Log}) ->
    {ok, #state{
      disklog=Log
      }}.

handle_call({log, Event}, _From, State=#state{disklog=Log}) ->
    ok = disk_log:alog(Log, Event),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    error_logger:error_msg("~s: unexpected call ~p\n", [?MODULE, Request]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    %% TODO: Handle disk_log notifications
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
