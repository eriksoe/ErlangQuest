-module(quest_log).

-behaviour(gen_server).

%% API
-export([start_link/0,
         log/1,
         async_playback_from/1]).

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
                        {mode, read_write},
                        {notify, true}]) of
        {ok, Log} -> ok;
        {repaired, Log, _, _} -> ok
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Log}, []).

log(Event) ->
    gen_server:call(?SERVER, {log, timestamp_in_micros(), Event}).

async_playback_from(Timestamp) ->
    gen_server:call(?SERVER, {async_playback_from, Timestamp}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Log}) ->
    error_logger:info_msg("Quest log ready.\n", []),
    {ok, #state{
      disklog=Log
      }}.

handle_call({log, Timestamp, Event}, _From, State=#state{disklog=Log}) ->
    ok = disk_log:alog(Log, {Timestamp,Event}),
    {reply, ok, State};
handle_call({async_playback_from, Timestamp}, _From={Pid,_}, State=#state{disklog=Log}) ->
    Ref = start_async_playback(Pid, Timestamp, Log),
    {reply, {ok, Ref}, State};
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
timestamp_in_micros() ->
    {Megas, Secs, Micros} = now(),
    ((Megas * 1000000) + Secs) * 1000000 + Micros.

start_async_playback(Pid, Timestamp, Log) when is_pid(Pid) ->
    Ref = make_ref(),
    proc_lib:spawn(fun() ->
                           link(Pid),
                           %% TODO: Fastforward till Timestamp
                           async_playback_loop(Pid, Ref, Log, start)
                   end),
    Ref.

async_playback_loop(Pid, Ref, Log, Cont) ->
    case disk_log:chunk(Log, Cont) of
        {Cont2, Terms} ->
            send_log_items(Pid, Ref, Terms),
            async_playback_loop(Pid, Ref, Log, Cont2);
        {Cont2, Terms, _Badbytes} ->
            send_log_items(Pid, Ref, Terms),
            async_playback_loop(Pid, Ref, Log, Cont2);
        eof ->
            Pid ! {Ref, eof};
        {error,_}=Err ->
            Pid ! {Ref, Err}
    end.

send_log_items(Pid, Ref, Terms) ->
    lists:foreach(fun(X) -> Pid ! {Ref, {log_item, X}} end, Terms).
