-module(quest_log).

-behaviour(gen_server).

%% API
-export([start_link/0,
         log/1,
         async_playback/2,
         subscribe/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          disklog,
          subscribers
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
    gen_server:call(?SERVER, {log, timestamp_in_micros(now()), Event}).

async_playback(FromTS, ToTS) ->
    gen_server:call(?SERVER, {async_playback, to_micros(FromTS), to_micros(ToTS)}).

subscribe() ->
    gen_server:call(?SERVER, {subscribe, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Log}) ->
    error_logger:info_msg("Quest log ready.\n", []),
    {ok, #state{
       disklog=Log,
       subscribers=sets:new()
      }}.

handle_call({log, Timestamp, Event}, From, State=#state{disklog=Log}) ->
    ok = disk_log:alog(Log, {Timestamp,Event}),
    gen_server:reply(From, ok),
    publish_event(Timestamp, Event, State),
    {noreply, State};
handle_call({async_playback, FromTS, ToTS}, _From={Pid,_},
            State=#state{disklog=Log}) ->
    Ref = start_async_playback(Pid, FromTS, ToTS, Log),
    {reply, {ok, Ref}, State};
handle_call({subscribe, Subscriber}, _From, State=#state{subscribers=Subscribers}) ->
    _MRef = monitor(process, Subscriber),
    error_logger:info_msg("~s: Adding subscriber ~p\n", [?MODULE, Subscriber]),
    Subscribers2 = sets:add_element(Subscriber, Subscribers),
    {reply, ok, State#state{subscribers=Subscribers2}};
handle_call(Request, _From, State) ->
    error_logger:error_msg("~s: unexpected call ~p\n", [?MODULE, Request]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Mref, process, Pid, _Reason}, State=#state{subscribers=Subscribers}) ->
    error_logger:info_msg("~s: Removing subscriber ~p\n", [?MODULE, Pid]),
    Subscribers2 = sets:del_element(Pid, Subscribers),
    {noreply, State#state{subscribers=Subscribers2}};
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
publish_event(Timestamp, Event, #state{subscribers=Subscribers}) ->
    PublishEvent = {quest_log_event, Timestamp, Event},
    sets:fold(fun(Sub,_) -> Sub ! PublishEvent end,
              dummy,
              Subscribers),
    ok.

to_micros(epoch) -> 0;
to_micros(now) -> timestamp_in_micros(now());
to_micros(infinity) -> infinity;
to_micros(TS) when is_integer(TS) -> TS.

timestamp_in_micros({Megas, Secs, Micros}) ->
    ((Megas * 1000000) + Secs) * 1000000 + Micros.

start_async_playback(Pid, FromTS, ToTS, Log) when is_pid(Pid) ->
    Ref = make_ref(),
    proc_lib:spawn(fun() ->
                           link(Pid),
                           ok = subscribe(),
                           %% Is it possible to fastforward till FromTS, instead of filter?
                           async_playback_loop(Pid, Ref, Log, FromTS, ToTS, start)
                   end),
    Ref.

async_playback_loop(Pid, Ref, Log, FromTS, ToTS, Cont) ->
    case disk_log:chunk(Log, Cont) of
        {error,_}=Err ->
            Pid ! {Ref, Err};
        eof ->
            Pid ! {Ref, live},
            error_logger:info_msg("~s: async_playback ~p: going live [~p;~p]\n", [?MODULE, self(), FromTS, ToTS]),
            live_playback_init(Pid, Ref, max(FromTS,timestamp_in_micros(now())), ToTS);
        {Cont2, Terms} ->
            TS2 = send_log_items(Pid, Ref, FromTS, ToTS, Terms),
            async_playback_loop(Pid, Ref, Log, TS2, ToTS, Cont2);
        {Cont2, Terms, _Badbytes} ->
            TS2 = send_log_items(Pid, Ref, FromTS, ToTS, Terms),
            async_playback_loop(Pid, Ref, Log, TS2, ToTS, Cont2)
    end.

live_playback_init(Pid, Ref, FromTS, ToTS) when FromTS > ToTS ->
    error_logger:info_msg("~s: async_playback ~p: stopping live feed [~p;~p]\n", [?MODULE, self(), FromTS, ToTS]),
    Pid ! {Ref, eof};
live_playback_init(Pid, Ref, FromTS, ToTS) ->
    %% Flush messages we've already sent
    receive
        {quest_log_event, TS, _}=Msg ->
            if TS < FromTS ->
                    error_logger:info_msg("~s: async_playback ~p: skipping live event for TS=~p [~p;~p]\n", [?MODULE, self(), TS, FromTS, ToTS]),
                    live_playback_init(Pid, Ref, FromTS, ToTS);
               true ->
                    %% Go live!
                    live_playback_loop(Pid, Ref, Msg, ToTS)
            end
    end.

live_playback_loop(Pid, Ref, {quest_log_event, TS, Event}, ToTS) ->
    if TS > ToTS ->
            done;
       true ->
            %% error_logger:info_msg("~s: async_playback ~p: publishing live event for TS=~p [?;~p]\n", [?MODULE, self(), TS, ToTS]),
            send_log_item(Pid, Ref, {TS,Event}),
            receive
                {quest_log_event, _, _}=Msg2 ->
                    live_playback_loop(Pid, Ref, Msg2, ToTS)
            end
    end.

send_log_items(Pid, Ref, FromTS, ToTS, Terms) ->
    Terms2 = lists:dropwhile(fun({TS,_}) -> TS < FromTS end, Terms),
    case Terms2 of
        [] -> FromTS;
        _ ->
            lists:foreach(fun({TS,_}=X) ->
                                  (TS =< ToTS) andalso send_log_item(Pid, Ref, X)
                          end, Terms2),
            {LatestSendTS, _} = lists:last(Terms2),
            LatestSendTS+1
    end.

send_log_item(Pid, Ref, Term={_,_}) ->
    Pid ! {Ref, {log_item, Term}}.
