-module(quest_interlocutor).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add/3, forget/1, read_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          interaction_table
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Token, Fun, InitialState) ->
    gen_server:call(?SERVER, {add_interaction, Token, Fun, InitialState}).

forget(Token) ->
    gen_server:call(?SERVER, {forget_interaction, Token}).

read_state(Token) ->
    gen_server:call(?SERVER, {read_state, Token}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{
       interaction_table = ets:new(interaction_state, [set, protected])
      }}.

handle_call({add_interaction, Token, Fun, InitialState}, _From, State) ->
    add_handler_for_interaction(Token, Fun, InitialState, State),
    {reply, ok, State};
handle_call({read_state, Token}, _From, State) ->
    IState = read_interaction_state(Token, State),
    {reply, IState, State};
handle_call({forget_interaction, Token}, _From, State) ->
    forget_interaction(Token, State),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    error_logger:error_msg("~s: Unexpected call: ~p\n", [Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("~s: Unexpected cast: ~p\n", [Msg]),
    {noreply, State}.

handle_info({Token,Data}, State) when is_integer(Token) ->
    handle_message_for_interaction(Token, Data, State),
    {noreply, State};
handle_info(Msg, State) ->
    error_logger:error_msg("~s: Unexpected message: ~p\n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_handler_for_interaction(Token, Fun, InitialState, #state{interaction_table = Table}) ->
    true = ets:insert(Table, {Token, Fun, InitialState}).

handle_message_for_interaction(Token, Data, #state{interaction_table = Table}=State) ->
    case ets:lookup(Table, Token) of
        [{Token, IFun, IState}] ->
            try IFun(Data, IState) of
                %% done ->
                %%     forget_interaction(Token, State);
                NewIState ->
                    ets:update_element(Table, Token, {3,NewIState})
                %% BadReturn ->
                %%     error_logger:error_msg("~s: Callback function returned bad value ~p (function=~p, in_message=~p, in_state=~p)\n",
                %%                            [?MODULE, BadReturn, IFun, Data, IState]),
                %%     forget_interaction(Token, State)
            catch _:Err ->
                    error_logger:error_msg("~s: Callback function threw ~p (function=~p, in_message=~p, in_state=~p)\n",
                                           [?MODULE, Err, IFun, Data, IState]),
                    forget_interaction(Token, State)
            end;
        [] ->
            %% Bad interaction ID. Make no noise.
            ok
    end.

forget_interaction(Token, #state{interaction_table=Table}) ->
    ets:delete(Table, Token).

read_interaction_state(Token, #state{interaction_table=Table}) ->
    case ets:lookup(Table, Token) of
        [{Token, _IFun, IState}] -> {ok, IState};
        [] -> {error, not_found}
    end.
