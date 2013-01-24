-module(quest_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("quest_quests.hrl").
-define(SERVER, ?MODULE).

-type(username() :: atom()).
-type(quest_id() :: atom()).
-type(challenge_id() :: integer()).

-record(active_challenge, {
          id :: challenge_id(),
          quest :: quest_id(),
          issued :: timer:timestamp()
         }).
-type(user_data_entry() ::
        {{score, username()}, integer()} % Score
      | {{unlocked, username(), quest_id()}, integer(), integer()} % PointsAwarded, BestTimeMillis
        ).
-record(state, {
          active_challenges :: ets:tab(), % Of #active_challenge{}
          user_data :: ets:tab() % Of user_data_entry(). TODO: make into a dets
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{
       active_challenges = ets:new(active_challenges, [{keypos, #active_challenge.id}]),
       user_data = ets:new(user_data, [ordered_set])
      }}.

handle_call({list, Username}, _From, State) ->
    Reply = quests_available_to_user(Username, State),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    error_logger:warning_msg("~s: unexpected call: ~p\n", [?MODULE, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("~s: unexpected cast: ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    error_logger:warning_msg("~s: unexpected message: ~p\n", [?MODULE, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_challenge_id() ->
    <<ID:64>> = crypto:rand_bytes(8),
    ID.

quests_available_to_user(Username, State) ->
    Score = get_user_score(Username, State),
    [{QuestID,PointsWorth}
     || {QuestID, LevelRequired, PointsWorth,_Description} <- quest_list(),
        Score >= LevelRequired].

get_user_score(Username, #state{user_data=UserTab}) ->
    case ets:lookup(UserTab, {score,Username}) of
        [] -> 0;
        [{_,Score}] -> Score
    end.

%%%===================================================================
%%% Quests
%%%===================================================================


%%% Quest spec: {QuestID, LevelRequired, PointsWorth, Description}.
quest_list() ->
    quest_quests:quest_list().
