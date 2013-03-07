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
-type(quest_variant() :: slow | fast).

-record(active_challenge, {
          id :: challenge_id(),
          user :: username(),
          quest :: quest_id(),
          input :: _,
          %% TODO: repetitions_left, max_duration_in_reps
          issued :: timer:timestamp()
         }).

-record(user_score, {
          username :: username(),
          score :: integer()
         }).

-type(user_achievement_key() :: {username(), quest_id(), quest_variant()}).
-record(user_achievement, {
          key :: user_achievement_key(),
          points_awarded :: integer()
          %%best_time_millis :: integer(),
         }).

-record(state, {
          active_challenges :: ets:tab(), % Of #active_challenge{}
          user_scores       :: ets:tab(),
          user_achievements :: ets:tab()
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
    self() ! '$init_from_log',
    {ok, #state{
       active_challenges = ets:new(active_challenges, [{keypos, #active_challenge.id}]),
       user_scores = ets:new(user_scores, [{keypos, #user_score.username}]),
       user_achievements = ets:new(user_achievements, [ordered_set, {keypos, #user_achievement.key}])
      }}.

handle_call({list, Username}, _From, State) ->
    Reply = quests_available_to_user(Username, State),
    {reply, Reply, State};
handle_call({describe_quest, QuestName}, _From, State) ->
    Reply = describe_quest(QuestName),
    {reply, Reply, State};
handle_call({get_challenge, Username, QuestID}, _From, State) ->
    try get_challenge(Username, QuestID, State) of
        Reply -> {reply, Reply, State}
    catch
        _:Error ->
            error_logger:error_msg("~s: get_challenge() crashed: ~p\n** trace: ~p\n",
                                   [?MODULE, Error, erlang:get_stacktrace()]),
            {reply, {error,{internal_error, Error}}, State}
    end;

handle_call({answer_challenge, ChallengeID, Answer}, _From, State) ->
    try answer_challenge(ChallengeID, Answer, State) of
        Reply -> {reply, Reply, State}
    catch
        _:Error ->
            error_logger:error_msg("~s: answer_challenge() crashed: ~p\n** trace: ~p\n",
                                   [?MODULE, Error, erlang:get_stacktrace()]),
            {reply, {internal_error, Error}, State}
    end;
handle_call({score, Username}, _From, State) ->
    Reply = get_user_achievements(Username, State),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    error_logger:warning_msg("~s: unexpected call: ~p\n", [?MODULE, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("~s: unexpected cast: ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info('$init_from_log', State) ->
    error_logger:info_msg("~s: Restoring state from log...\n", [?MODULE]),
    {ok, NEvents} = init_from_log(State),
    error_logger:info_msg("~s: State restored from log (~b events).\n",
                          [?MODULE, NEvents]),
    {noreply, State};
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

quests_available_to_user(Username, #state{user_achievements=AchTab}=State) ->
    Score = get_user_score(Username, State),
    Quests = [{QuestID,PointsWorth}
	      || {QuestID, LevelRequired, PointsWorth,_Description} <- quest_list(),
		 Score >= LevelRequired],
    %% Decorate quest-list with status (done/undone) and Variant
    lists:map(fun({QuestID, PointsWorth}) ->
		      {QuestID,
		       PointsWorth,
		       quest_points(Username, QuestID, slow, AchTab),
		       quest_points(Username, QuestID, fast, AchTab)}
	      end,
	      Quests).

quest_points(Username, QuestID, Variant, AchTab) ->
    case ets:lookup(AchTab, {Username,QuestID,Variant}) of
	[] ->
	    0;
	[#user_achievement{points_awarded=Points}] ->
	    Points
    end.

get_user_achievements(Username, State) ->
    %% TODO: Add achievement list.
    [{current_score, get_user_score(Username, State)}].

describe_quest(QuestID) ->
    case quest_by_name(QuestID) of
        false ->
            {error, {unknown_quest, QuestID}};
        {_QuestID, LevelRequired, PointsWorth, Description} ->
            {ok, LevelRequired, PointsWorth, Description}
    end.

get_challenge(Username, QuestID, State) ->
    case quest_by_name(QuestID) of
        false ->
            {error, quest_unknown_or_not_accessible};
        {QuestID, LevelRequired, _PointsWorth, _Description}=Quest ->
            UserScore = get_user_score(Username, State),
            if LevelRequired > UserScore ->
                    {error, quest_unknown_or_not_accessible};
               true ->
                    make_challenge(Quest, Username, State)
            end
    end.

answer_challenge(ChallengeID, Answer, State=#state{active_challenges=ACTab}) ->
    TS1 = now(),
    case ets:lookup(ACTab, ChallengeID) of
        [] ->
            unknown_challenge_id;
        [#active_challenge{user=Username,
                           quest=QuestID,
                           input=Input,
                           issued=TS0}] ->
            ets:delete(ACTab, ChallengeID),
            #quest{verify=VerifyFun} = quest_functions(QuestID),
            case VerifyFun(Input, Answer) of
                true ->
                    Elapsed = timer:now_diff(TS1,TS0) div 1000,
                    answered_correctly(Username, QuestID, Elapsed, State);
                false ->
                    wrong_answer
            end
    end.

answered_correctly(Username, QuestID, Elapsed, State) ->
    {_QuestID, _LevelRequired, Points, _Description} = quest_by_name(QuestID),
    Achieved = add_achievements(Username, QuestID, Points, Elapsed, State),
    Props = [{time, Elapsed}],
    case Achieved of
        [] ->
            {correct_but_nothing_unlocked, Props};
        _ ->
            {achievement_unlocked, [{achieved, Achieved},
                                    {new_score, get_user_score(Username,State)}
                                    | Props]}
    end.

add_achievements(Username, QuestID, Points, Elapsed,
                 #state{user_achievements=AchTab}=State) ->
    %% Calculate list of new achievements:
    Achieved =
        [Variant
         || {Variant,TimeLimit} <- [{slow,infinity}, {fast,100}],
            Elapsed =< TimeLimit,
            ets:insert_new(AchTab, #user_achievement{key={Username,QuestID,Variant},
                                                     points_awarded=Points})],
    AddPoints = length(Achieved) * Points,
    if AddPoints > 0 ->
            ok = quest_log:log({achieved, Username, QuestID,
                                [{A,Points} || A <- Achieved]}),
            add_to_user_score(Username, AddPoints, State);
       true -> ok
    end,
    Achieved.

make_challenge(Quest, Username, #state{active_challenges = ACTab}) ->
    ChallengeID = gen_challenge_id(),
    {QuestID, _LevelRequired, _PointsWorth, _Description} = Quest,
    #quest{generate=GenFun} = quest_functions(QuestID),
    ChallengeSpec = GenFun(),
    ets:insert(ACTab, #active_challenge{id=ChallengeID,
                                        user=Username,
                                        quest=QuestID,
                                        input = ChallengeSpec,
                                        issued=now()}),
    Input = case ChallengeSpec of
                {'$remember', _, I} -> I;
                I -> I
            end,
    {ChallengeID, Input}.

%%%===================================================================
%%% Quests
%%%===================================================================

quest_by_name(QuestID) ->
    lists:keyfind(QuestID, 1, quest_list()).

quest_functions(QuestID) ->
    quest_quests:QuestID().

%%% Quest spec: {QuestID, LevelRequired, PointsWorth, Description}.
quest_list() ->
    quest_quests:quest_list().

%%%===================================================================
%%% Scoring
%%%===================================================================

get_user_score('$admin', _) -> 1000000;
get_user_score(Username, #state{user_scores=UserTab}) ->
    case ets:lookup(UserTab, Username) of
        [] -> 0;
        [#user_score{score=Score}] -> Score
    end.

add_to_user_score(Username, Amount, #state{user_scores=ScoresTab}) ->
    try ets:update_counter(ScoresTab, Username, {#user_score.score,Amount})
    catch error:badarg ->
            ets:insert(ScoresTab, #user_score{username=Username, score=Amount})
    end.

%%%===================================================================
%%% Persistence
%%%===================================================================

init_from_log(State) ->
    {ok, Ref} = quest_log:async_playback(epoch, now),
    restore_loop(Ref, State, 0).

restore_loop(Ref, State, N) ->
    receive
        {Ref, Msg} ->
            case Msg of
                eof -> {ok, N}; % Done.
                live ->
                    restore_loop(Ref, State, N); % Keep reading to eof.
                {error, Reason} ->
                    erlang:error({unable_to_restore_state, Reason});
                {log_item, {TS,Item}} ->
                    replay_event(TS, Item, State),
                    restore_loop(Ref, State, N+1)
            end
    after 5000 ->
            erlang:error({timeout_while_restoring_state})
    end.

replay_event(_TS, {achieved, Username, QuestID, Achieved}, State) ->
    #state{user_achievements=AchTab} = State,
    PointList =
        [begin
             ets:insert(AchTab, #user_achievement{key={Username,QuestID,V},
                                                  points_awarded=P}),
             P
         end
         || {V,P} <- Achieved],
    AddPoints = lists:sum(PointList),
    add_to_user_score(Username, AddPoints, State).

