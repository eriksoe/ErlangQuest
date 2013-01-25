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
    {ok, #state{
       active_challenges = ets:new(active_challenges, [{keypos, #active_challenge.id}]),
       user_scores = ets:new(user_scores, [{keypos, #user_score.username}]),
       user_achievements = ets:new(user_achievements, [ordered_set, {keypos, #user_achievement.key}])
      }}.

handle_call({list, Username}, _From, State) ->
    Reply = quests_available_to_user(Username, State),
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

get_user_achievements(Username, State) ->
    %% TODO: Add achievement list.
    [{current_score, get_user_score(Username, State)}].

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
    (AddPoints==0) orelse add_to_user_score(Username, AddPoints, State),
    Achieved.

make_challenge(Quest, Username, #state{active_challenges = ACTab}) ->
    ChallengeID = gen_challenge_id(),
    {QuestID, _LevelRequired, PointsWorth, Description} = Quest,
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
    {ChallengeID, PointsWorth, Description, Input}.

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
