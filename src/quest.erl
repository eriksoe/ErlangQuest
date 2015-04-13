-module(quest).

-export([help/0,
         list/0, list/1, list/2,
         score/0, score/1,
         describe_quest/1,
         get_challenge/1, get_challenge/2,
         answer_challenge/2,
         submit/2, submit/3,
	 set_user/1
         ]).

-define(SERVER, {global, quest_server}).
-define(DEFAULT_USERNAME, 'ERLANG_QUEST_DEFAULT_USER').

help_text() ->
    "\n-=- This is the Erlang Quest. -=-\n" ++
        "Here's what you can do:\n\n"++
        "  * quest:help() -- shows this text.\n"++
	"\n"++
        "  * quest:set_user(Username) -- sets the default username.\n"++
	"\n"++
        "  * quest:list(Username, Options) -- shows available quests.\n"++
        "       If Options is [] only undone quest is listed\n"++
        "                     [all] every available quest is listed.\n"++
        "  * quest:list(Username) -- only undone quest.\n"++
        "  * quest:list(Options) -- control output for default user.\n"++
        "  * quest:list() -- undone quests for the default user.\n"++
	"\n"++
        "  * quest:score(Username) -- shows your accomplishments so far.\n"++
        "  * quest:score() -- using the default user.\n"++
	"\n"++
        "  * quest:describe_quest(QuestID) -- shows a description of a quest.\n"++
	"\n"++
        "  * quest:get_challenge(Username, QuestID) -- requests a challenge.\n"++
        "      Returns {ChallengeID, Input}.\n"++
        "  * quest:get_challenge(QuestID) -- using the default user.\n"++
	"\n"++
        "  * quest:answer_challenge(ChallengeID, Answer) -- answers a challenge.\n"++
        "      Returns {achievement_unlocked, [...]}\n"++
        "      or      {correct_but_nothing_unlocked, [...]}\n"++
        "      or      wrong_answer\n"++
        "      or      unknown_challenge_id.\n"++
        "\n"++
        "  * quests:submit(Username, QuestID, SolutionFun) -- shortcut for\n"++
        "      submitting a solution to a quest.\n"++
        "      Is equivalent to:\n"++
        "        {CID,Input}=quest:get_challenge(Username, QuestID),\n"++
        "        quest:answer_challenge(CID,SolutionFun(Input))\n"++
        "  * quests:submit(QuestID, SolutionModule) -- giving the solution fun via\n"++
        "      a module.\n"++
        "      SolutionModule should contain an exported function named QuestID\n"++
        "      prepended with 'quest_'. Fx: quest_any_answer(Input)\n"++
        "  * quests:submit(QuestID, SolutionFun) -- using the default user.\n"++
        "\n"++
        "Each quest has two levels: slow and fast.  You get points the first time you\n"++
        "answer a challenge correctly (slow-level achievement), and the first time\n"++
        "you answer correctly within 100ms (fast-level achievement).\n\n"++
        "".

help() ->
    io:format("~s", [help_text()]).

%%% List %%%%%%%%%%%%

list() ->
    call_with_default_username(fun(Username) -> list(Username, []) end).

list(Options) when is_list(Options) ->
    call_with_default_username(fun(Username) -> list(Username, Options) end);
list(Username) when is_atom(Username) ->
    list(Username, []).

list(Username, Options) when is_atom(Username), is_list(Options) ->
    Quests = gen_server:call(?SERVER, {list, Username}),
    io:format("Quests currently available to ~s:\n", [Username]),
    io:format("----Quest----------------------------Points------Variants------------\n"),
    lists:foreach(fun display_quest_line/1, extract_quests_to_display(Quests, Options)).

extract_quests_to_display(Quests, Options) ->
    Qs = case lists:member(all, Options) of
	     false ->
		 [Q || Q={_QuestID, _PointsWorth, SlowPoints, FastPoints} <- Quests,
		       quest_in_progress(SlowPoints, FastPoints)];
	     true ->
		 Quests
	 end,
    lists:keysort(2, Qs).

display_quest_line({QuestID, PointsWorth, SlowPoint, FastPoints}) ->
    io:format(" ~2s ~-30s ~8s     ~1sslow(50%), ~1sfast(50%)\n",
	      [completion_label(SlowPoint,FastPoints),
	       QuestID,
	       point_label(PointsWorth,SlowPoint,FastPoints),
	       variant_bullet(SlowPoint),
	       variant_bullet(FastPoints)]).

point_label(P,S,F) ->
    P2 = P+P,
    case S+F of
	P2 ->
	    io_lib:format("~p", [P2]);
	Sum ->
	    io_lib:format("~p/~p", [Sum, P2])
    end.

completion_label(S,F) ->
    case {quest_not_started(S,F), quest_completed(S,F)} of
	{true, _} ->
	    "";
	{false, false} ->
	    "*";
	{false, true} ->
	    "**"
    end.

variant_bullet(V) ->
    case V > 0 of
	true ->
	    "*";
	false ->
	    ""
    end.

quest_not_started(Slow, Fast) -> Slow == 0 andalso Fast == 0.
quest_in_progress(Slow, Fast) -> Slow == 0 orelse  Fast == 0.
quest_completed  (Slow, Fast) -> Slow  > 0 andalso Fast  > 0.

%%% Score %%%%%%%%%%%%

score() ->
    call_with_default_username(fun(Username) -> score(Username) end).

score(Username) when is_atom(Username) ->
    gen_server:call(?SERVER, {score, Username}).

%%% Describe quest %%%%%%%%%%%%

describe_quest(QuestID) ->
    case gen_server:call(?SERVER, {describe_quest, QuestID}) of
        {ok, LevelRequired, PointsWorth, Description} ->
            io:format("Quest ~-40s (Worth: ~b.  Requires: ~b.)~n",
                      [["'",atom_to_list(QuestID),"':"],
                       PointsWorth, LevelRequired]),
            %% io:format("(Points worth: ~b.  Level required: ~b.)~n",
            %%           [PointsWorth, LevelRequired]),
            print_quest_description(Description),
            io:format("~n");
        {error,_}=Error ->
            Error
    end.

print_quest_description(Str) when is_list(Str), is_integer(hd(Str)) ->
    print_quest_description([Str]);
print_quest_description(L) ->
    lists:foreach(fun(S) when is_list(S) ->
                          io:format("  ~s\n", [S])
                  end,
                  L),
    ok.

%%% Get Challenge %%%%%%%%%%%%

get_challenge(QuestID) ->
    call_with_default_username(fun(Username) -> get_challenge(Username, QuestID) end).

get_challenge(Username, QuestID) when is_atom(Username), is_atom(QuestID) ->
    gen_server:call(?SERVER, {get_challenge, Username, QuestID}).

%%% Answer Challenge %%%%%%%%%%%%

answer_challenge(ChallengeID, Answer) ->
    gen_server:call(?SERVER, {answer_challenge, ChallengeID, Answer}).

%%% Submit %%%%%%%%%%%%

submit(QuestID, SolutionFunOrMod) ->
    call_with_default_username(fun(Username) -> submit(Username, QuestID, SolutionFunOrMod) end).

submit(Username, QuestID, SolutionModule) when is_atom(SolutionModule) ->
    FunName = list_to_atom("quest_"++atom_to_list(QuestID)),
    submit(Username, QuestID, fun SolutionModule:FunName/1);
submit(Username, QuestID, SolutionFun) when is_atom(Username),
                                            is_atom(QuestID),
                                            is_function(SolutionFun,1) ->
    case quest:get_challenge(Username, QuestID) of
        {error,_}=Error -> Error;
        {ChallengeID,Input} ->
            quest:answer_challenge(ChallengeID, SolutionFun(Input))
    end.

%%% Set user %%%%%%%%%%%%

set_user(Username) when is_atom(Username) ->
    put(?DEFAULT_USERNAME, Username),
    ok;
set_user(_Username) ->
    {error, username_must_be_an_atom}.

%%% Utils %%%%%%%%%

call_with_default_username(Continuation) ->
    case get(?DEFAULT_USERNAME) of
	undefined ->
	    io:format("No default user set.  Use quest:set_user(Username).~n");
	Username ->
	    Continuation(Username)
    end.
