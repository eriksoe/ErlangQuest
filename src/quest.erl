-module(quest).

-export([help/0,
         list/0, list/1,
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
        "  * quest:list(Username) -- shows the quests currently available to you.\n"++
        "  * quest:list() -- using the default user.\n"++
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

list() ->
    call_with_default_username(fun(Username) -> list(Username) end).

list(Username) when is_atom(Username) ->
    Quests = gen_server:call(?SERVER, {list, Username}),
    io:format("Quests currently available to ~s:\n", [Username]),
    io:format("---Pts--Quest-----------\n"),
    lists:foreach(fun({Q,P}) -> io:format("  ~4b  ~s\n", [P,Q]) end,
                  Quests).

score() ->
    call_with_default_username(fun(Username) -> score(Username) end).

score(Username) when is_atom(Username) ->
    gen_server:call(?SERVER, {score, Username}).

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

get_challenge(QuestID) ->
    call_with_default_username(fun(Username) -> get_challenge(Username, QuestID) end).

get_challenge(Username, QuestID) when is_atom(Username), is_atom(QuestID) ->
    gen_server:call(?SERVER, {get_challenge, Username, QuestID}).

answer_challenge(ChallengeID, Answer) ->
    gen_server:call(?SERVER, {answer_challenge, ChallengeID, Answer}).

submit(QuestID, SolutionFunOrMod) ->
    call_with_default_username(fun(Username) -> submit(Username, QuestID, SolutionFunOrMod) end).

submit(Username, QuestID, SolutionModule) when is_atom(SolutionModule) ->
    FunName = list_to_atom("quest_"++atom_to_list(QuestID)),
    submit(Username, QuestID, fun(Input) -> SolutionModule:FunName(Input) end);
submit(Username, QuestID, SolutionFun) when is_atom(Username),
                                            is_atom(QuestID),
                                            is_function(SolutionFun,1) ->
    case quest:get_challenge(Username, QuestID) of
        {error,_}=Error -> Error;
        {ChallengeID,Input} ->
            quest:answer_challenge(ChallengeID, SolutionFun(Input))
    end.

set_user(Username) ->
    put(?DEFAULT_USERNAME, Username),
    ok.

call_with_default_username(Continuation) ->
    case get(?DEFAULT_USERNAME) of
	undefined ->
	    io:format("No default user set.  Use quest:set_user(Username).~n");
	Username ->
	    Continuation(Username)
    end.
