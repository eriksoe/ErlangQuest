-module(quest).

-export([help/0,
         list/1,
         score/1,
         get_challenge/2,
         answer_challenge/2,
         submit/3
         ]).

-define(SERVER, {global, quest_server}).

help_text() ->
    "\n-=- This is the Erlang Quest. -=-\n" ++
        "Here's what you can do:\n"++
        "* quest:help() -- shows this text.\n"++
        "* quest:list(Username) -- shows the quests currently available to you.\n"++
        "* quest:score(Username) -- shows your accomplishments so far.\n"++
        "* quest:get_challenge(Username, QuestID) -- requests a challenge.\n"++
        "    Returns {ChallengeID, Points, Description, Input}.\n"++
        "* quest:answer_challenge(ChallengeID, Answer) -- answers a challenge.\n"++
        "    Returns {achievement_unlocked, NewScore}\n"++
        "    or      {correct_but_already_achieved, CurrentScore}\n"++
        "    or      wrong_answer\n"++
        "    or      unknown_challenge_id.\n"++
        "\n"++
        "* quest:submit(Username, QuestID, SolutionFun) -- shortcut for\n"++
        "    submitting a solution to a quest.\n"++
        "    Is equivalent to:\n"++
        "      {CID,_,_,Input}=quest:get_challenge(Username, QuestID),\n"++
        "      quest:answer_challenge(CID,SolutionFun(Input))\n"++
        "".

help() ->
    io:format("~s", [help_text()]).

list(Username) when is_atom(Username) ->
    Quests = gen_server:call(?SERVER, {list, Username}),
    io:format("Quests currently available to ~s:\n", [Username]),
    io:format("   Pts  Quest\n"),
    lists:foreach(fun({Q,P}) -> io:format("  ~4b  ~s\n", [P,Q]) end,
                  Quests).

score(Username) when is_atom(Username) ->
    gen_server:call(?SERVER, {score, Username}).

get_challenge(Username, QuestID) when is_atom(Username) -> 'TODO'.
answer_challenge(ChallengeID, Answer) -> 'TODO'.

submit(Username, QuestID, SolutionFun) when is_atom(Username),
                                            is_atom(QuestID),
                                            is_function(SolutionFun,1) ->
    {ChallengeID,_,_,Input}=quest:get_challenge(Username, QuestID),
    quest:answer_challenge(ChallengeID, SolutionFun(Input)).


