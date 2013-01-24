-module(quest_quests).
-compile(export_all).

-include("quest_quests.hrl").

%%%===================================================================
%%% Quests
%%%===================================================================

%%% Quest spec: {QuestID, LevelRequired, PointsWorth, Description}.
quest_list() ->
    [{any_answer,         0, 1, "Answer with any value whatsoever."},
     {any_pid,            1, 1, "Answer with a value of type pid."},
     {any_reference,      1, 1, "Answer with a value of type reference."},
     {answer_the_input,   1, 1, "Answer with the input given in the challenge."},
     {list_of_length_10,  2, 1, "Answer with any list of length 10."},
     {impure_list,        4, 2, "Answer with any impure list."},
     {sum_of_numbers,     8, 3, "Given a list of numbers, answer with their sum."},
     {even_count,         8, 3, "Given a list of integers, answer how many of them are even."},
     {tuple_swap,         8, 2, "Given a pair (2-tuple), answer with a pair with the same elements, but swapped."},
     {base_7,            14, 5, "Given an integer, answer with a string containing the base 7 representation of the number."},
     {tuple_rotate,      20, 5, "Given a tuple of an unknown arity, rotate the elements one place to the left."},
     {primality_check,   20, 7, "Given a list of integers, answer with a list of booleans indicating whether the corresponding number is a prime."},
     {boolean_evaluator, 30, 15,
      "Given a boolean expression of the grammar:\n"++
          "expr ::= a | b | c       % Variables\n"++
          "       | true | false    % Constants\n"++
          "       | {not, <expr>} | {or, <expr>, <expr>} | {and, <expr>, <expr>}\n"++
          "construct the truth table for the expression, containing the value\n"++
          "of the expression for each of the 8 truth assignments to a, b and c.\n"++
          "Examples:\n"++
          "  a         -> [false,true,false,true,false,true,false,true]\n"++
          "  {not,c}   -> [true,true,true,true,false,false,false,false]\n"++
          "  {and,a,b} -> [false,false,false,true,false,false,false,true].\n"}
     ].

any_answer() ->
    #quest{generate=fun()->dummy end,
           verify=fun(_,_)->true end}.

any_pid() ->
    #quest{generate=fun()->dummy end,
           verify=fun(_,Answer) -> is_pid(Answer) end}.

any_reference() ->
    #quest{generate=fun()->dummy end,
           verify=fun(_,Answer) -> is_reference(Answer) end}.

list_of_length_10() ->
    #quest{generate=fun()->dummy end,
           verify=fun(_,Answer) -> catch(length(Answer))==10 end}.

impure_list() ->
    #quest{generate=fun()->dummy end,
           verify=fun(_,Answer) -> is_impure_list(Answer,0) end}.
is_impure_list([],_) -> false;
is_impure_list([_|T],N) -> is_impure_list(T,N+1);
is_impure_list(_,N) -> N>0.

answer_the_input() ->
    #quest{generate=fun()->semi_bignum() end,
           verify=fun(Input,Answer) -> Answer=:=Input end}.

sum_of_numbers() ->
    #quest{generate=fun()->[rnd_integer() || _ <- lists:seq(1,5+rnd_integer(15))] end,
           verify=fun(Input,Answer) -> Answer==lists:sum(Input) end}.

%%%==================== Common generators ==============================

rnd_integer() ->
    crypto:rand_uniform(-100,100).

rnd_integer(N) ->
    crypto:rand_uniform(1,N).

semi_bignum() ->
    <<ID:64>> = crypto:rand_bytes(8),
    ID.
