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
      ["Given a boolean expression of the grammar:",
       "expr ::= a | b | c       % Variables",
       "       | true | false    % Constants",
       "       | {not, <expr>} | {or, <expr>, <expr>} | {and, <expr>, <expr>}",
       "construct the truth table for the expression, containing the value",
       "of the expression for each of the 8 truth assignments to a, b and c.",
       "Examples:",
       "  a         -> [false,true,false,true,false,true,false,true]",
       "  {not,c}   -> [true,true,true,true,false,false,false,false]",
       "  {and,a,b} -> [false,false,false,true,false,false,false,true]."]}
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

boolean_evaluator() ->
    #quest{generate=fun() -> rnd_bool_exp(3+rnd_integer(10)) end,
           verify=fun(Exp,Answer) -> verify_boolean_evaluations(Exp,Answer) end}.
verify_boolean_evaluations(Exp, Answer) ->
    TF = [true,false],
    Inputs = [{A,B,C} || A <- TF,
                         B <- TF,
                         C <- TF],
    (catch length(Answer))==8
        andalso lists:all(fun erlang:is_boolean/1, Answer)
        andalso lists:all(fun ({I,O})->verify_boolean_evaluation(Exp,I,O) end,
                          lists:zip(Inputs, Answer)).
verify_boolean_evaluation(C, _, V) when is_boolean(C) -> C=:=V;
verify_boolean_evaluation(a, {A,_,_}, V) -> V =:= A;
verify_boolean_evaluation(b, {_,B,_}, V) -> V =:= B;
verify_boolean_evaluation(c, {_,_,C}, V) -> V =:= C;
verify_boolean_evaluation({'not', E}, In, Out) ->
    verify_boolean_evaluation(E, In, not Out);
verify_boolean_evaluation({'or', E1, E2}, In, true) ->
    verify_boolean_evaluation(E1, In, true) orelse verify_boolean_evaluation(E2, In, true);
verify_boolean_evaluation({'or', E1, E2}, In, false) ->
    verify_boolean_evaluation(E1, In, false) andalso verify_boolean_evaluation(E2, In, false);
verify_boolean_evaluation({'and', E1, E2}, In, true) ->
    verify_boolean_evaluation(E1, In, true) andalso verify_boolean_evaluation(E2, In, true);
verify_boolean_evaluation({'and', E1, E2}, In, false) ->
    verify_boolean_evaluation(E1, In, false) orelse verify_boolean_evaluation(E2, In, false).


%%%==================== Common generators ==============================

rnd_integer() ->
    crypto:rand_uniform(-100,100).

rnd_integer(N) ->
    crypto:rand_uniform(1,N+1).

semi_bignum() ->
    <<ID:64>> = crypto:rand_bytes(8),
    ID.

rnd_of(L) ->
    lists:nth(rnd_integer(length(L)), L).

rnd_bool_exp(0) ->
    rnd_of([false, true, a, b, c]);
rnd_bool_exp(Sz) when Sz > 0 ->
    case rnd_of(['not', 'and', 'or']) of
        'not' -> {'not', rnd_bool_exp(Sz-1)};
        BinOp ->
            LeftSz = rnd_integer(Sz)-1,
            RightSz = Sz-1-LeftSz,
            {BinOp, rnd_bool_exp(LeftSz), rnd_bool_exp(RightSz)}
    end.


