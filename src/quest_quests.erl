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
     {which_type,         8, 5, ["Given a list of values, answer with a list",
                                 "stating the type of the corresponding value as follows:",
                                 "  'number' if it is a number",
                                 "  'atom' if it is an atom",
                                 "  'list' if it is a list",
                                 "  'tuple' if it is a tuple",
                                 "  'binary' if it is a binary",
                                 "  'pid' if it is a process ID",
                                 "  'reference' if it is a reference",
                                 "  'closure' if it is a function object"]},
     {which_type2,       15,10, ["Given a list of values, answer with a list",
                                 "stating the type of the corresponding value as follows:",
                                 "  'natural_number' if it is a positive integer",
                                 "  'integer' if it is any other integer",
                                 "  'float' if it is a floating-point number",
                                 "  'boolean' if it is 'true' or 'false'",
                                 "  'atom' if it is any other atom",
                                 "  'nil' if it is the empty list",
                                 "  'string' if it is a non-empty list containing only numbers in [0;255]",
                                 "  'impure_list' if it is an impure",
                                 "  '{list,Length}' if it is any other list and has length Length",
                                 "  '{tuple, Size}' if it is a tuple of size Size",
                                 "  '{binary,Size}' if it is a binary and contains Size bytes",
                                 "  'pid' if it is a process ID",
                                 "  'reference' if it is a reference",
                                 "  '{closure, Arity}' if it is a function object of arity Arity"]},
     {sum_of_numbers,     8, 3, "Given a list of numbers, answer with their sum."},
     {even_count,         8, 3, "Given a list of integers, answer how many of them are even."},
     {tuple_swap,         8, 2, "Given a pair (2-tuple), answer with a pair with the same elements, but swapped."},
     {base_7,            14, 5, "Given a positive integer, answer with a string containing the base 7 representation of the number."},
     {tuple_rotate,      20, 5, "Given a tuple of an unknown arity, rotate the elements one place to the left."},
     {primality_check,   20, 7, "Given a list of integers, answer with a list of booleans indicating whether the corresponding number is a prime."},
     {boolean_evaluator, 30, 15,
      ["Given a boolean expression of the grammar:",
       "expr ::= a | b | c       % Variables",
       "       | true | false    % Constants",
       "       | {'not', <expr>} | {'or', <expr>, <expr>} | {'and', <expr>, <expr>}",
       "construct the truth table for the expression, containing the value",
       "of the expression for each of the 8 truth assignments to a, b and c.",
       "Examples:",
       "  a         -> [false,true,false,true,false,true,false,true]",
       "  {'not',c}   -> [true,true,true,true,false,false,false,false]",
       "  {'and',a,b} -> [false,false,false,true,false,false,false,true]."]},
     {closest_fraction1, 25, 10,
      ["Given a pair {X,N} of a floating-point number X in [0;1] and an integer ",
       "N in [1;100], find the fraction Enumerator/Denominator which is",
       "the closest to X among all fractions with denominator =< N.",
       "If there is more than one answer, pick the one with lowest Denominator.",
       "Answer with the tuple {Enumerator,Denominator}.",
       "Example: {0.36, 6} -> {1,3}."]},
     {closest_fraction2, 30, 15,
      ["Given a pair {X,N} of a floating-point number X in [0;1] and an integer ",
       "N in [1;10^7], find the fraction Enumerator/Denominator which is",
       "the closest to X among all fractions with denominator =< N.",
       "If there is more than one answer, pick the one with lowest Denominator.",
       "Answer with the tuple {Enumerator,Denominator}.",
       "Example: {0.36, 6} -> {1,3}."]}
    ].

any_answer() ->
    #quest{generate=fun()->dummy end,
           verify=fun(_,_)->true end}.

answer_the_input() ->
    #quest{generate=fun()->semi_bignum() end,
           verify=fun(Input,Answer) -> Answer=:=Input end}.

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
           verify=fun(_,Answer) -> is_impure_list(Answer) end}.


%%%----------
which_type() ->
    #quest{generate=fun()-> [rnd_any_type() || _ <- lists:seq(1,20)] end,
           verify=fun(Input,Answer) ->
                          allzipwith(fun correct_type1/2, Input, Answer)
                  end}.
which_type2() ->
    #quest{generate=fun()-> [rnd_any_type() || _ <- lists:seq(1,20)] end,
           verify=fun(Input,Answer) ->
                          allzipwith(fun correct_type2/2, Input, Answer)
                  end}.

correct_type1(X,number) when is_number(X) -> true;
correct_type1(X,atom)   when is_atom(X)   -> true;
correct_type1(X,list)   when is_list(X)   -> true;
correct_type1(X,tuple)  when is_tuple(X)  -> true;
correct_type1(X,binary) when is_binary(X) -> true;
correct_type1(X,pid)    when is_pid(X)    -> true;
correct_type1(X,reference) when is_reference(X) -> true;
correct_type1(X,closure) when is_function(X) -> true;
correct_type1(_,_) -> false.

correct_type2(X,T) when is_integer(X), X>0 -> T == natural_number;
correct_type2(X,T) when is_integer(X)      -> T == integer;
correct_type2(X,T) when is_float(X)        -> T==float;
correct_type2(X,T) when is_boolean(X)      -> T==boolean;
correct_type2(X,T) when is_atom(X)         -> T==atom;
correct_type2(X,T) when X==[]              -> T==nil;
correct_type2(X,T) when is_list(X) ->
    case is_latin1_string(X) of
        true -> T==string;
        false ->
            case is_impure_list(X) of
                true -> T==impure_list;
                false -> T=={list, length(X)}
            end
    end;
correct_type2(X,T) when is_tuple(X)        -> T=={tuple, tuple_size(X)};
correct_type2(X,T) when is_binary(X)       -> T=={binary, byte_size(X)};
correct_type2(X,T) when is_pid(X)          -> T==pid;
correct_type2(X,T) when is_reference(X)    -> T==reference;
correct_type2(X,T) when is_function(X)     ->
    {arity,N} = erlang:fun_info(X, arity),
    T=={closure, N};
correct_type2(_,_) -> false.
%%%----------


sum_of_numbers() ->
    #quest{generate=fun()->[rnd_integer() || _ <- lists:seq(1,5+rnd_integer(15))] end,
           verify=fun(Input,Answer) -> Answer=:=lists:sum(Input) end}.

even_count() ->
    #quest{generate=fun()->[rnd_integer() || _ <- lists:seq(1,10+rnd_integer(30))] end,
           verify=fun(L,Answer) -> Answer=:=length([X||X<-L, X rem 2==0]) end}.

tuple_swap() ->
    #quest{generate=fun()->{char_atom(), rnd_integer(100)} end,
           verify=fun({A,B},Answer) -> Answer=:={B,A} end}.


base_7() ->
    #quest{generate=fun()->rnd_integer(10000) end,
           verify=fun(Input,Answer) -> Answer=:=integer_to_list(Input,7) end}.

tuple_rotate() ->
    #quest{generate=fun()-> L = [char_atom() || _ <- lists:seq(1,5+rnd_integer(20))],
                            list_to_tuple(L)
                    end,
           verify=fun(Input,Answer) ->
                          N = tuple_size(Input),
                          is_tuple(Answer)
                              andalso tuple_size(Answer) == N
                              andalso element(1,Input)==element(N,Answer)
                              andalso lists:all(fun(I) ->
                                                        element(I,Input)==element(I-1,Answer)
                                                end,
                                                lists:seq(2,N))
                  end}.


%%%----------
boolean_evaluator() ->
    #quest{generate=fun() -> rnd_bool_exp(3+rnd_integer(10)) end,
           verify=fun(Exp,Answer) -> verify_boolean_evaluations(Exp,Answer) end}.
verify_boolean_evaluations(Exp, Answer) ->
    FT = [false,true],
    Inputs = [{A,B,C} || C <- FT,
                         B <- FT,
                         A <- FT],
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
%%%----------

closest_fraction1() ->
    #quest{generate=fun() -> gen_closest_fraction_problem(10, 100) end,
           verify=fun({'$remember', Expected, _},Answer) -> Answer=:=Expected end}.
closest_fraction2() ->
    #quest{generate=fun() -> gen_closest_fraction_problem(1000000, 10000000) end,
           verify=fun({'$remember', Expected, _},Answer) -> Answer=:=Expected end}.

gen_closest_fraction_problem(MinN, MaxN) ->
    N = rnd_integer(MinN, MaxN),
    {A0,B0} = {rnd_integer(N),rnd_integer(N)},
    {A1,B1} = {min(A0,B0), max(A0,B0)}, % Keep in [0;1]
    GCD = gcd(A1,B1),
    {A,B} = {A1 div GCD, B1 div GCD}, % Reduce
    Perturbance = (rnd_float()*0.99 - 0.5) / (N*N),
    Input = {A/B + Perturbance, N},
    {'$remember', {A,B}, Input}.

%%%==================== Common list functions ==============================
allzipwith(Fun, L1, L2) when is_function(Fun,2), is_list(L1), is_list(L2) ->
    lists:all(fun({A,B}) -> Fun(A,B) end,
              lists:zip(L1,L2)).

%%%==================== Common math ==============================
gcd(A,B) when B>A -> gcd(B,A);  % Keep A largest.
gcd(A,B) when B<0 -> gcd(A,-B); % Keep B non-negative.
gcd(A,0) -> A;
gcd(A,B) -> gcd(B, A rem B).    % case A>B>0.

%%%==================== Common predicates ==============================

is_impure_list(L) -> is_impure_list(L, 0).

is_impure_list([],_) -> false;
is_impure_list([_|T],N) -> is_impure_list(T,N+1);
is_impure_list(_,N) -> N>0.

is_latin1_string([]) -> true;
is_latin1_string([H|T]) ->
    is_integer(H) andalso
        H>=0 andalso
        H=<16#FF andalso
        is_latin1_string(T);
is_latin1_string(_) -> false.

%%%==================== Common generators ==============================

rnd_integer() ->
    crypto:rand_uniform(-100,100).

rnd_integer(N) ->
    crypto:rand_uniform(1,N+1).

rnd_integer(Min,Max) -> % Inclusive.
    crypto:rand_uniform(Min, Max+1).

rnd_float() ->
    random:uniform().

semi_bignum() ->
    <<ID:64>> = crypto:rand_bytes(8),
    ID.

rnd_any_type() ->
    Gen = rnd_of([fun rnd_integer/0,
                  fun rnd_float/0,
                  fun rnd_atom/0,
                  fun rnd_binary/0,
                  fun erlang:self/0,
                  fun erlang:make_ref/0,
                  fun () -> [rnd_integer(0,255) || _ <- lists:seq(1,rnd_integer(0,10))] end,
                  fun () -> [rnd_primitive_type() || _ <- lists:seq(1,rnd_integer(0,10))] end,
                  fun () -> [rnd_primitive_type() || _ <- lists:seq(1,rnd_integer(0,10))] ++ rnd_primitive_type() end, % Impure list
                  fun () -> list_to_tuple([rnd_atom() || _ <- lists:seq(1,rnd_integer(0,10))]) end,
                  fun () -> rnd_of([fun()->throw(function_not_supposed_to_be_called) end,
                                    fun(_)->throw(function_not_supposed_to_be_called) end,
                                    fun(_,_)->throw(function_not_supposed_to_be_called) end,
                                    fun(_,_,_)->throw(function_not_supposed_to_be_called) end])
                  end
                 ]),
    Gen().

rnd_primitive_type() ->
    Gen = rnd_of([fun rnd_integer/0,
                  fun rnd_float/0,
                  fun rnd_atom/0,
                  fun rnd_binary/0,
                  fun erlang:self/0,
                  fun erlang:make_ref/0]),
    Gen().


rnd_atom() ->
    case rnd_integer(10) of
        0 -> false;
        1 -> true;
        _ -> char_atom()
    end.

rnd_binary() ->
    crypto:rand_bytes(rnd_integer(0,20)).

char_atom() ->
    list_to_atom([$a + rnd_integer(26)-1]).

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


