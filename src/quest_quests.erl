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
     {min_and_max,        8, 3, ["Given a list of numbers, answer with {Min,Max}, where ",
                                 "Min is the smallest of the numbers and Max is the largest."]},
     {even_count,         8, 3, "Given a list of integers, answer how many of them are even."},
     {tuple_swap,         8, 2, "Given a pair (2-tuple), answer with a pair with the same elements, but swapped."},
     {base_7,            14, 5, "Given a positive integer, answer with a string containing the base 7 representation of the number."},
     {tuple_rotate,      20, 5, "Given a tuple of an unknown arity, rotate the elements one place to the left."},
     {sum_of_digits,     10, 4,
      ["Given a string representing a natural number, answer with the sum ",
       "of the digits.",
       "Example: \"1749\" -> 21"]},
     {rot13,             15, 6,
      ["Given a string, return the string as encrypted with ROT13. ",
       "(See http://en.wikipedia.org/wiki/ROT13). ",
       "Example: \"Hello!\" -> \"Uryyb!\""]},
     {word_count,        15, 7,
      "Given a string, return with the number of words (stretches of A-Z/a-z) it contains."},
     {primality_check,   20, 7, "Given a list of integers, answer with a list of booleans indicating whether the corresponding number is a prime."},
     {add_calculator,    20, 9,
      ["Given a string containing a simple addition expression of the form ",
       "\"A + B\", where A and B are integers, calculate the sum.",
       "The expression may contain spaces.",
       "Examples: \"1+2\" -> 3;     \" 19+  27 \" -> 46"]},
     {postfix_calculator,    20, 9,
      ["Given a string containing an expression in postfix notation ",
       "(containing integers and the operators +, - and *), ",
       "calculate the result.",
       "Examples: \"1 2+\" -> 3;     \" 1 2 3  4 **+ \" -> 25"]},
     {lisp_calculator,    20, 10,
      ["Given a string containing an expression in lisp notation ",
       "(built from integers and the binary operators +, - and *), ",
       "calculate the result.",
       "Examples: \"(+ 1 2)\" -> 3;  \" (+ 100 (- ( * 2 3) 4)) \" -> 102"]},
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
       "Example: {0.36, 6} -> {1,3}."]},
     {fifteen, 25, 15,
      ["A 4x4 array - a list of lists - contains the numbers 1 through 15 and ",
       "the atom 'x', shuffled in some order.",
       "The 'x' denotes a gap - a vacant square. ",
       "Because of this vacancy, the numbers can be shuffled around: ",
       "a number which is neighbour to the gap can trade places with it.",
       "Given such a 4x4 array, find a sequence of such steps which, when ",
       "applied, result in the array looking like this:",
       "  [[ 1, 2, 3, 4],",
       "   [ 5, 6, 7, 8],",
       "   [ 9,10,11,12],",
       "   [13,14,15, x]]",
       "Answer with the sequence in which the numbers are to be moved ",
       "(a list of numbers).",
       "Example:",
       "  [[ 1, 2, 3, 4],",
       "   [ 5, 6, 8, 11],",
       "   [ 9,10,7,x],",
       "   [13,14,15, 12]] -> [11,8,7,11,12]"]},
     {labyrinth, 40, 15,
      ["Given is a 2D array - a list of N lists of each M characters,",
       "each of which are either ' ' or 'x'.",
       "This array represents a labyrinth where ' ' represents a traversable ",
       "cell and 'x' represents a blocked cell.",
       "The cells are named {X,Y} where X is the (1-based) index of the ",
       "inner list and Y is the index of the outer list.",
       "Find a path from the top (Y=1) to the bottom (Y=N) consisting of ",
       "traversable cells which are pairwise neighbours either horizontally ",
       "or vertically.  Answer with the path on the type [{X,Y}], where each ",
       "{X,Y} is the coordinate of a cell on the path.",
       "Example: ",
       "  [\"x xx \",",
       "   \"x    \",",
       "   \"xx xx\"]   -> [{2,1}, {2,2}, {3,2}, {3,3}}]"
      ]}
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

min_and_max() ->
    #quest{generate=fun()->[rnd_integer() || _ <- lists:seq(1,10+rnd_integer(10))] end,
           verify=fun(Input,{Min,Max}) ->
                          lists:member(Min, Input)
                              andalso
                          lists:member(Max, Input)
                              andalso
                              lists:all(fun(X) -> X>= Min andalso X =< Max end,
                                        Input)
                  end}.

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

sum_of_digits() ->
    #quest{generate=fun() -> L = [rnd_integer(0,9)
                                  || _ <- lists:seq(1, rnd_integer(8,20))],
                             S = [X+$0 || X <- L],
                             {'$remember', L, S}
                    end,
           verify = fun({'$remember', L, _}, Answer) ->
                            Answer =:= lists:sum(L)
                    end}.

rot13() ->
    #quest{generate=fun()->rnd_sentence() end,
           verify=fun(Input,Answer) ->
                          length(Input)==length(Answer)
                              andalso
                              lists:all(fun({In,Out}) ->
                                                verify_rot13_chars(In,Out)
                                        end,
                                        lists:zip(Input, Answer))
                  end}.

verify_rot13_chars(In,Out) ->
    InIsAlpha = is_alpha_char(In),
    OutIsAlpha = is_alpha_char(Out),
    InIsAlpha==OutIsAlpha
        andalso
        if InIsAlpha ->
                abs(In-Out)=:=13;
           true ->
                In=:=Out
        end.

word_count() ->
    #quest{generate=fun() -> {S,N} = rnd_sentence_x(),
                             {'$remember', N, S}
                    end,
           verify=fun({'$remember', N, _}, Answer) -> Answer=:=N end}.


%%%---------- Calculators:
add_calculator() ->
    #quest{generate=fun() -> A = rnd_integer(100,100000),
                             B = rnd_integer(100,100000),
                             S = rnd_spaces(0,3)
                                 ++ integer_to_list(A)
                                 ++ rnd_spaces(0,3)
                                 ++ "+"
                                 ++ rnd_spaces(0,3)
                                 ++ integer_to_list(B)
                                 ++ rnd_spaces(0,3),
                             {'$remember', A+B, S}
                    end,
           verify=fun({'$remember', N, _}, Answer) -> Answer=:=N end}.

postfix_calculator() ->
    #quest{generate=fun() ->
                            LeafFun = fun(X)-> {integer_to_list(X),X} end,
                            BinopFun = fun(Op, {E1,R1}, {E2,R2}) ->
                                               case Op of
                                                   '+' -> OpS="+", R=R1+R2;
                                                   '-' -> OpS="-", R=R1-R2;
                                                   '*' -> OpS="*", R=R1*R2
                                               end,
                                               E = E1++rnd_spaces(1,2)++E2++rnd_spaces(0,2)++OpS,
                                               {E,R}
                                       end,
                            {Exp,Res} = rnd_arith_exp(rnd_integer(10,20),
                                                      LeafFun, BinopFun),
                            {'$remember', Res, Exp}
                    end,
           verify=fun({'$remember', N, _}, Answer) -> Answer=:=N end}.

lisp_calculator() ->
    #quest{generate=fun() ->
                            LeafFun = fun(X)-> {integer_to_list(X),X} end,
                            BinopFun = fun(Op, {E1,R1}, {E2,R2}) ->
                                               case Op of
                                                   '+' -> OpS="+", R=R1+R2;
                                                   '-' -> OpS="-", R=R1-R2;
                                                   '*' -> OpS="*", R=R1*R2
                                               end,
                                               E = "("++rnd_spaces(0,2)
                                                   ++OpS++rnd_spaces(1,2)
                                                   ++E1++rnd_spaces(1,2)
                                                   ++E2++rnd_spaces(0,2)
                                                   ++")",
                                               {E,R}
                                       end,
                            {Exp,Res} = rnd_arith_exp(rnd_integer(10,20),
                                                      LeafFun, BinopFun),
                            {'$remember', Res, Exp}
                    end,
           verify=fun({'$remember', N, _}, Answer) -> Answer=:=N end}.

rnd_arith_exp(0, LeafFun, _BinopFun) ->
    LeafFun(rnd_integer(0,100));
rnd_arith_exp(Sz, LeafFun, BinopFun) when Sz > 0 ->
    BinOp = rnd_of(['+', '-', '*']),
    LeftSz = rnd_integer(Sz)-1,
    RightSz = Sz-1-LeftSz,
    BinopFun(BinOp,
             rnd_arith_exp(LeftSz , LeafFun, BinopFun),
             rnd_arith_exp(RightSz, LeafFun, BinopFun)).

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

%%%----------
fifteen() ->
    #quest{generate=fun() -> make_fifteen(1) end,
           verify=fun verify_fifteen/2}.

make_fifteen(NIter) ->
    S0 = lists:foldl(fun(I,D) ->
                            dict:store(fifteen_number_to_point(I), I, D)
                     end,
                     dict:new(),
                     lists:seq(1,15)),
    {S1,_EndGapX,_EndGapY} =
        lists:foldl(fun(_,{D,GapX,GapY}) ->
                            Dir = rnd_integer(1,4),
                            DX = element(Dir, {0,1,0,-1}),
                            DY = element(Dir, {1,0,-1,0}),
                            NewX = GapX + DX,
                            NewY = GapY + DY,
                            if NewX>=0, NewX<4,
                               NewY>=0, NewY<4 ->
                                    D2 = fifteen_move({NewX,NewY}, {GapX,GapY}, D),
                                    {D2, NewX, NewY};
                               true -> % Out of bounds - No change
                                    {D,GapX,GapY}
                            end
                    end,
                    {S0, 3,3},
                    lists:seq(1, NIter)),
    dict_to_fifteen_board(S1).

verify_fifteen(Board, Path) ->
    lists:all(fun(P) when is_integer(P), P>=1, P=<15 -> true;
                 (_) -> false
              end, Path)
        orelse throw(bad_answer_type),
    {S0,_} =
        lists:foldl(fun(V,{D,I}) ->
                            {dict:store(V, fifteen_number_to_point(I), D),
                             I+1}
                    end,
                    {dict:new(),1},
                    lists:concat(Board)),
    S1 = lists:foldl(fun(SwapV, D) ->
                             GapPos = dict:fetch(x, D),
                             SwapPos = dict:fetch(SwapV, D),
                             are_neighbours_2D(GapPos, SwapPos) orelse
                                 throw(bad_move),
                             dict:store(x, SwapPos, dict:store(SwapV, GapPos, D))
                     end,
                     S0,
                     Path),
    Positions = lists:sort(dict:to_list(S1)),
    Expected = [{V, {(V-1) rem 4, (V-1) div 4}}
                || V <- lists:seq(1,15)]
        ++ [{x,{3,3}}],
    Positions =:= Expected.

dict_to_fifteen_board(D) ->
    [[case dict:find({X,Y}, D) of
          {ok, V} -> V;
          error -> x
      end
      || X <- lists:seq(0,3)]
     || Y <- lists:seq(0,3)].

fifteen_number_to_point(I) ->
    X = (I-1) rem 4,
    Y = (I-1) div 4,
    {X,Y}.

fifteen_move(From, To, D) ->
    V = dict:fetch(From, D),
    dict:store(To, V, dict:erase(From, D)).

%%%----------

labyrinth() ->
    #quest{generate=fun() -> make_labyrinth(40) end,
           verify=fun verify_labyrinth_path/2}.

verify_labyrinth_path(Labyrinth0, Path) ->
    Labyrinth = list_to_tuple([list_to_tuple(L) || L <- Labyrinth0]),
    lists:all(fun (P) -> labyrinth_cell_is_traversable(P, Labyrinth) end,
              Path)
        andalso
        verify_labyrinth_path_top(Path, Labyrinth).

verify_labyrinth_path_top([{_,Y} | _]=Path, Labyrinth) ->
    Y=:=1 andalso verify_labyrinth_path_middle(Path, Labyrinth).
verify_labyrinth_path_middle([{X1,Y1} | [{X2,Y2}|_]=Rest], Labyrinth) ->
    (abs(X2-X1) + abs(Y2-Y1))==1
        andalso verify_labyrinth_path_middle(Rest, Labyrinth);
verify_labyrinth_path_middle([P], Labyrinth) ->
    verify_labyrinth_path_bottom(P, Labyrinth).
verify_labyrinth_path_bottom({_,Y}, Labyrinth) ->
    Y=:=tuple_size(Labyrinth).


labyrinth_cell_is_traversable({X,Y}, Labyrinth) ->
    catch(element(X,element(Y,Labyrinth)))==$\s.

make_labyrinth(N) ->
    UF0 = uf_insert(top, uf_insert(bottom, uf_new())),
    UF1 = lists:foldl(fun uf_insert/2,
                      UF0,
                      [{X,Y} || X <- lists:seq(1,N), Y <- lists:seq(0,N+1)]),
    UF2 = lists:foldl(fun(X,UF) -> uf_join({X,0}, top, uf_join({X,N+1}, bottom, UF)) end,
                      UF1,
                      lists:seq(1,N)),
    FreeSet = unblock_random_until_passage(N, UF2, sets:new()),
    labyrinth_freeset_to_array(FreeSet, N, N).

unblock_random_until_passage(N, UF, FreeCells) ->
    X = rnd_integer(N),
    Y = rnd_integer(N),
    P = {X,Y},
    F = fun(P2={_,Y2},U) -> uf_join_when(P,P2,U, (Y2==0 orelse Y2==N+1) orelse sets:is_element(P2, FreeCells)) end,
    UF2 = F({X+1,Y}, F({X-1,Y}, F({X,Y+1}, F({X,Y-1}, UF)))),
    FreeCells2 = sets:add_element(P, FreeCells),
    case uf_same(top, bottom, UF2) of
        true ->
            FreeCells2;
        false ->
            unblock_random_until_passage(N, UF2, FreeCells2)
    end.

labyrinth_freeset_to_array(FreeSet, XMax, YMax) ->
    [[case sets:is_element({X,Y}, FreeSet) of
          true -> $\s;
          false -> $x
      end
      || X <- lists:seq(1,XMax)]
     || Y <- lists:seq(1,YMax)].

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

is_alpha_char(C) ->
    is_integer(C) andalso
    (($a =< C andalso C =< $z) orelse
     ($A =< C andalso C =< $Z)).

are_neighbours_2D({X1,Y1}, {X2,Y2}) when is_integer(X1),
                                         is_integer(Y1),
                                         is_integer(X2),
                                         is_integer(Y2) ->
    abs(X1-X2) + abs(Y1-Y2) =:= 1.
%%%==================== Union-find data structure ====================
uf_new() -> dict:new().

uf_insert(Node,D) -> dict:store(Node, Node, D).

uf_same(Node1, Node2, D) ->
    uf_canon(Node1, D) == uf_canon(Node2, D).

uf_canon(Node, D) ->
    case dict:find(Node,D) of
        {ok,Node} -> Node; % Found a root.
        {ok,CloserToRoot} -> uf_canon(CloserToRoot,D);
        error -> error({uf_canon_called_on_unknown_node, Node, dict:to_list(D)})
    end.

uf_join_when(A, B, UF, Condition) ->
    case Condition of
        true -> uf_join(A, B, UF);
        false -> UF
    end.

uf_join(Node1, Node2, D) ->
    Canon1 = uf_canon(Node1, D),
    Canon2 = uf_canon(Node2, D),
    {A,B} = {min(Canon1,Canon2),
             max(Canon1,Canon2)},
    if A=:=B -> D;
       true ->
            %% Let A be new root (we prefer atoms over references :-))
            %% TODO: Performance could be better if B were chosen more intelligently.
            dict:store(Node1, A, dict:store(Node2, A, dict:store(B, A, D)))
    end.

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

rnd_bool() ->
    crypto:rand_uniform(0,2)==1.

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

rnd_word() ->
    [rnd_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
     || _ <- lists:seq(1, rnd_integer(1,10))].

rnd_word_separator() ->
    [rnd_of(" !?,.;:-\n\t")
     || _ <- lists:seq(1, rnd_integer(1,7))].

rnd_spaces(Min,Max) ->
    [$\s || _ <- lists:seq(1, rnd_integer(Min,Max))].

rnd_sentence() ->
    {S,_} = rnd_sentence_x(),
    S.
rnd_sentence_x() ->
    NWords1 =rnd_integer(10,20),
    NWords2 = rnd_integer(0,1),
    S = lists:flatten([[rnd_word_separator() || rnd_bool()],
                       [rnd_word() ++ rnd_word_separator()
                        || _ <- lists:seq(1, NWords1)],
                       [rnd_word() || NWords2>0]]),
    {S, NWords1+NWords2}.

