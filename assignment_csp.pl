:- lib(ic).
:- lib(branch_and_bound).


sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   activity(H, act(S, E)),
   Diff is E-S,
   Sum is Diff + Rest.

between(L, U, L):-
   L =< U.
between(L, U, X):-
   L < U,
   L1 is L+1,
   between(L1, U, X).

valid(_, _, [], _, _, _).
valid(Ab1, Ae1, [APId|APIds], L, I, J) :-
   activity(APId, act(Ab2, Ae2)),
   (Ab1 > Ae2 ; Ae1 < Ab2),     % check if it is valid
   NewJ is J+1,
   valid(Ab1, Ae1, APIds, L, I, NewJ).

valid(Ab1, Ae1, [APId|APIds], L, I, J) :-
   activity(APId, act(Ab2, Ae2)),
   (Ab1 =< Ae2 , Ae1 >= Ab2),     % check if it not is valid
   constrain_element2(L, I, J),
   NewJ is J+1,
   valid(Ab1, Ae1, APIds, L, I, NewJ).

constrain_element2(L, I, J):-
    element(I, L, P1),
    element(J, L, P2),
    P1 #\= P2.

getsublist(_, 0, []).
getsublist([R|Row], N, [R|NRow]) :-
    N > 0,
    NN is N - 1,
    getsublist(Row, NN, NRow).

max_list(List, Max) :-
    max_list(List, 0, Max).

max_list([], Max, Max).
max_list([H|T], MaxSoFar, Max) :-
    H #> MaxSoFar,
    max_list(T, H, Max).
max_list([H|T], MaxSoFar, Max) :-
    H #=< MaxSoFar,
    max_list(T, MaxSoFar, Max).

constrain_element(_, [], _).
constrain_element(L, [A|AIds], I):-
    activity(A, act(A1, A2)),
    NewI is I+1,
    Minus is I-1,
    getsublist(L, Minus, Sublist),
    max_list(Sublist, Max),
    element(I, L, Xi),
    Xi #=< Max+1,
    valid(A1, A2, AIds, L, I, NewI),
    constrain_element(L, AIds, NewI).

constrain_sum2([], _, [], _, C, C).
constrain_sum2([I|List], P, [A|AIds], MT, Count, C):-
    activity(A, act(A1, A2)),
    Diff #= (I#=P)*(A2-A1),
    NewCount #= Count+Diff,
    NewCount #=< MT,
    constrain_sum2(List, P, AIds, MT, NewCount, C).

constrain_sum_person(_, [], _, _, []).
constrain_sum_person(L, [P|Persons], AIds, MT, [C|Count]):-
    constrain_sum2(L, P, AIds, MT, 0, C),
    constrain_sum_person(L, Persons, AIds, MT, Count).


create_list(List, N) :-
    findall(X, between(1, N, X), List).

build_asp(ASA, ASP, NP):- 
   findall(I-A-Time,
   (between(1, NP, I),
   findall(X, (member(X-Val, ASA), Val =:= I), A),
   sum_list(A, Time)),
   ASP).

build_asa([], [], []).
build_asa([I|L], [A|AIds], [A - I |ASA]):-
    build_asa(L, AIds, ASA).

assignment_csp(NP, MT, ASP, ASA):-
    findall(A, activity(A, act(_, _)), AIds),
    length(AIds, Len),
    length(L, Len),
    L #:: 1..NP,
    element(1, L, 1),
    constrain_element(L, AIds, 1),
    create_list(Persons, NP),
    constrain_sum_person(L, Persons, AIds, MT, Count),
    search(L, 0, input_order, indomain, complete, []),
    build_asa(L, AIds, ASA),
    build_asp(ASA, ASP, NP).

mysublist(_, [], 0).
mysublist([A1|AllAIds], [A1|AIds], NF):-
    NF > 0,
    NewNF is NF-1,
    mysublist(AllAIds, AIds, NewNF).

calc_total_time([], 0).
calc_total_time([A|AIds], D):-
    activity(A, act(A1, A2)),
    Diff #= A2-A1,
    calc_total_time(AIds, Rest),
    D #= Diff + Rest.

minimize_cost([], _, 0).
minimize_cost([C|Count], A, Cost):-
    Diff #= abs(A - C),
    NewDiff #= Diff * Diff,
    minimize_cost(Count, A, NewCost),
    Cost #= NewDiff + NewCost.

assignment_opt(NF, NP, MT, F, T, ASP, ASA, Cost):-
    findall(A, activity(A, act(_, _)), AllAIds),
    (NF > 0 ->
    mysublist(AllAIds, AIds, NF);
    AIds = AllAIds),
    length(AIds, Len),
    length(L, Len),
    L #:: 1..NP,
    element(1, L, 1),
    constrain_element(L, AIds, 1),
    create_list(Persons, NP),
    constrain_sum_person(L, Persons, AIds, MT, Count),
    calc_total_time(AIds, D),
    Di is D / NP,
    Af is round(Di),
    A is integer(Af),
    minimize_cost(Count, A, Cost),
    bb_min(search(L, 0, input_order, indomain, complete, []), Cost, bb_options{strategy:restart, delta:F, timeout:T, from:1}),
    build_asa(L, AIds, ASA),
    build_asp(ASA, ASP, NP).