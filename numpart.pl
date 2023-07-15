:- lib(ic).
:- lib(ic_global).

% to teleftaio paradeigma mou pairnei ~460secs sto tkeclipse
% kai 235secs sta linux

numpart(N, L1, L2) :-
    Len #= N/2,
    length(L1, Len),
    length(L2, Len),
    L1 #:: 1..N,
    element(1, L1, 1),
    constrain(L1, N),
    search(L1, 0, occurrence, indomain, complete, []),
    construct(L1, L2, N, 1).

square_sum([], 0).
square_sum([X|Rest], Sum) :-
    square_sum(Rest, Sum1),
    Sum #= eval(Sum1 + X*X).

constrain(L1, N):-
    ic:alldifferent(L1),
    ordered_sum(L1, Sum1),
    S1 is (N*(N+1)//4),
    Sum1 #= S1,
    square_sum(L1, Sum2),
    S2 is (N*(N+1)*(2*N+1)//12),
    Sum2 #= S2.


construct(_, _, N, M) :- 
    M > N.
construct([Num|L1], L2, N, Num):-
    NewNum is Num+1, !,
    construct(L1, L2, N, NewNum).

construct(L1, [Num|L2], N, Num):-
    NewNum is Num+1,
    construct(L1, L2, N, NewNum).
