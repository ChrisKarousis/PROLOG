%ta prwta paradeigmata moy ta trexei poly grhgora
%omws ta teleftaia pairnoun para polu wra
%paroti akolouthisa tis kateuthunseis pou dothikan sthn dialeksi


:- lib(gfd).
:- compile(skyscr_data).

transpose([], []).
transpose([[]|_], []).
transpose(List, [R|Rest]) :-
    transpose2(List, R, NewList),
    transpose(NewList, Rest).

transpose2([], [], []).
transpose2([[H|T]|Tail], [H|R], [T|Rest]) :-
    transpose2(Tail, R, Rest).

alldifferentList([]).
alldifferentList([List|Vars]):-
   alldifferent(List),
   alldifferentList(Vars).


maxlist([], _, []).
maxlist([Var|Vars], MaxSoFar, [Var|MaxVars]):-
    Var #>= MaxSoFar,
    maxlist(Vars, Var, MaxVars).

maxlist([Var|Vars], MaxSoFar, [MaxSoFar|MaxVars]):-
    Var #< MaxSoFar,
    maxlist(Vars, MaxSoFar, MaxVars).

modify_list(Vars, MaxVars):-
    maxlist(Vars, 0, MaxVars).


height_constrain_rtol([], []).
height_constrain_rtol([Var|Vars], [L1|List1]):-
   (L1 > 0 ->
   modify_list(Var, MaxVars),
   nvalues(MaxVars, (#=), L1),
   height_constrain_rtol(Vars, List1);
   height_constrain_rtol(Vars, List1)).

reverse_list2([], []).
reverse_list2([X|Rest], Reversed) :-
    reverse_list2(Rest, RestReversed),
    append(RestReversed, [X], Reversed).

reverse_list([], []).
reverse_list([X|L], [Rev|Reversed]):-
    reverse_list2(X, Rev),
    reverse_list(L, Reversed).

skyscr(PuzzleId, Vars):-
   puzzle(PuzzleId, N, List1, List2, List3, List4, Vars),
   Vars #:: 1..N,
   transpose(Vars, TVars),
   alldifferentList(Vars),
   alldifferentList(TVars),
   height_constrain_rtol(Vars, List1),
   reverse_list(Vars, RevVars),
   height_constrain_rtol(RevVars, List2),
   height_constrain_rtol(TVars, List3),
   reverse_list(TVars, RevTVars),
   height_constrain_rtol(RevTVars, List4),
   flatten(Vars, FlatVars),
   search(FlatVars, 0, most_constrained, indomain, complete, []),!.

   
 