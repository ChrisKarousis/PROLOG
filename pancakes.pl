is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|L]):-
   X =< Y,
   is_sorted([Y|L]).

between(L, U, L):-
   L =< U.
between(L, U, X):-
   L < U,
   L1 is L+1,
   between(L1, U, X).


not_first([X1|_], X2):-
   X1 \= X2.

pancakes_dfs(State, Operators, States) :-
   pancakes_dfs(State, [], Operators, [State], States).

pancakes_dfs(State, Operators, Operators, States, States) :-
   is_sorted(State).

pancakes_dfs(State1, SoFarOperators, Operators, SoFarStates, States) :-
   move(State1, State2, Operator),
   \+ member(State2, SoFarStates),
   append(SoFarOperators, [Operator], NewSoFarOperators),
   append(SoFarStates, [State2], NewSoFarStates),
   pancakes_dfs(State2, NewSoFarOperators, Operators, NewSoFarStates, States). 

move(State1, State2, Operator):-
   length(State1, N),
   between(1, N, Operator),
   not_first(State1, Operator),
   append(PancakesStack, [Operator|Rest], State1),
   reverse(PancakesStack, ReversedPancakesStack),
   append([Operator|ReversedPancakesStack], Rest, State2).

pancakes_ids(State, Operators, States):- 
   pancakes_ids_iter(State, Operators, States, 0).

pancakes_ids_iter(State, Operators, States, Lim):-
   ldfs(State, [], Operators, [State], States, Lim),
   writeln(Lim).

pancakes_ids_iter(State, Operators, States, Lim):-
   Lim1 is Lim+1,
   pancakes_ids_iter(State, Operators, States, Lim1),
   writeln(Lim1).

ldfs(State, Operators, Operators, States, States, _):-
   is_sorted(State).

ldfs(State1, SoFarOperators, Operators, SoFarStates, States, Lim):-
   Lim > 0,
   Lim1 is Lim - 1,
   move(State1, State2, Operator),
   \+ member(State2, SoFarStates),
   append(SoFarOperators, [Operator], NewSoFarOperators),
   append(SoFarStates, [State2], NewSoFarStates),
   ldfs(State2, NewSoFarOperators, Operators, NewSoFarStates, States, Lim1).

