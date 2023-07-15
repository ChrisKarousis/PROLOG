:- lib(ic).
:- lib(branch_and_bound).
:- lib(ic_global).

create_formula(NVars, NClauses, Density, Formula) :-
   formula(NVars, 1, NClauses, Density, Formula).

formula(_, C, NClauses, _, []) :-
   C > NClauses.
formula(NVars, C, NClauses, Density, [Clause|Formula]) :-
   C =< NClauses,
   one_clause(1, NVars, Density, Clause),
   C1 is C + 1,
   formula(NVars, C1, NClauses, Density, Formula).

one_clause(V, NVars, _, []) :-
   V > NVars.
one_clause(V, NVars, Density, Clause) :-
   V =< NVars,
   rand(1, 100, Rand1),
   (Rand1 < Density ->
      (rand(1, 100, Rand2),
       (Rand2 < 50 ->
        Literal is V ;
        Literal is -V),
       Clause = [Literal|NewClause]) ;
      Clause = NewClause),
   V1 is V + 1,
   one_clause(V1, NVars, Density, NewClause).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

find_c([], _, []).
find_c([Clause|F], S, [Res|C]):-
   eval_clause(Clause, S, ClauseRes),
   Res #= (ClauseRes #> 0),
   find_c(F, S, C).

eval_clause([], _, 0).
eval_clause([Literal|Clause], S, Res):-
   eval_literal(Literal, S, LitRes),
   eval_clause(Clause, S, ClauseRes),
   Res #= LitRes + ClauseRes.

eval_literal(Literal, S, Res):-
   (Literal > 0 -> position(Literal, S, X), Res #= X ;
    abs(Literal, AbsLiteral), position(AbsLiteral, S, X), Res #= 1-X).

position(1, [X|_], X).
position(N, [_|Tail], X):-
   N > 1,
   N1 is N-1,
   position(N1, Tail, X).

maxsat(NV, NC, D, F, S, M):-
   length(S, NV),
   length(C, NC),
   S #:: 0..1,
   C #:: 0..1,
   create_formula(NV, NC, D, F),
   find_c(F, S, C),
   Sum #= NC - sumlist(C),
   bb_min(search(S,0,smallest,indomain_middle,complete,[]), Sum, _),
   M #= sumlist(C).