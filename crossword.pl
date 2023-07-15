% fc + mrv based on the code of nqueens

between(L, U, L):-
   L =< U.
between(L, U, X):-
   L < U,
   L1 is L+1,
   between(L1, U, X).

find_lists([], []).
find_lists([Row|Rest], [List_Vars|List]):-
    find_row(Row, [], List_Vars),
    find_lists(Rest, List).

find_row([], [], []).
find_row([], L, [L]) :- L \= [].
find_row([Var|Rest], Acc, List_Vars) :-
    var(Var), !,
    append(Acc, [Var], Acc2),
    find_row(Rest, Acc2, List_Vars).
find_row([black|Rest], Acc, [Acc|List_Vars]) :-
    find_row(Rest, [], List_Vars).

flatten_list([], Acc, Acc).
flatten_list([H|T], Acc, L2) :-
    flatten_list2(H, Acc, Acc2),
    flatten_list(T, Acc2, L2).

flatten_list2([], Acc, Acc).
flatten_list2([H|T], Acc, L2) :-
    append(Acc, [H], Acc2),
    flatten_list2(T, Acc2, L2).

create_square(X,Y,Var) :-
    (black(X,Y) -> Var = black; Var=_). 

create_row(X,Row) :-
    dimension(Dim),
    findall(Var, (between(1,Dim,Y), create_square(X,Y,Var)), Row).

remove_squares([], []).
remove_squares([L1|Rest], [L1|List]):-
    length(L1, N),
    N > 1,!,
    remove_squares(Rest, List).
remove_squares([_|Rest], List):-
    remove_squares(Rest, List).

transpose([], []).
transpose([[]|_], []).
transpose(M, [R|Rs]) :-
    transpose2(M, R, Ms),
    transpose(Ms, Rs).

transpose2([], [], []).
transpose2([[H|T]|Ms], [H|R], [T|Ts]) :-
    transpose2(Ms, R, Ts).

find_names([], []).
find_names([Ascii|X], [Head|S]):-
    name(Ascii, Head),
    find_names(X, S).

find_domain([], _, []).
find_domain([S|Rest], N, [S|S1]):-
    length(S, N1),
    N == N1,
    find_domain(Rest, N, S1).

find_domain([S|Rest], N, S1):-
    length(S, N1),
    N =\= N1,
    find_domain(Rest, N, S1).

combine_soldom([], _, []).
combine_soldom([Var|Vars], S, [Var-S1|SolDom]) :-
    length(Var, N),
    find_domain(S, N, S1),
    combine_soldom(Vars, S, SolDom).

update_domains(_, [], []).
update_domains(X, [Y-Domain1|SolDom1], [Y-Domain2|SolDom2]) :-
   update_domain(X, Y, Domain1, Domain2), 
   update_domains(X, SolDom1, SolDom2).

update_domain(X, Y, Domain1, Domain4) :-
   findall(Y, member(Y, Domain1), Domain2),
   remove_if_exists(X, Domain2, Domain4).

remove_if_exists(_, [], []).
remove_if_exists(X, [X|List], List) :-
    !.
remove_if_exists(X, [Y|List1], [Y|List2]) :-
   remove_if_exists(X, List1, List2).

generate_solution_with_fc_mrv([]).
generate_solution_with_fc_mrv(SolDom1) :-
   mrv_var(SolDom1, X-Domain, SolDom2),
   member(X, Domain),
   update_domains(X, SolDom2, SolDom3),
   generate_solution_with_fc_mrv(SolDom3). 

mrv_var([X-Domain], X-Domain, []).
mrv_var([X1-Domain1|SolDom1], X-Domain, SolDom3) :-
   mrv_var(SolDom1, X2-Domain2, SolDom2),
   length(Domain1, N1),
   length(Domain2, N2),
   (N1 < N2 ->
      (X = X1,
       Domain = Domain1,
       SolDom3 = SolDom1) ;
      (X = X2,
       Domain = Domain2,
       SolDom3 = [X1-Domain1|SolDom2])).

crossword(Solution) :-
    dimension(Dim),
    findall(Row, (between(1,Dim,X), create_row(X,Row)), Grid), % create Grid
    find_lists(Grid, List1),
    flatten_list(List1, [], New_List1), % flatten list
    remove_squares(New_List1, L1), % remove unwanted squares such as [] and single variables
    transpose(Grid, TransposeGrid),
    find_lists(TransposeGrid, List2),
    flatten_list(List2, [], New_List2),
    remove_squares(New_List2, L2),
    append(L1, L2, Variables),
    words(X),
    find_names(X, S),
    combine_soldom(Variables, S, SolDom),
    generate_solution_with_fc_mrv(SolDom),
    find_names(Solution, Variables),
    print_cross(Grid,1),!.

print_cross([], _).
print_cross([Row|Grid], Y) :- 
    print_cross_row(Row, 1, Y),
    NewY is Y+1,
    print_cross(Grid, NewY).

print_cross_row([], _, _) :- writeln("").
print_cross_row([Var|Rest], X, Y):-
    (black(Y, X) -> write("###") ; (name(Ascii, [Var]), write(" "), write(Ascii), write(" "))),
    NewX is X+1,
    print_cross_row(Rest, NewX, Y).