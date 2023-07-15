activity(a01, act(0,3)).
activity(a02, act(0,4)).
activity(a03, act(1,5)).
activity(a04, act(4,6)).
activity(a05, act(6,8)).
activity(a06, act(6,9)).
activity(a07, act(9,10)).
activity(a08, act(9,13)).
activity(a09, act(11,14)).
activity(a10, act(12,15)).
activity(a11, act(14,17)).
activity(a12, act(16,18)).
activity(a13, act(17,19)).
activity(a14, act(18,20)).
activity(a15, act(19,20)).

sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   activity(H, act(S, E)),
   Diff is E-S,
   Sum is Diff + Rest.

build_list(ASA, ASP, NP):- 
   findall(I-A-Time,
   (between(1, NP, I),
   findall(X, (member(X-Val, ASA), Val =:= I), A),
   sum_list(A, Time)),
   ASP).

between(L, U, L):-
   L =< U.
between(L, U, X):-
   L < U,
   L1 is L+1,
   between(L1, U, X).

max_list(List, Max) :-
    max_list(List, 0, Max).

max_list([], Max, Max).
max_list([H|T], MaxSoFar, Max) :-
    H > MaxSoFar,
    max_list(T, H, Max).
max_list([H|T], MaxSoFar, Max) :-
    H =< MaxSoFar,
    max_list(T, MaxSoFar, Max).

assignment(NP, MT, ASP, ASA) :-
   findall(A, activity(A, act(_, _)), AIds), % Gather all activities in list AIds
   assign(AIds, NP, MT, ASA, _),  % make the assignment
   build_list(ASA, ASP, NP).    % build ASP list

assign([], _, _, [], []).
assign([AId|AIds], NPersons, MT, [AId-PId|Assignment], [PId|PPIds]) :-
   assign(AIds, NPersons, MT, Assignment,  PPIds),
   max_list(PPIds, N),      % find which PId to add
   (N < NPersons -> NewNewJobs is N+1; NewNewJobs is NPersons),
   between(1, NewNewJobs, PId), % Select a person PId for activity AId
   activity(AId, act(Ab, Ae)),
   findall(A, member(A - PId, Assignment), APIds), % Gather in list APIds so far activities of PId  
   Diff is Ae-Ab,
   valid(Ab, Ae, Diff, MT, APIds). % Is current assignment consistent with previous ones?
   
valid(_, _, _, _, []).
valid(Ab1, Ae1, CurMT, MT, [APId|APIds]) :-
   activity(APId, act(Ab2, Ae2)),
   NewCurMT is CurMT+Ae2-Ab2,
   (Ab1 > Ae2 ; Ae1 < Ab2),     % check if it is valid
   NewCurMT=<MT,
   valid(Ab1, Ae1, NewCurMT, MT, APIds).