% ?- time((between(1,6,N), solve(N, E, 999, [1,3,5,10,25,50], _), fail; true)).

% solve(+Integer, -Term, +Integer, +List, -List)
solve(1, N, N, P, Q) :- !, select(N, P, Q).
solve(K, G, N, P, Q) :-
   J is K-1,
   between(1, J, I),
   L is K-I,
   solve2(I, E, A, P, H),
   forward(E, A, F, B, G, N),
   solve(L, F, B, H, Q).

forward(E, A, F, B, E+F, N) :- N > A, B is N-A, A =< B.
forward(E, A, F, B, E-F, N) :- A > N, B is A-N.
forward(E, A, F, B, E*F, N) :- N mod A =:= 0, B is N div A, A =< B.
forward(E, A, F, B, E/F, N) :- A mod N =:= 0, B is A div N.

% solve2(+Integer, -Term, -Integer, +List, -List)
solve2(1, N, N, P, Q) :- !, select(N, P, Q).
solve2(K, G, N, P, Q) :-
   J is K-1,
   between(1, J, I),
   L is K-I,
   solve2(I, E, A, P, H),
   solve2(L, F, B, H, Q),
   combine(E, A, F, B, G, N).

combine(E, A, F, B, E+F, N) :- A =< B, N is A+B.
combine(E, A, F, B, E-F, N) :- A > B, N is A-B.
combine(E, A, F, B, E*F, N) :- A =< B, N is A*B.
combine(E, A, F, B, E/F, N) :- A mod B =:= 0, N is A div B.
