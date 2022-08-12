specifics(Yes, No, D1,D2,D3,D4,D5,D6,D7,D8) :-
    Yes = [1,3,5,0,*,=],
    No = [2,4,6,7,8,9,+,-,/],
    not_in([9,1,7,3], D1),
    not_in([2,3,1], D2),
    D3 = (*),
    not_in([6,7,5], D4),
    D5 = (=),
    D6 = 1,
    D7 = 5,
    not_in([4,6,1,5], D8).

solve(S) :-
    Ss = [D1,D2,D3,D4,D5,D6,D7,D8],
    specifics(Yes, No, D1,D2,D3,D4,D5,D6,D7,D8),
    expr(Yes, No, Ss),
    atomic_list_concat(Ss, S).

all_syms([=,+,-,*,/, 1,2,3,4,5,6,7,8,9,0]).

expr(Yes, No, Ss) :-
    length(Ss, 8),
    possible(No, Yes, Possible),
    maplist(in(Possible), Ss),
    append(LeftSs, [=|RightSs], Ss),
    \+ member(=, RightSs),
    chars_term(LeftSs, Left),
    chars_term(RightSs, Right),
    catch(Left =:= Right, _, fail).

possible(No, Yes, Possible) :-
    all_syms(Possible0),
    include(not_in(No), Possible0, Possible),
    foldl(select, Yes, Possible, _).

chars_term(Chars, Term) :-
    Chars \= [],
    Chars \= [0|_],
    atomic_list_concat(Chars, String),
    catch(term_string(Term, String), _, fail).

not_in(NotList, X) :-
    freeze(X, \+ member(X, NotList)).

in(List, X) :-
    member(X, List).

