specifics(MinMax, D1,D2,D3,D4,D5,D6,D7,D8) :-
    % Yes = [1,3,5,0,*,=],
    % No = [2,4,6,7,8,9,+,-,/],
    MinMax = minmax{'=':(1,1),
                    '+':(0,0),
                    '-':(0,0),
                    '/':(0,0),
                    '*':(1,1),
                    0:(1,8),
                    1:(1,8),
                    2:(0,0),
                    3:(1,8),
                    4:(0,0),
                    5:(1,8),
                    6:(0,0),
                    7:(0,0),
                    8:(0,0),
                    9:(0,0)},
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
    specifics(MinMax, D1,D2,D3,D4,D5,D6,D7,D8),
    expr(MinMax, Ss),
    atomic_list_concat(Ss, S).

all_syms([=,+,-,*,/, 1,2,3,4,5,6,7,8,9,0]).

expr(MinMax, Ss) :-
    all_syms(AllSyms),
    include(no(MinMax), AllSyms, No),
    include(yes(MinMax), AllSyms, Yes),
    length(Ss, 8),
    possible(No, Yes, Possible),
    maplist(in(Possible), Ss),
    append(LeftSs, [=|RightSs], Ss),
    \+ member(=, RightSs),
    chars_term(LeftSs, Left),
    chars_term(RightSs, Right),
    catch(Left =:= Right, _, fail).

min(MinMax, D, Min) :- (Min,_) = MinMax.D.
max(MinMax, D, Max) :- (_,Max) = MinMax.D.
no(MinMax, D) :- (0,0) = MinMax.D.
yes(MinMax, D) :- (1,_) = MinMax.D.

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

