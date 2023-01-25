% Try to figure out best 2 first guesses
% You can use it:
%   forall(two_guesses(CsS, DsS, EsS), format("~q~n", [[CsS, EsS, DsS]])).
% The output is 1.7GB, so there should be some additional heuristics to
% make it smaller.
% Note that the ordering is a bit strange (i.e., doesn't start with first
% guesses starting with "0").
%   First and last results:
%     ["3*8-4=20", "6/1+9=15", "1+56/7=9"]
%     ["3*8-0=24", "9+7/1=16", "56/7+1=9"]
%     ["4*7-8=20", "6/1+9=15", "1+36/9=5"]
%     ["4/2+8=10", "9*9-6=75", "57-9*6=3"]
%     ["5*8-1=39", "6/2+7=10", "42/6+0=7"]
%     ["5*9-8=37", "8+4/2=10", "20/4+1=6"]
%     ["6*7-2=40", "5/1+8=13", "18/9+3=5"]
%     ["6+8/2=10", "9*7-9=54", "39-7*5=4"]
%     ["7*4-8=20", "6/1+9=15", "1+36/9=5"]
%     ["9*4-6=30", "5/1+7=12", "1+28/7=5"]
%     ["9*8-67=5", "0+12/4=3", "8+9/3=11"]
%     ["0+12/3=4", "50-6*7=8", "8*9-5=67"]
%     ["0+63/9=7", "8*7-54=2", "5*4-8=12"]

:- use_module(all_puzzles_facts).

two_guesses(CsS, DsS, EsS) :-
    a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs=[C1,C2,C3,C4,C5,C6,C7,C8],
    a_puzzle(_,E1,E2,E3,E4,E5,E6,E7,E8), Es=[E1,E2,E3,E4,E5,E6,E7,E8],
    ( C5 = '=' -> E5 = '=', maplist(\=, [C1,C2,C3,C4,C5,C7,C8], [E1,E2,E3,E4,E5,E7,E8])
    ; C6 = '=' -> E6 = '=', maplist(\=, [C1,C2,C3,C4,C5,C7,C8], [E1,E2,E3,E4,E5,E7,E8])
    ; C7 = '=',   E7 = '=', maplist(\=, [C1,C2,C3,C4,C5,C6,C8], [E1,E2,E3,E4,E5,E6,E8])
    ),

    append(Cs,Es,CEs),
    memberchk('*',CEs), memberchk('-',CEs), memberchk('/',CEs), memberchk('+',CEs),
    non_unique(CEs, CEsDiff), CEsDiff =< 2,

    a_puzzle(_,D1,D2,D3,D4,D5,D6,D7,D8), Ds=[D1,D2,D3,D4,D5,D6,D7,D8],
    maplist(\=, Cs, Ds), % Also ensures '=' not in the same place
    append(Cs,Ds,CDs),
    memberchk('*',CDs), memberchk('-',CDs), memberchk('/',CDs), memberchk('+',CDs),
    non_unique(CDs, CDsDiff), (CEsDiff =< 1 -> true ; CDsDiff =< 1 ),

    string_chars(CsS,Cs), string_chars(DsS,Ds), string_chars(EsS,Es).

non_unique(List, Diff) :-
    sort(List, ListUnique),
    length(ListUnique, ListUniqueLen),
    length(List, ListLen),
    Diff is ListLen - ListUniqueLen.
