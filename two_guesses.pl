% Try to figure out best 2 first guesses

:- use_module(all_puzzles_facts).

two_guesses(CsS, DsS, EsS) :-
    a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs=[C1,C2,C3,C4,C5,C6,C7,C8],
    ( C5 = '=' -> E5 = '=' ; true), ( C6 = '=' -> E6 = '=' ), (C7 = '=' -> E7 = '=' ; true),
    a_puzzle(_,E1,E2,E3,E4,E5,E6,E7,E8), Es=[E1,E2,E3,E4,E5,E6,E7,E8],
    append(Cs,Es,CEs),
    memberchk('*',CEs), memberchk('-',CEs), memberchk('/',CEs), memberchk('+',CEs),
    non_unique(CEs, CEsDiff), CEsDiff =< 2,

    ( C5 = '=' -> dif(D5, '=') ; true ), (C6 = '=' -> dif(D6, '=') ; true ), (C7 = '=' -> dif(D7, '='); true ),
    a_puzzle(_,D1,D2,D3,D4,D5,D6,D7,D8), Ds=[D1,D2,D3,D4,D5,D6,D7,D8],
    Cs \= Ds, Cs \= Es, Ds \= Es,
    append(Cs,Ds,CDs),
    memberchk('*',CDs), memberchk('-',CDs), memberchk('/',CDs), memberchk('+',CDs),
    non_unique(CDs, CDsDiff), (CEsDiff =< 1 -> true ; CDsDiff =< 1 ),

    string_chars(CsS,Cs), string_chars(DsS,Ds), string_chars(EsS,Es).

non_unique(List, Diff) :-
    sort(List, ListUnique),
    length(ListUnique, ListUniqueLen),
    length(List, ListLen),
    Diff is ListLen - ListUniqueLen.
