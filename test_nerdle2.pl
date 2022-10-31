% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_nerdle2,
	  [ test_nerdle2/0
	  ]).

:- encoding(utf8).

:- asserta(user:file_search_path(library, '.')).

:- use_module(library(plunit)).
:- use_module(library(error)).
:- use_module(library(debug), [assertion/1]).

:- use_module(nerdle2).


test_nerdle2 :-
    run_tests([ nerdle2
	      ]).

:- begin_tests(nerdle2).

p([X1,X2,X3,X4,X5,X6,X7,X8]) :- p(X1,X2,X3,X4,X5,X6,X7,X8).

c1(['7','+','8','-','5','=','1','0'],
   [ b , b , b , r , r , g , g , r ]).
c2(['5','2','-','4','0','=','1','2'],
   [ r , b , g , b , g , g , g , b ]).
c3(['6','3','-','5','0','=','1','3'],
   [ g , b , g , g , g , g , g , b ]).
c4(['6','9','-','5','0','=','1','9'],
   [ g,  g,  g,  g,  g,  g,  g,  g ]).

constrain(Lookup, Puzzle) :-
    call(Lookup, Guess, Result),
    constrain(Guess, Result, Puzzle).

p(X1,X2,X3,X4,X5,X6,X7,X8) :-
     Puzzle=[X1,X2,X3,X4,X5,X6,X7,X8],
     constrain(c1, Puzzle),
     constrain(c2, Puzzle),
     constrain(c3, Puzzle),
     puzzle_fill(Puzzle).
     % Puzzle =  ['6','9','-','5','0','=','1','9'].

test(p1, all(P == [['6','1','-','5','0','=','1','1'],
                   ['6','5','-','5','0','=','1','5'],
                   ['6','6','-','5','0','=','1','6'],
                   ['6','9','-','5','0','=','1','9']])) :-
    p(P).

:- end_tests(nerdle2).

