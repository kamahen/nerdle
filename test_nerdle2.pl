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

p1([X1,X2,X3,X4,X5,X6,X7,X8]) :- p1(X1,X2,X3,X4,X5,X6,X7,X8).

c11(['7','+','8','-','5','=','1','0'],
    [ b , b , b , r , r , g , g , r ]).
c12(['5','2','-','4','0','=','1','2'],
    [ r , b , g , b , g , g , g , b ]).
c13(['6','3','-','5','0','=','1','3'],
    [ g , b , g , g , g , g , g , b ]).
c14(['6','9','-','5','0','=','1','9'],  % answer
    [ g,  g,  g,  g,  g,  g,  g,  g ]).

:- meta_predicate constrain(2, +).
constrain(Lookup, Puzzle) :-
    call(Lookup, Guess, Result),
    constrain(Guess, Result, Puzzle).

p1(X1,X2,X3,X4,X5,X6,X7,X8) :-
     Puzzle=[X1,X2,X3,X4,X5,X6,X7,X8],
     constrain(c11, Puzzle),
     constrain(c12, Puzzle),
     constrain(c13, Puzzle),
     puzzle_fill(Puzzle).
     % Puzzle =  ['6','9','-','5','0','=','1','9'].

test(p1, all(P == [['6','1','-','5','0','=','1','1'],
                   ['6','5','-','5','0','=','1','5'],
                   ['6','6','-','5','0','=','1','6'],
                   ['6','9','-','5','0','=','1','9']])) :-
    p1(P).

p2([X1,X2,X3,X4,X5,X6,X7,X8]) :- p2(X1,X2,X3,X4,X5,X6,X7,X8).

c21(['7','+','8','-','0','=','1','5'],
    [ r , b , r , r , b , r , b , b ]).
c22(['2','3','-','2','*','7','=','9'],
    [ b , b , r , b , b , r , g , r ]).
c23(['6','4','/','4','-','9','=','7'],
    [ r , b , g , b , g , r , g , r ]).
c24(['9','8','/','7','-','6','=','8'],
    [ g , g , g , g , g , r , g , r ]).

p2(X1,X2,X3,X4,X5,X6,X7,X8) :-
    Puzzle=[X1,X2,X3,X4,X5,X6,X7,X8],
    constrain(c21, Puzzle),
    constrain(c22, Puzzle),
    constrain(c23, Puzzle),
  % constrain(c24, Puzzle),
    puzzle_fill(Puzzle).

test(p2, all(P == [['9','8','/','7','-','6','=','8'],
                   ['9','8','/','7','-','8','=','6']])) :-
    p2(P).

:- end_tests(nerdle2).

