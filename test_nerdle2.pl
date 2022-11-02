% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_nerdle2,
	  [ test_nerdle2/0
	  ]).

:- encoding(utf8).

:- use_module(library(plunit)).
:- use_module(nerdle2).

test_nerdle2 :-
    run_tests([ nerdle2
	      ]).

:- begin_tests(nerdle2).

test(p1, Ps == ["69-50=19",
                "61-50=11",
                "65-50=15",
                "66-50=16"]) :-
    puzzle_solve_all(["7+8-5=10"-
                      "bbbrrggr",
                      "52-40=12"-
                      "rbgbgggb",
                      "63-50=13"-
                      "gbgggggb"],
                    % "69-50=19"-  % answer
                    % "gggggggg"
                     Ps).

test(p2, Ps == ["98/7-6=8",
                "98/7-8=6"]) :-
    puzzle_solve_all(["7+8-0=15"-
                      "rbrrbrbb",
                      "23-2*7=9"-
                      "bbrbbrgr",
                      "64/4-9=7"-
                      "rbgbgrgr"],
                      % "98/7-6=8"-  % answer
                      % "gggggggg"
                     Ps).

test(p3, Ps == ["2-10/5=0"]) :-
    puzzle_solve_all(["8+9-3=14"-
                      "bbbrbrrb",
                      "10-5-3=2"-
                      "rrrrbbgr",
                      "5-20/5=1"-
                      "bgrggggr"],
                     Ps).

:- end_tests(nerdle2).

