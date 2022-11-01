% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_nerdle2,
	  [ test_nerdle2/0,
            % The following are exported for REPL testing:
            puzzle_solve/2,
            puzzle_fill/1
	  ]).

:- encoding(utf8).

:- use_module(library(plunit)).
:- use_module(nerdle2).

test_nerdle2 :-
    run_tests([ nerdle2
	      ]).

:- begin_tests(nerdle2).

test(p1, all(P == ["61-50=11",
                   "65-50=15",
                   "66-50=16",
                   "69-50=19"])) :-
    puzzle_solve(["7+8-5=10"-
                  "bbbrrggr",
                  "52-40=12"-
                  "rbgbgggb",
                  "63-50=13"-
                  "gbgggggb"],
                % "69-50=19"-  % answer
                % "gggggggg"
                 P).

test(p2, all(P == ["98/7-6=8",
                   "98/7-8=6"])) :-
    puzzle_solve(["7+8-0=15"-
                  "rbrrbrbb",
                  "23-2*7=9"-
                  "bbrbbrgr",
                  "64/4-9=7"-
                  "rbgbgrgr"],
                % "98/7-6=8"-  % answer
                % "gggggggg"
                 P).

:- end_tests(nerdle2).

