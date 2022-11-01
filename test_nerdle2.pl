% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_nerdle2,
	  [ test_nerdle2/0,
            % The following are exported for REPL testing:
            p/2,
            puzzle_fill/1
	  ]).

:- encoding(utf8).

:- asserta(user:file_search_path(library, '.')).

:- use_module(library(pairs), [pairs_keys_values/3]).
:- use_module(library(plunit)).
:- use_module(library(error)).
:- use_module(library(debug), [assertion/1]).

:- use_module(nerdle2).

test_nerdle2 :-
    run_tests([ nerdle2
	      ]).

% process one set of inputs, producing a puzzle result (backtracks).
p(GuessResults, PuzzleStr) :-
    process_inputs(GuessResults, Guesses, Results),
    maplist(constrain(Puzzle), Guesses, Results),
    puzzle_fill(Puzzle),
    maplist(constrain_counts(Puzzle), Guesses, Results),
    string_chars(PuzzleStr, Puzzle).

process_inputs(GuessResults, Guesses, Results) :-
    pairs_keys_values(GuessResults, GuessStrs, ResultStrs),
    maplist(string_chars, GuessStrs, Guesses),
    maplist(string_chars, ResultStrs, Results0),
    maplist(maplist(normalize_result), Results0, Results).

:- begin_tests(nerdle2).

test(p1, all(P == ["61-50=11",
                   "65-50=15",
                   "66-50=16",
                   "69-50=19"])) :-
    p(["7+8-5=10"-
       "bbbrrggr",
       "52-40=12"-
       "rbgbgggb",
       "63-50=13"-
       "gbgggggb"],
   %   "69-50=19"-  % answer
   %   "gggggggg"
      P).

test(p2, all(P == ["98/7-6=8",
                   "98/7-8=6"])) :-
    p(["7+8-0=15"-
       "rbrrbrbb",
       "23-2*7=9"-
       "bbrbbrgr",
       "64/4-9=7"-
       "rbgbgrgr"],
   %   "98/7-6=8"-  % answer
   %   "gggggrgr"
      P).

:- end_tests(nerdle2).

