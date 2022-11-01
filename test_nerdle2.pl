% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_nerdle2,
	  [ test_nerdle2/0,
            % The following are exported for REPL testing:
            constrain/2,
            constrain_counts/2,
            c11/2, c12/2, c13/2, c14/2,
            c21/2, c22/2, c23/2, c24/2,
            puzzle_fill/1
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

c11("7+8-5=10",
    "bbbrrggr").
c12("52-40=12",
    "rbgbgggb").
c13("63-50=13",
    "gbgggggb").
c14("69-50=19",  % answer
    "gggggggg").

:- meta_predicate constrain(2, +).
constrain(Lookup, Puzzle) :-
    guess_lookup(Lookup, Guess, Result),
    constrain(Guess, Result, Puzzle).

:- meta_predicate constrain_counts(2, +).
constrain_counts(Lookup, Puzzle) :-
    guess_lookup(Lookup, Guess, Result),
    constrain_counts(Guess, Result, Puzzle).

:- meta_predicate guess_lookup(2, -, -).
guess_lookup(Lookup, Guess, Result) :-
    call(Lookup, GuessStr, Result0Str),
    string_chars(GuessStr, Guess),
    string_chars(Result0Str, Result0),
    maplist(normalize_result, Result0, Result).

p1(PuzzleStr) :-
    constrain(c11, Puzzle),
    constrain(c12, Puzzle),
    constrain(c13, Puzzle),
    puzzle_fill(Puzzle),
    constrain_counts(c11, Puzzle),
    constrain_counts(c12, Puzzle),
    constrain_counts(c13, Puzzle),
    string_chars(PuzzleStr, Puzzle).
    % PuzzleStr =  "69-50=19".

c21("7+8-0=15",
    "rbrrbrbb").
c22("23-2*7=9",
    "bbrbbrgr").
c23("64/4-9=7",
    "rbgbgrgr").
c24("98/7-6=8",
    "gggggrgr").

p2(PuzzleStr) :-
    length(Puzzle, 8),
    constrain(c21, Puzzle),
    constrain(c22, Puzzle),
    constrain(c23, Puzzle),
  % constrain(c24, Puzzle),
    puzzle_fill(Puzzle),
    constrain_counts(c21, Puzzle),
    constrain_counts(c22, Puzzle),
    constrain_counts(c23, Puzzle),
    constrain_counts(c24, Puzzle),
    string_chars(PuzzleStr, Puzzle).

:- begin_tests(nerdle2).

test(p1, all(P == ["61-50=11",
                   "65-50=15",
                   "66-50=16",
                   "69-50=19"])) :-
    p1(P).

test(p2, all(P == ["98/7-6=8",
                   "98/7-8=6"])) :-
    p2(P).

:- end_tests(nerdle2).

