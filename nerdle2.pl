% -*- mode: Prolog; coding: utf-8 -*-

:- module(nerdle2,
          [puzzle_fill/1,
           puzzle_atom/1,
           constrain/3,
           constrain_counts/3,
           normalize_result/2
          ]).

:- encoding(utf8).

/****************************************************

A "helper" / solver for the Nerdle puzzle (http://nerdlegame.com).

Terminology/conventions:

Puzzle - a list of 8 items (or "tiles", to use the game's terminology):

  - a Guess is a "labeling" of the Puzzle, filled with digits,
    operators, and a single equal sign (see puzzle_label/1).

  - a Result is a list of "colors" of the items from the guess
    緑 (green): correct symbol at this location
    黒 (black): this symbol doesn't occur elsewhere
    紅 (red):   incorrect symbol at this location - maybe should be
                赤 (red) or 紫 ([light] purple)? ...
                Wordle uses yellow (黄)

An example:
   Guess =  ['1', '2', '+', '1', '4', '=', '2', '6']
   Result = [ 黒,  緑,  紅,   緑,  黒,  紅,   紅,  緑]

   The first "1" is 黒 and the second "1" is 緑; this means that the
   first "1" is wrong and the second one is correct. (From this, we
   can infer that there is only one "1" in the answer.)

   The first "2" is 緑 and the second "2" is 紅; this means that there
   are at least two "2"s in the answer and that the second "2" is in
   an incorrect position.

****************************************************/

% :- set_prolog_flag(autoload, false).
:- use_module(library(apply), [include/3, exclude/3, maplist/2, maplist/3, maplist/4]).
:- use_module(library(pairs), [pairs_keys_values/3, group_pairs_by_key/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(dif),   [dif/2]).
:- use_module(library(when),  [when/2]).

% Allow more elements when printing a list before the "|...":
:- Options = [quoted(true),
              portray(true),
              attributes(write),
              max_depth(16),
              spacing(standard)],
   set_prolog_flag(  answer_write_options, Options),
   set_prolog_flag(debugger_write_options, Options),
   set_prolog_flag(write_attributes, write).

%! constrain(+Guess:list, +Result:list, Puzzle:list) is det.
% Given a guess (e.g., ['7','+','8','-','5','=','1','0']) and
% a result (e.g.,      [ b , b , b , r , r , g , g , r ])),
% add constraints to Puzzle (an 8-element list).
constrain(Guess, Result, Puzzle) :-
    length(Puzzle, 8),
    maplist(normalize_result, Result, ResultNormalized),
    maplist(constrain_from_guess, ResultNormalized, Guess, Puzzle).

%! constrain_from_guess(+Result:atom, +Guess{g,b,r}, PuzzleItem) is det.
% For a single item in a guess (e.g., Result='7', Guess=b), and the matching
% PuzzleItem from Puzzle, add constraints.
:- det(constrain_from_guess/3).
% Add constraints for a single result (緑,黒,紅), Guess{digit,operator,=}, puzzle solution.
constrain_from_guess(緑, Guess, PuzzleItem) :- % Guess is correct at this location
    PuzzleItem = Guess.
constrain_from_guess(紅, Guess, PuzzleItem) :- % Guess exists elsewhere
    dif(PuzzleItem, Guess).
constrain_from_guess(黒, Guess, PuzzleItem) :- % Guess is not here
    dif(PuzzleItem, Guess).

constrain_counts(Guess, Result, Puzzle) :-
    pairs_keys_values(GuessResult0, Guess, Result),
    keysort(GuessResult0, GuessResult),
    group_pairs_by_key(GuessResult, GuessGroups),
    maplist(constrain_count(Puzzle), GuessGroups).

:- det(constrain_count/2).
%! constrain_count(+Puzzle:list, +Label:atom-Results:list) is det.
% Add counting constraints to Puzzle according to a specific Label-Results.
% If all the Results are black, this provides no information
% If there is at least one black, then we know the maximum number of
% this particular Label is the number of non-black results.
% Otherwise (no black), we know the minimum number of this Label.
constrain_count(_Puzzle, _Label-[]) => fail. % group_pairs_by_key/2 can't generate this.
constrain_count(Puzzle, Label-Results), all_black(Results) =>
    count_check(Puzzle, Label, =(0)).
constrain_count(Puzzle, Label-Results), some_black(Results) =>
    not_black_count(Results, Count),
    count_check(Puzzle, Label, between(1,Count)).
constrain_count(Puzzle, Label-Results) =>
    not_black_count(Results, Count),
    count_check(Puzzle, Label, between(Count,8)).

all_black(Xs) :-
    maplist(black, Xs).

some_black(Xs) :-
    include(black, Xs, [_|_]).

not_black_count(Xs, Count) :-
    exclude(black, Xs, NotBlacks),
    length(NotBlacks, Count).

:- meta_predicate count_check(+, +, 1).
count_check(Puzzle, Label, Test) :-
    count_label(Puzzle, Label, Count),
    call(Test, Count).

count_label(Puzzle, Label, Count) :-
    include(=(Label), Puzzle, PuzzleMatch),
    length(PuzzleMatch, Count).

puzzle_atom(PuzzleAtom) :-
    puzzle_fill(Puzzle),
    atomic_list_concat(Puzzle, PuzzleAtom).

puzzle_fill(Puzzle) :-
    puzzle(Left, Right),
    append(Left, [=|Right], Puzzle),
    maplist(digit_or_operator, Left),
    \+ starts_with_invalid(Left),
    atomic_list_concat(Left, LeftString),
    catch(term_string(LeftTerm, LeftString), _, fail),
    catch(LeftValue is LeftTerm, _, fail),
    integer(LeftValue),
    atom_chars(LeftValue, Right),
    \+ starts_with_invalid(Right).

starts_with_invalid([X|_]) :-
    invalid_start(X).
starts_with_invalid(Expr) :-
    append(_, [Op,X|_], Expr),
    invalid_start(X),
    operator(Op).

invalid_start('0').
invalid_start('+').
invalid_start('-').
invalid_start('/').
invalid_start('*').
invalid_start('=').


puzzle(Left, Right) :-
    between(2, 6, LenLeft),
    LenRight is 7 - LenLeft,
    length(Left, LenLeft),
    length(Right, LenRight).

% Not used:
% :- use_module(library(yall)).
% zero_counts(Counts) :-
%     all_puzzle_label(Xs),
%     maplist([X,X-0]>>true, Xs, X0s),
%     dict_create(Counts, counts, X0s).
%
% all_puzzle_label(Xs) :-
%     bagof(X, puzzle_label(X), Xs).

puzzle_label(X) :- digit_or_operator(X).
puzzle_label('=').

digit_or_operator(X) :- digit(X).
digit_or_operator(X) :- operator(X).

digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

operator('+').
operator('-').
operator('*').
operator('/').

green(緑).
green(g).
green(green).

black(黒).
black(b).
black(black).

red(紅).
red(赤).
red(紫).
red(黄).
red(r).
red(red).

:- det(normalize_result/2).
normalize_result(緑,    緑).
normalize_result(g,     緑).
normalize_result(green, 緑).
normalize_result(黒,    黒).
normalize_result(b,     黒).
normalize_result(black, 黒).
normalize_result(紅,    紅).
normalize_result(赤,    紅).
normalize_result(紫,    紅).
normalize_result(黄,    紅).
normalize_result(r,     紅).
normalize_result(red,   紅).

