% -*- mode: Prolog; coding: utf-8 -*-

:- module(nerdle2,
          [puzzle_fill/1,
           puzzle_atom/1,
           constrain/3
          ]).

:- encoding(utf8).

% Allow more elements when printing a list before the "|...":
:- Options = [quoted(true),
              portray(true),
              attributes(write),
              max_depth(16),
              spacing(standard)],
   set_prolog_flag(  answer_write_options, Options),
   set_prolog_flag(debugger_write_options, Options),
   set_prolog_flag(write_attributes, write).

% Input moves.
% There are three colors for a guess:
%   緑 (green): correct symbol at this location
%   黒 (black): this symbol doesn't occur elsewhere
%   紅 (red):   incorrect symbol at this location - maybe should be 赤 (red) or 紫 ([light] purple)? ...
%               Wordle uses yellow (黄)
% These results are somewhat context-dependent. For example, a guess of
%   12+12=24 might return 黒 for the first "1" and 緑 for the 2nd "1"; this means
%            that there is only one "1" in the answer.
% A guess is a list of symbols; a result is a list of the three colors.
%   e.g., ['1', '2', '+', '1', '2', '=', '2', '4']
%         [黒,  緑,  紅,   緑,  黒,  紅,   黒,  緑]
/* 
     Puzzle=[X1,X2,X3,X4,X5,X6,X7,X8],
     constrain(['1','2','+','1','2','=','2','4'],
                [黒, 緑, 紅,  緑, 黒, 紅,  黒, 緑], Puzzle).
*/

constrain(Guess, Result, Puzzle) :-
    zero_counts(ZeroCounts),
    foldl(constrain_from_guess(Puzzle), Result, Guess, Puzzle, ZeroCounts, _Counts).

:- det(constrain_from_guess/6).
constrain_from_guess(Puzzle, Result, Guess, PuzzleItem, Counts, Counts2) :-
    C2 is Counts.Guess + 1,
    put_dict(Guess, Counts, C2, Counts2),
    constrain_from_guess_(Result, Guess, Puzzle, PuzzleItem, Counts2).

:- det(constrain_from_guess_/5).
%! constrain_from_guess_(+Result, +Guess, Puzzle:list, +PuzzleIem, +Counts) is det.
% Add constraints for a single result (緑,黒紅), Guess(digit,operator,=), puzzle solution, counts so far.
constrain_from_guess_(緑, Guess, Puzzle, PuzzleItem, Counts) :- % Guess is correct at this location
    PuzzleItem = Guess,
    C = Counts.Guess,
    when(ground(Puzzle), count_item_test(Puzzle, Guess, =<(C))).
constrain_from_guess_(紅, Guess, Puzzle, PuzzleItem, Counts) :- % Guess exists elsewhere (to the right)
    dif(PuzzleItem, Guess),
    C = Counts.Guess,
    when(ground(Puzzle), count_item_test(Puzzle, Guess, =<(C))).
constrain_from_guess_(黒, Guess, Puzzle, PuzzleItem, Counts) :- % Guess is not here nor to the right
    dif(PuzzleItem, Guess),
    C is Counts.Guess - 1,
    when(ground(Puzzle), count_item_test(Puzzle, Guess, >=(C))).
constrain_from_guess_(黄,     Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(紅, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(green,  Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(緑, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(red,    Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(紅, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(yellow, Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(紅, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(black,  Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(黒, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(g,      Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(緑, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(r,      Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(紅, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(y,      Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(紅, Guess, Puzzle, PuzzleItem, Counts).
constrain_from_guess_(b,      Guess, Puzzle, PuzzleItem, Counts) :- constrain_from_guess_(黒, Guess, Puzzle, PuzzleItem, Counts).

count_item_test(Puzzle, Item, Test) :-
    count_item(Puzzle, Item, Count),
    call(Test, Count).

count_item(Puzzle, Item, Count) :-
    include(=(Item), Puzzle, PuzzleMatch),
    length(PuzzleMatch, Count).

zero_counts(Counts) :-
    bagof(X, digit_or_operator(X), Xs),
    maplist(zero, Xs, X0s),
    dict_create(Counts, counts, ['='-0|X0s]).

zero(X, X-0).

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
    atom_chars(LeftValue, Right).


starts_with_invalid([X|_]) :-
    invalid_start(X).
starts_with_invalid(Expr) :-
    append(_, [Op,X|_], Expr),
    invalid_start(X),
    operator(Op),

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

