% -*- mode: Prolog; coding: utf-8 -*-

:- module(nerdle2,
          [puzzle_solve/3,
           puzzle_solve_all/2
          ]).

:- encoding(utf8).

/****************************************************

A "helper" / solver for the Nerdle puzzle (http://nerdlegame.com).

Terminology/conventions:

Puzzle - a list of 8 items (or "tiles", to use the game's terminology):

  - A Guess is a "labeling" of the Puzzle, filled with digits,
    operators, and a single equal sign. Numbers may not start
    with "0", nor are unary operators ("+", "-") allowed.

  - A Result is a list of "colors" of the items from the guess
    緑 (green): correct symbol at this location
    黒 (black): this symbol doesn't occur elsewhere
    紅 (red):   incorrect symbol at this location - maybe should be
                赤 (red) or 紫 (purple)? ...
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
:- use_module(library(apply), [include/3, exclude/3,
                               maplist/2, maplist/3, maplist/4,
                               foldl/4]).
:- use_module(library(pairs), [pairs_keys_values/3, pairs_values/2,
                               group_pairs_by_key/2]).
:- use_module(library(lists), [append/3, append/2, member/2]).
:- use_module(library(dif),   [dif/2]).
:- use_module(library(debug), [assertion/1]).

% For debugging: print more elements of a list before the "|...":
:- Options = [quoted(true),
              portray(true),
              attributes(write),
              max_depth(16),
              spacing(standard)],
   set_prolog_flag(  answer_write_options, Options),
   set_prolog_flag(debugger_write_options, Options),
   % set_prolog_flag(write_attributes, write).
   true.

%! puzzle_solve_all(+GuessResults:list(pair), -PuzzleStrs:list(string)) :-
% GuessResults is a list of pairs, with the first element being the guess
% and the second element being the result of the guess. For example:
%       Guess =  ['1', '2', '+', '1', '4', '=', '2', '6']
%       Result = [ 黒,  緑,  紅,   緑,  黒,  紅,   紅,  緑]
%       Guess =  ['1', '2', '+', '1', '4', '=', '2', '6']
%       Result = [ b,   g,   r,   g,   b,   r,   r,   g]
% would be [['1'-黒, '2'-緑, '+'-紅, '1'-緑, '4'-黒, '='-紅, '2'-紅, '6'-緑],
%           ['1'-b,  '2'-g,  '+'-r, '1'-g,  '4'-b,  '='-r, '2'-r,  '6'-g]]
% The resulting PuzzleStrs is a list of 8-character answers, with the
% best answers first. ("Best" is defined in terms of "adds the most
% information")
puzzle_solve_all(GuessResults, PuzzleStrs) :-
    setof(Score-PuzzleStr, puzzle_solve(GuessResults, Score, PuzzleStr), Xs),
    pairs_values(Xs, PuzzleStrs).

%! puzzle_solve(+GuessResults:list(pair), -Score:integer, -PuzzleStr:string) is nondet.
% Process one set of inputs, producing a puzzle result (backtracks).
% GuessResults is as for puzzle_solve_all/2.
% Score: the more negative, the better (for easier sorting).
puzzle_solve(GuessResults, Score, PuzzleStr) :-
    process_inputs(GuessResults, Guesses, Results),
    assertion(maplist(maplist(verify_result), Results)),
    maplist(constrain(Puzzle), Guesses, Results),
    constrain_black(Puzzle, Guesses, Results),
    puzzle_fill(Puzzle),
    maplist(constrain_counts(Puzzle), Guesses, Results),
    score(Puzzle, Guesses, Score),
    string_chars(PuzzleStr, Puzzle).

%! verify_result(C:atom) is nondet.
verify_result(黒).
verify_result(緑).
verify_result(紅).

:- det(process_inputs/3).
%! process_inputs(+GuessResults:list(pair), -Guesses:list(list(atom)), -Results:list(list(atom))) is det.
% Take a list of guess and results (see puzzle_solve_all/2), makes
% lists from the strings and normalizes the "guesses" (see
% normalize_result/2).
process_inputs(GuessResults, Guesses, Results) :-
    pairs_keys_values(GuessResults, GuessStrs, ResultStrs),
    maplist(string_chars, GuessStrs, Guesses),
    maplist(string_chars, ResultStrs, Results0),
    maplist(maplist(normalize_result), Results0, Results).

:- det(constrain/3).
%! constrain(Puzzle:list, +Guess:list, +Result:list) is det.
% Given a guess (e.g., ['7','+','8','-','5','=','1','0']) and
% a result (e.g.,      [ b , b , b , r , r , g , g , r ])),
% add constraints to Puzzle (an 8-element list).
constrain(Puzzle, Guess, Result) :-
    length(Puzzle, 8),
    maplist(normalize_result, Result, ResultNormalized),
    maplist(constrain_from_guess, ResultNormalized, Guess, Puzzle).

:- det(constrain_from_guess/3).
%! constrain_from_guess(+Result:atom, +Guess{g,b,r}, PuzzleItem) is det.
% For a single item in a guess (e.g., Result='7', Guess=b), and the matching
% PuzzleItem from Puzzle, add constraints.
% Add constraints for a single result (緑,黒,紅), Guess{digit,operator,=}, puzzle solution.
constrain_from_guess(緑, Guess, PuzzleItem) :- % Guess is correct at this location
    PuzzleItem = Guess.
constrain_from_guess(紅, Guess, PuzzleItem) :- % Guess is in the answer, but elsewhere
    dif(PuzzleItem, Guess).
constrain_from_guess(黒, Guess, PuzzleItem) :- % Guess is not here
    dif(PuzzleItem, Guess).

%! constrain_counts(+Puzzle:list(?), +Guess:list(atom), +Result:list(atom)) is det.
% Add constraints to Puzzle, according to the Guess and Result.
% For example, if a Guess item is '3' and has the corresponding Result
% 黒 (black), then a constraint is added that corresponding Puzzle
% item cannot be '3', and the constraint would be of the form `dif(Item,'3')`.
constrain_counts(Puzzle, Guess, Result) :-
    pairs_keys_values(GuessResult0, Guess, Result),
    keysort(GuessResult0, GuessResult),
    group_pairs_by_key(GuessResult, GuessGroups),
    maplist(constrain_count(Puzzle), GuessGroups).

%! constrain_count(+Puzzle:list, +Label:atom-Results:list) is det.
% Add counting constraints to Puzzle according to a specific Label-Results.
% If all the Results are black, this provides no information
% If there is at least one black, then we know the maximum number of
% this particular Label is the number of non-black results.
% Otherwise (no black), we know the minimum number of this Label.
constrain_count(Puzzle, Label-Results) :-
    count_label(Puzzle, Label, LabelCount),
    assertion(Results \= []), % group_pairs_by_key/2 can't generate this
    constrain_count_(Results, LabelCount).

%! constrain_count_(+Results, +LabelCount) is nondet.
% Helper for constrain_count/2: add a constraint according to the
% results (a list of results for the label) and label count.
% constrain_count_([], _LabelCount) => fail. % group_pairs_by_key/2 can't generate this
constrain_count_(Results, LabelCount), all_black(Results) =>
    LabelCount = 0.
constrain_count_(Results, LabelCount), some_black(Results) =>
    not_black_count(Results, Count),
    1 =< LabelCount, LabelCount =< Count.
constrain_count_(Results, LabelCount) =>
    not_black_count(Results, Count),
    LabelCount >= Count.

all_black(Xs) :-
    maplist(black, Xs).

some_black(Xs) :-
    include(black, Xs, [_|_]).

not_black_count(Xs, Count) :-
    exclude(black, Xs, NotBlacks),
    length(NotBlacks, Count).

count_label(Puzzle, Label, Count) :-
    include(=(Label), Puzzle, PuzzleMatch),
    length(PuzzleMatch, Count).

%! constrain_black(Puzzle:list, +Guesses, +Results) :-
% Add constraints for all labels that can't appear.
% This gives about a 10% performance boost.
constrain_black(Puzzle, Guesses, Results) :-
    maplist(pairs_keys_values, GuessResults, Guesses, Results),
    append(GuessResults, AllGuessResults),
    setof(G, only_black_guess(AllGuessResults, G), GuessesNotInAnswer),
    maplist(constrain_black_(Puzzle), GuessesNotInAnswer).

only_black_guess(AllGuessResults, G) :-
    member(G-黒, AllGuessResults),
    \+ memberchk(G-緑, AllGuessResults),
    \+ memberchk(G-紅, AllGuessResults).

constrain_black_(Puzzle, G) :-
    maplist(dif(G), Puzzle).

%! puzzle_fill(+Puzzle:list(?) is nondet.
% Fill in the uninstantiated parts of Puzzle.  If this predicate
% succeeds, the Puzzle is ground and valid, according to the
% constraints (but there may be more constraints to be added, for
% required counts for some labels.
puzzle_fill(Puzzle) :-
    append(Left, ['='|Right], Puzzle),
    puzzle(Left, Right),
    valid_left(Left),
    atomic_list_concat(Left, LeftString),
    catch(term_string(LeftTerm, LeftString), _, fail),
    catch(LeftValue is LeftTerm, _, fail),
    integer(LeftValue),
    atom_chars(LeftValue, Right),
    valid_number(Right).

valid_left(Left) :- phrase(valid_left, Left).

valid_number(Number) :- phrase(valid_number, Number).

valid_left --> valid_number.
valid_left --> valid_number, operator, valid_left.

valid_number -->
    digit(D),
    (  { D = '0' }
    -> [ ]
    ;  valid_number_rest
    ).

valid_number_rest --> [].
valid_number_rest --> digit(_), valid_number_rest.

digit(C) --> [C], { digit(C) }.

operator --> [C], { operator(C) }.

%! puzzle(-Left:list, -Right:list) is nondet.
% Instantiate Left and Right to two lists that can be combined with a
% '='. Left's and Right's contents are uninstantiated.
puzzle(Left, Right) :-
    between(2, 6, LenLeft),
    LenRight is 7 - LenLeft,
    length(Left, LenLeft),
    length(Right, LenRight).

:- det(score/3).
%! score(+Puzzle:list(atom), +Guesses:list(list(atom)), -Score:int) is det.
% Compute a score for the Puzzle results together with the list of
% guesses. The score is negative, so that the lower negative number is
% a higher score (this make sorting eaiser).
score(Puzzle, Guesses, Score) :-
    append(Guesses, GuessesCombined0),
    sort(GuessesCombined0, GuessesCombined),
    foldl(score_label, Puzzle, GuessesCombined-0, _-Score).

score_label(Label, GuessesCombined-Score, GS), memberchk(Label, GuessesCombined) =>
    GS = GuessesCombined-Score.
score_label(Label, GuessesCombined-Score0, GS) =>
    Score is Score0 - 100,
    GS = [Label|GuessesCombined]-Score.

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
green(grn).
green(green).

black(黒).
black(b).
black(blk).
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

