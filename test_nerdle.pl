% -*- mode: Prolog -*-

:- module(test_nerdle,
	  [ test_nerdle/0
	  ]).

:- asserta(user:file_search_path(library, '.')).

:- use_module(library(plunit)).
:- use_module(library(error)).
:- use_module(library(debug), [assertion/1]).

:- use_module(nerdle).

test_nerdle :-
    run_tests([ nerdle
	      ]).

:- begin_tests(nerdle, [setup(set_interactive_display(false))]).

test(solve1, set(S == ['50*3=150'])) :-
    solve(constraints1, S).

test(solve2, set(S == ['77-9*8=5', '79-9*8=7'])) :-
    solve(constraints2, S).

% TODO: solve3

test(solve4, set(S == ['5*73=365'])) :-
    solve(constraints4, S).

test(run_puzzle1, Summary == counts{1 : correct,
                                    2 : wrong,
                                    3 : correct,
                                    4 : wrong,
                                    5 : correct,
                                    6 : wrong,
                                    7 : unknown,
                                    8 : unknown,
                                    9 : unknown,
                                    0 : correct,
                                    + : wrong,
                                    - : unknown,
                                    * : correct,
                                    / : wrong,
                                    = : correct}) :-
    init_summary(Summary0),
    run_puzzle([[3,1,*,5,=,1,5,5],
                [1,+,1,6,/,4,=,5],
                [1,2,/,6,*,3,=,6],
                [5,0,*,5,=,2,5,0],
                [5,0,*,3,=,1,5,0]],
               [5,0,*,3,=,1,5,0],
               [], Summary0, Summary).

test(run_puzzle2, Summary == counts{1 : correct,
                                    2 : correct,
                                    3 : wrong,
                                    4 : correct,
                                    5 : wrong,
                                    6 : wrong,
                                    7 : correct,
                                    8 : wrong,
                                    9 : unknown,
                                    0 : wrong,
                                    + : correct,
                                    - : unknown,
                                    * : unknown,
                                    / : unknown,
                                    = : correct}) :-
    init_summary(Summary0),
    GuessesAsLines = ['14+38=52',
                      '24+16=40',
                      '44+27=71'],
    maplist(string_to_guess, GuessesAsLines, GuessesAsLists),
    assertion(GuessesAsLists == [[1,4,+,3,8,=,5,2],
                                 [2,4,+,1,6,=,4,0],
                                 [4,4,+,2,7,=,7,1]]),
    run_puzzle(GuessesAsLists,
               [4,4,+,2,7,=,7,1],
               [], Summary0, Summary).

% TODO: use constraints4 to make a puzzle with guesses
%       4*8+5=37
%       6*27=162
%       5*73=365 (answer)
test(run_puzzle3) :-
    init_summary(_Summary0),
    true.

:- end_tests(nerdle).


% Old game:
:- det(constraints1/2).
constraints1([D1,D2,D3,D4,D5,D6,D7,D8], MinMax) :-
    % Yes = [1,3,5,0,*,=],
    % No = [2,4,6,7,8,9,+,-,/],
    MinMax = minmax{ = : (1,1),
                     + : (0,0),
                     - : (0,0),
                     / : (0,0),
                     * : (1,1),
                     0 : (1,8),
                     1 : (1,8),
                     2 : (0,0),
                     3 : (1,8),
                     4 : (0,0),
                     5 : (1,8),
                     6 : (0,0),
                     7 : (0,0),
                     8 : (0,0),
                     9 : (0,0)},
    constrain_not_in([9,1,7,3], D1),
    constrain_not_in([2,3,1], D2),
    D3 = (*),
    constrain_not_in([6,7,5], D4),
    D5 = (=),
    D6 = 1,
    D7 = 5,
    constrain_not_in([4,6,1,5], D8).

:- det(constraints2/2).
constraints2([D1,D2,D3,D4,D5,D6,D7,D8], MinMax) :-
    % Yes = [7,8,9,-,*,=]
    % No = [1,2,3,6,+]
    MinMax = minmax{ = : (1,1),
                     + : (0,0),
                     - : (1,8),
                     / : (0,8),
                     * : (1,8),
                     0 : (0,8),
                     1 : (0,0),
                     2 : (0,0),
                     3 : (0,0),
                     4 : (0,8),
                     5 : (0,8),
                     6 : (0,0),
                     7 : (1,8),
                     8 : (1,8),
                     9 : (1,8)},
    D1 = 7,
    constrain_not_in([+,8], D2),
    D3 = (-),
    constrain_not_in([-,8], D4),
    D5 = (*),
    constrain_not_in([=,9], D6),
    D7 = (=),
    constrain_not_in([2,6], D8).

:- det(constraints3/2).
constraints3([D1,D2,D3,D4,D5,D6,D7,D8], MinMax) :-
    MinMax = minmax{ = : (1,1),
                     + : (1,8),
                     - : (0,8),
                     * : (0,8),
                     / : (0,8),
                     1 : (1,8),
                     2 : (1,8),
                     3 : (0,0),
                     4 : (1,8),
                     5 : (0,0),
                     6 : (0,0),
                     7 : (0,8),
                     8 : (0,0),
                     9 : (0,8),
                     0 : (0,0)},
    constrain_not_in([1,2], D1),
    D2 = 4,
    D3 = +,
    constrain_not_in([3,1], D4),
    constrain_not_in([8,6], D5),
    D6 = =,
    constrain_not_in([5,4], D7),
    constrain_not_in([2,0], D8).

constraints4([D1,D2,D3,D4,D5,D6,D7,D8], MinMax) :-
    MinMax = minmax{ = : (1,1),
                     + : (0,0),
                     - : (0,8),
                     / : (0,8),
                     * : (1,8),
                     1 : (0,0),
                     2 : (0,0),
                     3 : (1,8),
                     4 : (0,0),
                     5 : (1,8),
                     6 : (1,8),
                     7 : (1,8),
                     8 : (0,0),
                     9 : (0,8),
                     0 : (0,8)},
    constrain_not_in([4,6], D1),
    D2 = *,
    constrain_not_in([8,2], D3),
    constrain_not_in([+,7], D4),
    constrain_not_in([5], D5), D5 = (=),
    constrain_not_in([=,1], D6),
    constrain_not_in([3], D7), D7 = 6,
    constrain_not_in([7,2], D8).
