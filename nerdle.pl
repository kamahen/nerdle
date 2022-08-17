% -*- mode: Prolog -*-

:- module(nerdle,
          [solve/2,
           run_puzzle/0,
           run_puzzle/1,
           run_puzzle/2,
           run_puzzle/5,
           lazy_read_guesses/1,
           lazy_read_guesses/2,
           init_summary/1,
           string_to_guess/2,
           constrain_not_in/2
          ]).

:- use_module(library(lazy_lists)).

:- set_prolog_flag(optimise, true).

:- meta_predicate solve(2, +).

solve(Constraints, SolutionStr) =>
    puzzle(Solution),
    call(Constraints, Solution, MinMax),
    expr(MinMax, Solution),
    atomic_list_concat(Solution, SolutionStr).

all_syms([1,2,3,4,5,6,7,8,9,0,+,-,*,/,=]).

expr(MinMax, Solution) =>
    puzzle(Solution),
    all_syms(AllSyms),
    include(no(MinMax), AllSyms, No),
    include(yes(MinMax), AllSyms, Yes),
    % format('*** yes=~q no=~q~n', [Yes, No]),
    !, % not needed
    possible(MinMax, Yes, No, AllSyms, Possible),
    maplist(is_in(Possible), Solution),
    valid_puzzle(Solution),
    % The possible/5 check above is too permissive because it uses
    % AllSyms; we need to do another check with just the solution.
    possible(MinMax, Yes, No, Solution, _).

valid_puzzle(Solution) =>
    append(LeftSolution, [=|RightSolution], Solution),
    maplist(is_in_chk([0,1,2,3,4,5,6,7,8,9]), RightSolution),
    \+ adjacent_ops(LeftSolution),
    LeftSolution \= [+|_],
    LeftSolution \= [-|_],
    chars_term(LeftSolution, Left),
    chars_term(RightSolution, Right),
    catch(Left =:= Right, _, fail).

adjacent_ops(Ds) =>
    append(_, [D1,D2|_], Ds),
    member(D1, [+,-,*,/,=]),
    member(D2, [+,-,*,/,=]).

:- det(puzzle/1).
puzzle(Solution) =>
    length(Solution, 8).

% min_count(MinMax, D, Min) :- (Min,_) = MinMax.D.
% max_count(MinMax, D, Max) :- (_,Max) = MinMax.D.

no( MinMax, D) :- (0,0) = MinMax.D.
yes(MinMax, D) :- (1,_) = MinMax.D.

possible(MinMax, _Yes, No, Possible0, Possible) =>
    include(constrain_not_in(No), Possible0, Possible),
    % foldl(select, _Yes, Possible, _), % maplist(valid_count...) supersedes this
    fill_summary(0, ZeroCounts),
    foldl(count, Possible, ZeroCounts, Counts),
    dict_pairs(Counts, _, CountPairs),
    maplist(valid_count(MinMax), CountPairs).

:- det(init_summary/1).
init_summary(Summary) :-
    fill_summary(unknown, Summary).

:- det(fill_summary/2).
fill_summary(Fill, Summary) =>
    all_syms(AllSyms),
    maplist(zero_count(Fill), AllSyms, SummaryList),
    dict_create(Summary, counts, SummaryList).

:- det(zero_count/3).
zero_count(Fill, D, D-Fill).

% e.g.: incr_dict_item(b, -1, d{a:10,b:5}, d{a:10,b:4}).
incr_dict_item(Key, Incr, Counts0, Counts) =>
    C is Counts0.Key + Incr,
    put_dict(Key, Counts0, C, Counts).

:- det(count/3).
count(D, Counts0, Counts) =>
    incr_dict_item(D, 1, Counts0, Counts).

valid_count(MinMax, D-Count) =>
    (DMin,DMax) = MinMax.D,
    DMin =< Count,
    DMax >= Count.

chars_term(Chars, Term) =>
    Chars \= [],
    Chars \= [0|_],
    atomic_list_concat(Chars, String),
    catch(term_string(Term, String), _, fail).

constrain_not_in(NotList, X) =>
    freeze(X, \+ member(X, NotList)).

is_in(List, X) =>
    member(X, List).

is_in_chk(List, X) =>
    memberchk(X, List).

% make_puzzle/1 generates an infinite number of puzzles, using
% backtracking (so, don't do bagof(S, make_puzzle(S), Ss) because it
% won't terminate).
make_puzzle(Solution) =>
    all_syms(AllSyms),
    puzzle(Solution),
    repeat,
    maplist(random_elem(AllSyms), Solution),
    valid_puzzle(Solution).

:- det(random_elem/2).
random_elem(List, Elem) =>
    length(List, Length0),
    Length is Length0 + 1, % random is a half-open interval
    random(1, Length, I),
    nth1(I, List, Elem).

:- det(run_puzzle/0).
run_puzzle =>
    make_puzzle(Solution),
    writeln(Solution), % DO NOT SUBMIT
    !, % don't backtrack into another puzzle
    run_puzzle(Solution).

:- det(run_puzzle/1).
run_puzzle(Solution) :-
    lazy_read_guesses(ReadGuesses),
    run_puzzle(ReadGuesses, Solution).

:- det(run_puzzle/2).
run_puzzle(ReadGuesses, Solution) =>
    % Emacs: (ansi-color-for-comint-mode-on)
    fill_summary(unknown, Summary0),
    run_puzzle(ReadGuesses, Solution, Summary0).

:- det(run_puzzle/3).
run_puzzle(ReadGuesses, Solution, Summary0) :-
    run_puzzle(ReadGuesses, Solution, [], Summary0, _).

:- det(run_puzzle/4).
% run_puzzle(+ReadGuesses:list, +Solution, +Guesses0, +Summary0, +Summary)
run_puzzle([Guess|ReadGuesses], Solution, Guesses0, Summary0, Summary) :-
    append(Guesses0, [Guess], Guesses2), % TODO: Guesses2 = [Guess|Guesses0]
    display_result(Guesses2, Solution, Summary0, Summary2),
    (   Solution == Guess
    ->  Summary = Summary2,
        writeln(''), writeln('')
    ;   run_puzzle(ReadGuesses, Solution, Guesses2, Summary2, Summary)
    ).
run_puzzle([], _Solution, _Guesses0, Summary, Summary).

:- det(read_guess/2).
read_guess(InStream, Guess) =>
    write('Guess: '),
    read_line_to_string(InStream, Line),
    (   Line == end_of_file
    ->  Guess = end_of_file
    ;   Line == "end_of_file"
    ->  Guess = end_of_file
    ;   string_to_guess(Line, Guess)
    ->  true
    ;   format('Invalid input (~w)~n', [Line]),
        read_guess(InStream, Guess)
    ).

% semi-deterministic.
string_to_guess(Line, Guess) :-
    string_chars(Line, LineChars),
    maplist(digitify, LineChars, Guess),
    valid_puzzle(Guess),
    !.

:- det(lazy_read_guesses/1).
lazy_read_guesses(ReadGuesses) :-
    lazy_read_guesses(user_input, ReadGuesses).

:- det(lazy_read_guesses/2).
lazy_read_guesses(InStream, ReadGuesses) :-
    lazy_list(lazy_read_guess(InStream), ReadGuesses).

:- det(lazy_read_guess/3).
lazy_read_guess(InStream, List, Tail) =>
    (   read_guess(InStream, Guess),
        Guess \== end_of_file
    ->  List  = [Guess|Tail]
    ;   Tail = [],
        List = []
    ).

:- det(lazy_write_results/2).
lazy_write_results(List, Tail) =>
    (   List = [X|Tail]
    ->  freeze(X, format('~q~n', [X]))
    ;   Tail = [],
        List = []
    ).

:- det(digitify/2).
digitify('0', D) => D = 0.
digitify('1', D) => D = 1.
digitify('2', D) => D = 2.
digitify('3', D) => D = 3.
digitify('4', D) => D = 4.
digitify('5', D) => D = 5.
digitify('6', D) => D = 6.
digitify('7', D) => D = 7.
digitify('8', D) => D = 8.
digitify('9', D) => D = 9.
digitify(C,   D) => D = C.

:- det(display_result/4).
display_result(Guesses, Solution, Summary0, Summary) =>
    set_prolog_flag(color_term, true),
    foldl(display_result_1(Solution), Guesses, Summary0, Summary),
    nl,
    display_summary(Summary).

:- det(display_result_1/4).
display_result_1(Solution, Guess, Summary0, Summary) =>
    fill_summary(0, ZeroCounts),
    foldl(count, Solution, ZeroCounts, SolutionCounts),
    foldl(adjust_count_for_correct, Guess, Solution, SolutionCounts, SolutionCounts2),
    foldl(display_1, Guess, Solution, SolutionCounts2-Summary0, _-Summary).

:- det(adjust_count_for_correct/4).
adjust_count_for_correct(D, S, Counts0, Counts), D == S =>
    incr_dict_item(D, -1, Counts0, Counts).
adjust_count_for_correct(_D, _S, Counts0, Counts) =>
    Counts = Counts0.

:- det(display_summary/1).
display_summary(Summary) =>
    all_syms(Syms),
    maplist(sym_and_count(Summary), Syms, SummaryPairs),
    join(SummaryPairs, display_colorized, write(' ')),
    ansi_format([reset], '~n', []).

:- det(sym_and_count/3).
sym_and_count(Summary, Sym, Sym-Summary.Sym).

:- det(display_1/4).
%! Display(+GuessC, +SolutionC, +Counts0Summary0, -CountsSummary) is det.
display_1(D, S, Counts-Summary0, CountsSummary), D == S =>
    % Counts have already been adjusted
    new_summary(D, Summary0, correct, Summary),
    CountsSummary = Counts-Summary.
display_1(D, _S, Counts0-Summary0, CountsSummary), Counts0.D > 0 =>
    incr_dict_item(D, -1, Counts0, Counts),
    new_summary(D, Summary0, partial, Summary),
    CountsSummary = Counts-Summary.
display_1(D, _S, Counts0-Summary0, CountsSummary) =>
    incr_dict_item(D, -1, Counts0, Counts),
    new_summary(D, Summary0, wrong, Summary),
    CountsSummary = Counts-Summary.

:- det(new_summary/4).
new_summary(D, Summary0, Judgment, Summary) =>
    display_colorized(D-Judgment),
    updated_summary(Summary0.D, Judgment, New),
    put_dict(D, Summary0, New, Summary).

:- det(updated_summary/3).
%! updated_summary(+Summary0.D, +Judgment, -New) is det.
updated_summary(Was,     Was,     New) => New = Was.
updated_summary(_,       unknown, _  ) => fail. % should never happen
updated_summary(correct, _,       New) => New = correct.
updated_summary(_,       correct, New) => New = correct.
updated_summary(partial, wrong,   New) => New = partial.
updated_summary(unknown, Was,     New) => New = Was.

:- det(display_colorized/1).
display_colorized(D-Type) =>
    display_fmt(Type, Fmt),
    ansi_format(Fmt, '~w', [D]).

:- det(display_fmt/2).
% Defined colors: black, red, green, yellow, blue, magenta, cyan, white
%                 bold, underline, reset
display_fmt(unknown, Fmt) => Fmt = [bold, bg(white),   fg(black)].
display_fmt(wrong,   Fmt) => Fmt = [bold, bg(black),   fg(white)].
display_fmt(correct, Fmt) => Fmt = [bold, bg(green),   fg(white)].
display_fmt(partial, Fmt) => Fmt = [bold, bg(magenta), fg(white)].

:- det(join/3).
join([],     _,    _)     => true.
join([X],    Call, _)     => call(Call, X).
join([X|Xs], Call, Call2) =>
    call(Call, X),
    call(Call2),
    join(Xs, Call, Call2).

