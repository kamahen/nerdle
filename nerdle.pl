specifics(MinMax, D1,D2,D3,D4,D5,D6,D7,D8) :-
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
    not_in([9,1,7,3], D1),
    not_in([2,3,1], D2),
    D3 = (*),
    not_in([6,7,5], D4),
    D5 = (=),
    D6 = 1,
    D7 = 5,
    not_in([4,6,1,5], D8).

solve(S) :-
    Ss = [D1,D2,D3,D4,D5,D6,D7,D8],
    specifics(MinMax, D1,D2,D3,D4,D5,D6,D7,D8),
    expr(MinMax, Ss),
    atomic_list_concat(Ss, S).

all_syms([=,+,-,*,/, 1,2,3,4,5,6,7,8,9,0]).

expr(MinMax, Ss) :-
    all_syms(AllSyms),
    include(no(MinMax), AllSyms, No),
    include(yes(MinMax), AllSyms, Yes),
    puzzle(Ss),
    possible(MinMax, Yes, No, AllSyms, Possible),
    maplist(in(Possible), Ss),
    valid_puzzle(Ss).

valid_puzzle(Ss) :-
    append(LeftSs, [=|RightSs], Ss),
    maplist(in([0,1,2,3,4,5,6,7,8,9]), RightSs),
    \+ adjacent_ops(LeftSs),
    chars_term(LeftSs, Left),
    chars_term(RightSs, Right),
    catch(Left =:= Right, _, fail).

adjacent_ops(Ds) :-
    append(_, [D1,D2|_], Ds),
    member(D1, [+,-,*,/,=]),
    member(D2, [+,-,*,/,=]).

puzzle(Ss) :-
    length(Ss, 8).

min(MinMax, D, Min) :- (Min,_) = MinMax.D.
max(MinMax, D, Max) :- (_,Max) = MinMax.D.
no( MinMax, D) :- (0,0) = MinMax.D.
yes(MinMax, D) :- (1,_) = MinMax.D.

possible(MinMax, _Yes, No, Possible0, Possible) :-
    include(not_in(No), Possible0, Possible),
    % foldl(select, _Yes, Possible, _), % maplist(valid_count...) supersedes this
    fill_summary(0, ZeroCounts),
    foldl(count, Possible, ZeroCounts, Counts),
    dict_pairs(Counts, _, CountPairs),
    maplist(valid_count(MinMax), CountPairs).

fill_summary(Fill, ZeroCounts) :-
    all_syms(AllSyms),
    maplist(zero_count(Fill), AllSyms, ZeroCountsList),
    dict_create(ZeroCounts, counts, ZeroCountsList).

zero_count(Fill, D, D-Fill).

count(D, Counts0, Counts) :-
    C is Counts0.D + 1,
    put_dict(D, Counts0, C, Counts).

valid_count(MinMax, D-Count) :-
    (DMin,DMax) = MinMax.D,
    DMin =< Count,
    DMax >= Count.

chars_term(Chars, Term) :-
    Chars \= [],
    Chars \= [0|_],
    atomic_list_concat(Chars, String),
    catch(term_string(Term, String), _, fail).

not_in(NotList, X) :-
    freeze(X, \+ member(X, NotList)).

in(List, X) :-
    member(X, List).

% make_puzzle/1 generates an infinite number of puzzles, using
% backtracking (so, don't do bagof(S, make_puzzle(S), Ss) because it
% won't terminate).
make_puzzle(Ss) :-
    all_syms(AllSyms),
    puzzle(Ss),
    repeat,
    maplist(random_elem(AllSyms), Ss),
    valid_puzzle(Ss).

random_elem(List, Elem) :-
    length(List, Length),
    random(1, Length, I),
    nth1(I, List, Elem).

:- det(run_puzzle/0).
run_puzzle :-
    make_puzzle(Ss),
    !, % don't backtrack into another puzzle
    run_puzzle(Ss).

:- det(run_puzzle/1).
run_puzzle(Ss) =>
    % Emacs: (ansi-color-for-comint-mode-on)
    run_puzzle(Ss, []).

:- det(run_puzzle/2).
run_puzzle(Ss, Guesses) =>
    read_guess(Guess),
    append(Guesses, [Guess], Guesses2),
    display_result(Guesses2, Ss),
    run_puzzle2(Ss, Guess, Guesses).

:- det(run_puzzle2/3).
run_puzzle2(Ss, Guess, _Guesses), solved(Ss, Guess) => true.
run_puzzle2(Ss, _Guess, Guesses) =>
    run_puzzle(Ss, Guesses).

:- det(display_result/2).
display_result(Guesses, Ss) =>
    set_prolog_flag(color_term, true),
    fill_summary(unknown, Summary0),
    display_result(Guesses, Ss, Summary0).

:- det(display_result/3).
display_result([], Ss, Summary) =>
    dict_pairs(Summary, _, SummaryPairs),
    join(SummaryPairs, display_result_summary, write(' ')),
    format('~n', []).

:- det(display_result_summary/1).
display_result_summary(D-Type) =>
    display_fmt(Type, Fmt),
    ansi_format(Fmt, '~w', [D]).

:- det(display_fmt/2).
% Defined colors: black, red, green, yellow, blue, magenta, cyan, white
display_fmt(unknown, Fmt) => Fmt = [bold, bg(cyan),    fg(black)].
display_fmt(wrong,   Fmt) => Fmt = [bold, bg(black),   fg(white)].
display_fmt(correct, Fmt) => Fmt = [bold, bg(green),   fg(white)].
display_fmt(partial, Fmt) => Fmt = [bold, bg(magenta), fg(white)].

:- det(join/3).
join([], _, _) => true.
join([X], Call, _) => call(Call, X).
join([X|Xs], Call, Call2) =>
    call(Call, X),
    call(Call2),
    join(Xs, Call, Call2).


