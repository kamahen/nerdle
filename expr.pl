% -*- mode: Prolog; coding: utf-8 -*-

:- module(expr,
          [expr//1,
           random_expr_string/1,
           random_expr_chars/8,
           random_expr_chars/1,
           num//1,
           eval/2,
           puzzle/2
          ]).

:- encoding(utf8).

:- use_module(library(random), [random_between/3]).
:- use_module(all_puzzles_facts, [a_puzzle/2, a_puzzle/9,
                                  a_puzzle_i_min/1,a_puzzle_i_max/1]).

%! puzzle(-Left:list, -Right:list) is nondet.
% Instantiate Left and Right to two lists that can be combined with a
% '='. Left's and Right's contents are uninstantiated.
puzzle(Left, Right) :-
    % 9*99=891, so Left must be at least 4 in length
    between(4, 6, LenLeft),
    puzzle_(LenLeft, Left, Right).

puzzle_(LenLeft, Left, Right) :-
    LenRight is 7 - LenLeft,
    length(Left, LenLeft),
    length(Right, LenRight).

% Predicates to parse an expresssion string to a term, and to evaluate
% the term.

:- set_prolog_flag(prefer_rationals, true).

%! eval(+Expr, -Result: rational) is nondet.
% Expr is a term representing an arithmetic expression. Delays
% as needed when it encounters an uninstantiated variable.
% Fails for things like divide-by-zero.
% Result is a rational (assuming flag `prefer_rationals` is set).

eval(Expr, Result) :-
    freeze(Expr, eval_(Expr, Result)).

eval_(X+Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2+Y2.
eval_(X-Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2-Y2.
eval_(X*Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2*Y2.
eval_(X/Y, Result) => eval(X, X2), eval(Y, Y2), \+ Y2 = 0,
                                                Result is X2/Y2.
eval_(X,   Result), rational(X) => Result = X.

:- det(random_expr_chars/8).
random_expr_chars(C1,C2,C3,C4,C5,C6,C7,C8) :-
    a_puzzle_i_min(Min),
    a_puzzle_i_max(Max),
    random_between(Min, Max, I),
    a_puzzle(I, C1,C2,C3,C4,C5,C6,C7,C8).

:- det(random_expr_chars/1).
random_expr_chars([C1,C2,C3,C4,C5,C6,C7,C8]) :-
    random_expr_chars(C1,C2,C3,C4,C5,C6,C7,C8).

:- det(random_expr_string/1).
%! random_expr_string(-ExprString:string) is det.
random_expr_string(ExprString) :-
    a_puzzle_i_min(Min),
    a_puzzle_i_max(Max),
    random_between(Min, Max, I),
    a_puzzle(I, ExprString).

/*
  findall(C1, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
  [1-3473, 2-2151, 3-1917, 4-1894, 9-1852, 6-1779, 5-1760, 8-1758, 7-1731, 0-251]

  findall(C2, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
  [(*)-3094, (+)-2343, 0-1532, 2-1340, 4-1278, 6-1222, 8-1179, 1-1171, 5-1168, 3-1097, 7-1023, 9-966, (-)-739, (/)-414]

  C2='*', findall(C1, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
  [8-403, 9-397, 6-390, 7-380, 5-366, 4-357, 3-324, 2-271, 1-206]

  C1='8', C2='*', findall(C3, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).

  C1='8', C2='*', C3='9', findall(C4, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
  [9-51, 6-50, 3-48, 5-48, 4-47, 7-47, 8-47, 2-42, 1-23]

  ... let's try ['8',*,'9','2','=','7','3','6']
            or  ['8',*,'9','/','2','=','3','6']

  Let's look at totals for all labels
  findall(C, char_in_puzzle(C), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
     [(=)-18566, 1-15159, 2-12089, 4-10816, 3-10467, 6-10009, 5-9990, 8-9495, (-)-9066, 7-8998, 9-8950, (+)-8256, (*)-6122, 0-5973, (/)-4572]

  C5='=', aggregate_all(count, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Count).
  Count =  1318
  C6='='  10914
  C7='='   6334

  So, maybe:
  ['4','*','9','-','1','=','3','5']

  Or (better?):
  ['6','+','9','-','3','=','1','2']
      C7='1', C8='2',C2='+',C6='=', puzzle_fill([C1,C2,C3,C4,C5,C6,C7,C8]),P=[C1,C2,C3,C4,C5,C6,C7,C8], sort(P,PS), length(PS,8).
*/

sort_by_frequency(List, Sorted) :-
    % TODO: use library(aggregate)
    msort(List, ListSorted),
    clumped(ListSorted, ListClumped),
    maplist(flip_negative_k_v, ListClumped, ListClumpedVK),
    msort(ListClumpedVK, ListClumpedVKsorted),
    maplist(flip_negative_v_k, ListClumpedVKsorted, Sorted).

char_in_puzzle(C1) :- a_puzzle(_,C1,_C2,_C3,_C4,_C5,_C6,_C7,_C8).
char_in_puzzle(C2) :- a_puzzle(_,_C1,C2,_C3,_C4,_C5,_C6,_C7,_C8).
char_in_puzzle(C3) :- a_puzzle(_,_C1,_C2,C3,_C4,_C5,_C6,_C7,_C8).
char_in_puzzle(C4) :- a_puzzle(_,_C1,_C2,_C3,C4,_C5,_C6,_C7,_C8).
char_in_puzzle(C5) :- a_puzzle(_,_C1,_C2,_C3,_C4,C5,_C6,_C7,_C8).
char_in_puzzle(C6) :- a_puzzle(_,_C1,_C2,_C3,_C4,_C5,C6,_C7,_C8).
char_in_puzzle(C7) :- a_puzzle(_,_C1,_C2,_C3,_C4,_C5,_C6,C7,_C8).
char_in_puzzle(C8) :- a_puzzle(_,_C1,_C2,_C3,_C4,_C5,_C6,_C7,C8).
flip_negative_k_v(K-V, V2-K) :-
    V2 is - V.

flip_negative_v_k(V-K, K-V2) :-
    V2 is - V.

%! expr(?Expr:term)//
% Parse an expression (list of chars) to produce a term. Fails if it
% doesn't get isn't a valid expression.  expr//1 can be used either to
% parse a list of chars to produce a term, or it can process a term to
% produce a list of chars; and if the list of chars is bounded in
% size, it can generate all combinations of terms and lists of chars.
expr(Expr) --> term(Term), expr_(Term, Expr).

expr_(Left, Expr) -->
    plus_minus(Left, Term, Left2),
    term(Term),
    expr_(Left2, Expr).
expr_(Expr, Expr) --> [].

plus_minus(Left, Term, Left+Term) --> ['+'].
plus_minus(Left, Term, Left-Term) --> ['-'].

term(Term) -->
    num(Num),
    term_(Num, Term).

term_(Left, Term) -->
    times_divide(Left, Num, Left2),
    num(Num),
    term_(Left2, Term).
term_(Expr, Expr) --> [].

times_divide(Left, Num, Left*Num) --> ['*'].
times_divide(Left, Num, Left/Num) --> ['/'].

%! num(?Num)//
% Parse a number (fails if an invalid string). It is not allowed to
% start with "0" nor with a "+" or "-" sign.
num(Num) --> digit1(Accum), digits(Accum, Num).
num(0) --> ['0'].

digits(Accum, Num) -->
    digit0(N),
    digits(Accum * 10 + N, Num).
digits(Accum, Accum) --> [].

digit0(Num) --> [D], { digit0(D, Num) }.
digit0('0', 0).
digit0('1', 1).
digit0('2', 2).
digit0('3', 3).
digit0('4', 4).
digit0('5', 5).
digit0('6', 6).
digit0('7', 7).
digit0('8', 8).
digit0('9', 9).

digit1(Num) --> [D], { digit1(D, Num) }.
digit1('1', 1).
digit1('2', 2).
digit1('3', 3).
digit1('4', 4).
digit1('5', 5).
digit1('6', 6).
digit1('7', 7).
digit1('8', 8).
digit1('9', 9).
