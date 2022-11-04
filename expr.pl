% -*- mode: Prolog; coding: utf-8 -*-

:- module(expr,
          [expr/2,
           eval/2
          ]).

:- encoding(utf8).

% Predicates to parse an expresssion string to a term, and to evaluate
% the term.

:- set_prolog_flag(prefer_rationals, true).

:- det(eval/2).
%! eval(+Expr, -Result: rational) is det.
% Expr is a term representing an arithmetic expression. Delays
% as needed when it encounters an uninstantiated variable.
% Result is a rational (assuming flag `prefer_rationals` is set).

eval(Expr, Result) :-
    freeze(Expr, eval_(Expr, Result)).

eval_(X+Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2+Y2.
eval_(X-Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2-Y2.
eval_(X*Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2*Y2.
eval_(X/Y, Result) => eval(X, X2), eval(Y, Y2), Result is X2/Y2.
eval_(X,   Result), rational(X) => Result = X.

:- det(expr/2).
%! expr(+String:string, -Expr) is det.
% Parse an expression to produce a term. Fails if String isn't a valid
% expression. The parser is "greedy" and has cuts in it to ensure
% determinicity.
expr(String, Expr) :-
    string_chars(String, Chars),
    phrase(expr(Expr), Chars).

%! num(+String:string, -Num) is det.
% Parse a number (fails if an invalid string). It is not allowed to
% start with "0" nor with a "+" or "-" sign.
num(String, Num) :-
    string_chars(String, Chars),
    phrase(num(Num), Chars).

expr(Expr) --> term(Term), expr_(Term, Expr).

expr_(Left, Expr) -->
    plus_minus(Left, Term, Left2),
    term(Term), !,
    expr_(Left2, Expr).
expr_(Expr, Expr) --> [].

plus_minus(Left, Term, Left+Term) --> ['+'].
plus_minus(Left, Term, Left-Term) --> ['-'].

term(Term) -->
    num(Num), !,
    term_(Num, Term).

term_(Left, Term) -->
    times_divide(Left, Num, Left2),
    num(Num), !,
    term_(Left2, Term).
term_(Expr, Expr) --> [].

times_divide(Left, Num, Left*Num) --> ['*'].
times_divide(Left, Num, Left/Num) --> ['/'].

num(Num) --> digit1(Accum), digits(Accum, Num).
num(0) --> ['0'].

digits(Accum, Num) -->
    digit0(N), !,
    digits(Accum * 10 + N, Num).
digits(Accum, Accum) --> [].

digit0(Num) --> [D], { digit0(D, Num) }, !.
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

digit1(Num) --> [D], { digit1(D, Num) }, !.
digit1('1', 1).
digit1('2', 2).
digit1('3', 3).
digit1('4', 4).
digit1('5', 5).
digit1('6', 6).
digit1('7', 7).
digit1('8', 8).
digit1('9', 9).
