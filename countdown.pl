% -*- mode: Prolog; coding: utf-8 -*-

% Solve the "countdown" problem.
% https://www.cs.nott.ac.uk/~pszgmh/countdown.pdf
% https://en.wikipedia.org/wiki/Countdown_(game_show)#Numbers_round

:- module(countdown,
          [solve/3]).

:- use_module(library(lists), [append/3, select/3]).

solve(Numbers, Target, ExprClean) :-
    subseq(Numbers, Numbers2),
    perm(Numbers2, NumbersPermuted),
    expr(NumbersPermuted, Expr),
    eval(Expr, Target),
    clean(Expr, ExprClean).

%! eval(+Expr, -Result)
% Expr is evaluated into Result.
% Trivial results, such as X+0 or 1*X are removed, as are commutative duplicates
% by commutivity (e.g., 2+3 and 3+2).
% Commented out are some redundant tests
eval(X+Y, Result) :- eval(X, X2), X2 =\= 0,
                     eval(Y, Y2), X2 =< Y2, /* Y2 =\= 0, */
                     Result is X2+Y2.
eval(X*Y, Result) :- eval(X, X2), X2 > 1,
                     eval(Y, Y2), X2 =< Y2, /* Y2 =\= 0, Y2 =\= 1, */
                     Result is X2*Y2.
eval(X-Y, Result) :- eval(Y, Y2), Y2 =\= 0,  % Y is evaluated before X
                     eval(X, X2), X2 > Y2,
                     Result is X2-Y2.
eval(X/Y, Result) :- eval(X, X2), X2 > 1,
                     eval(Y, Y2), Y2 > 1,
                     Result is X2/Y2,
                     integer(Result).
eval(num(X), X).  % integer(X), X >= 0

clean(X+Y, Xc+Yc) :- clean(X, Xc), clean(Y, Yc).
clean(X*Y, Xc*Yc) :- clean(X, Xc), clean(Y, Yc).
clean(X-Y, Xc-Yc) :- clean(X, Xc), clean(Y, Yc).
clean(X/Y, Xc/Yc) :- clean(X, Xc), clean(Y, Yc).
clean(num(X), X).

%! expr(+Ns, -Expr).
% Ns is a list of numbers; Expr is an expression
% made by interpolating all the possible operators.
% For example, expr([1,2,3], num(1)+num(2)*num(3)).

expr([N], Expr) :- !, Expr = num(N).
expr(Ns, Expr) :-
    append(Left, Right, Ns),
    expr_(Left, Right, Expr).

expr_(Left, Right, Expr) :-
    Left = [_|_],
    Right = [_|_],
    expr(Left, LeftExpr),
    expr(Right, RightExpr),
    op_primitive(LeftExpr, RightExpr, Expr).

op_primitive(Left, Right, Left+Right).     % Left @=< Right,
op_primitive(Left, Right, Left*Right).     % Left @=< Right,
op_primitive(Left, Right, Left-Right).
op_primitive(Left, Right, Left/Right).

% Permutation - taken from library(lists).
perm([], []).
perm(List, [First|Perm]) :-
    select(First, List, Rest),
    perm(Rest, Perm).

subseq(List, Subseq) :-
    Subseq = [_|_],
    subseq_(List, Subseq).

subseq_([], []).
subseq_([Head|Tail], [Head|SubTail]) :-
    subseq_(Tail, SubTail).
subseq_([_|Tail], SubSequence) :-
    subseq_(Tail, SubSequence).

% subseq_([], Subseq) => Subseq = [].
% subseq_([Head|Tail], Subseq) =>
%     subseq_(Head, Tail, Subseq).

% subseq_(Head, Tail, [Head|SubTail]) :-
%     subseq_(Tail, SubTail).
% subseq_(_, Tail, SubSequence) :-
%     subseq_(Tail, SubSequence).
