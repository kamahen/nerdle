% -*- mode: Prolog; coding: utf-8 -*-

% Solve the "countdown" problem.
% https://www.cs.nott.ac.uk/~pszgmh/countdown.pdf
% https://en.wikipedia.org/wiki/Countdown_(game_show)#Numbers_round

:- module(countdown,
          [solve/3]).

:- set_prolog_flag(prefer_rationals, true).

solve(Numbers, Target, Expr) :-
    subseq(Numbers, Numbers2),
    permutation(Numbers2, NumbersPermuted),
    expr(NumbersPermuted, Expr0),
    eval(Expr0, Target, Expr).

%! eval(+Expr, -Result, -Clean).
% Expr is evaluated into Result and a "clean" version of Expr is
% put into Clean (stripping num(...)).
% Trivial results, such as X+0 or 1*X are removed, as are duplicates
% by commutivity (e.g., 2+3 and 3+2).
eval(X+Y, Result, Clean) => eval(X, X2, Xc), eval(Y, Y2, Yc), Result is X2+Y2,
                                                               Clean = Xc+Yc,
                                                               Result \= X2,
                                                               Result \= Y2,
                                                               X2 =< Y2.
eval(X-Y, Result, Clean) => eval(X, X2, Xc), eval(Y, Y2, Yc), Result is X2-Y2,
                                                                Clean = Xc-Yc,
                                                              Result >= 0,
                                                              Result \= X2.
eval(X*Y, Result, Clean) => eval(X, X2, Xc), eval(Y, Y2, Yc), Result is X2*Y2,
                                                                Clean = Xc*Yc,
                                                              Result \= X2,
                                                              Result \= Y2,
                                                              X2 =< Y2.
eval(X/Y, Result, Clean) => eval(X, X2, Xc), eval(Y, Y2, Yc), Y2 \= 0,
                                                               Result is X2/Y2,
                                                                 Clean = Xc/Yc,
                                                               Result \= X2,
                                                               integer(Result).
eval(num(X), Result, Clean), integer(X), X >= 0 => Result = X,
                                                   Clean = X.
eval(num(_), _, _) => fail.

expr([], _) => fail.
expr([N], Expr) => Expr = num(N).
expr(Ns, Expr) =>
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

