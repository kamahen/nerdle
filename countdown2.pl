% -*- mode: Prolog; coding: utf-8 -*-

% This is a variation on countdown.pl, but using a different "eval".

% Solve the "countdown" problem.
% https://www.cs.nott.ac.uk/~pszgmh/countdown.pdf
% https://en.wikipedia.org/wiki/Countdown_(game_show)#Numbers_round

:- module(countdown,
          [solve/3]).

solve(Numbers, Target, Expr) :-
    subseq(Numbers, Numbers2),
    perm(Numbers2, NumbersPermuted),
    expr(NumbersPermuted, Expr),
    catch(Target is Expr, _, fail), % fail on zero-divide
    clean(Expr).

% Clean is used to remove rivial results, such as X+0 or 1*X are
% removed, as are commutative duplicates.
clean(X+Y) :- !, clean(X), X =\= 0,
                 clean(Y), X =< Y. /* Y =\= 0, */
clean(X*Y) :- !, clean(X), X > 1,
                 clean(Y), X =< Y. /* Y2 =\= 0, Y2 =\= 1, */
clean(X-Y) :- !, clean(Y), Y =\= 0,  % Y is evaluated before X
                 clean(X), X > Y.
clean(X/Y) :- !, clean(X), X > 1,
                 clean(Y), Y > 1,
                 Result is X/Y,
                 integer(Result).
clean(_). % number

%! expr(+Ns, -Expr).
% Ns is a list of numbers; Expr is an expression
% made by interpolating all the possible operators.
% For example, expr([1,2,3], num(1)+num(2)*num(3)).

expr([N], Expr) :- !, Expr = N. % was: Expr=num(N)
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
