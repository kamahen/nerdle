% -*- mode: Prolog; coding: utf-8 -*-

% This is a variation on countdown2.pl, but using a different "eval".
% Thanks to Jan Wielemaker
% https://swi-prolog.discourse.group/t/prolog-program-is-much-slower-than-haskell-why/6266/5
% A few small changes have been made, e.g., changing "=>" to ":- !",
% to allow running on other Prologs, and the expr/3 code was changed
% to have the `[_|_]` constraints before the append. Also, some mode
% declaration headers have been added.

% Solve the "countdown" problem.
% https://www.cs.nott.ac.uk/~pszgmh/countdown.pdf
% https://en.wikipedia.org/wiki/Countdown_(game_show)#Numbers_round
% https://swi-prolog.discourse.group/t/prolog-program-is-much-slower-than-haskell-why/6266

:- module(countdown,
          [solve/3]).

:- use_module(library(lists), [append/3, select/3]).

solve(Numbers, Target, Expr) :-
    subseq(Numbers, Numbers2),
    perm(Numbers2, NumbersPermuted),
    expr(NumbersPermuted, Expr, Target).

%! expr(+Ns:list(int), -Expr, -Value:int) is nondet.
% Ns is a list of numbers; Expr is an expression
% made by interpolating all the possible operators.
% For example, expr([1,2,3], 1+2*3, 7).

expr([N], Expr, Value) :- !, Expr = N, Value = N.
expr(Ns, Expr, Value) :-
    Left = [_|_], Right = [_|_],
    append(Left, Right, Ns),
    expr(Left, LeftExpr, LValue),
    expr(Right, RightExpr, RValue),
    op_primitive(LeftExpr, RightExpr, Expr, LValue, RValue, Value).

%! op_primitive(+Left, +Right, -Expr, +LValue:int, +RValue:int, -Value:int) is nondet.
op_primitive(Left, Right, Left+Right, LV, RV, V) :-
    LV =< RV, RV =\= 0, V is LV+RV.
op_primitive(Left, Right, Left*Right, LV, RV, V) :-
    LV =< RV, RV > 1, V is LV*RV.
op_primitive(Left, Right, Left-Right, LV, RV, V) :-
    RV =\= 0, LV > RV, V is LV-RV.
op_primitive(Left, Right, Left/Right, LV, RV, V) :-
    RV =\= 0, RV =\= 1, V is LV/RV, integer(V).

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
