% Examples from the CW631 paper
% https://www.cs.kuleuven.be/publicaties/rapporten/cw/CW631.pdf
% edited and updated:
%   reset/3 has different order of parameters and only sets Cont=0 (not Term)
%   call_continuation/1 not needed
%   renamed example sum/3 to sum_iterator/3 to avoid name collision with library(clpfdf)

:- use_module(library(readutil), [read_line_to_string/2]).

w(X) :- writeln(X).

p0 :-
    w(before_reset),
    reset(q0, Term, Cont),
    w([after_reset, term=Term, cont=Cont]),
    call(Cont).
q0 :-
    w(start_q),
    r0,
    w(end_q).
r0 :-
    w(start_r),
    shift(rterm),
    w(end_r).

p1 :-
    w(before_reset),
    q1(Cont, true, Term),
    w([after_reset, term=Term, cont=Cont]),
    call(Cont).
q1(Cont, ContAccu, Term) :-
    w(start_q),
    r1(Cont, ( w(end_q), ContAccu ), Term).
r1(Cont, ContAccu, Term) :-
    w(start_r),
    Term = rterm,
    Cont = ( w(end_r), ContAccu ).

%%% From paper ( reformatted, fixed reset/3, call_continuation/3 ) %%%

fromList([]).
fromList([X|Xs]) :-
    yield(X),
    fromList(Xs).

enumFromTo(L, U) :-
    (   L < U
    ->  yield(L),
        NL is L + 1,
        enumFromTo(NL, U)
    ;   true
    ).

enumFrom(L) :-
    yield(L),
    NL is L + 1,
    enumFrom(NL).

yield(Term) :-
    shift(yield(Term)).

init_iterator(Goal, Iterator) :-
    reset(Goal, YE, Cont),
    (   Cont == 0
    ->  Iterator = done
    ;   YE = yield(Element)
    ->  Iterator = next(Element, Cont)
    ).

next(next(Element, Cont), Element, Iterator) :-
    init_iterator(Cont, Iterator).

sum_iterator(Iterator, Acc, Sum) :-
    (   next(Iterator, X, NIterator)
    ->  NAcc is Acc + X,
        sum_iterator(NIterator, NAcc, Sum)
    ;   Acc = Sum
    ).

t1(Sum) :- init_iterator(fromList([7,2,3]),It), sum_iterator(It, 0, Sum).
% Sum = 12.

t2(Sum) :- init_iterator(enumFromTo(1,5),It), sum_iterator(It, 0, Sum).
% Sum = 10. % Paper has 15: error because EnumFromTo is open range [1,5)

sum_first_2(Sum) :-
    ask(X),
    ask(Y),
    Sum is X + Y.

sum_all(Sum) :-
    (   ask(X)
    ->  sum_all(Sum2),
        Sum is X + Sum2
    ;   Sum = 0
    ).

ask(X) :-
    shift(ask(X)).

with_read(Goal) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  true
    ;   Term = ask(X)
    ->  read(X),
        X \= end_of_file,
        with_read(Cont)
    ).

with_list(L, Goal) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  true
    ;   Term = ask(X)
    ->  L = [X|T],
        with_list(T, Cont)
    ).


t3a(Sum) :- with_list([1,2,3,4,5], sum_first_2(Sum)).
% Sum = 3.
t3b(Sum) :- with_list([1,2,3,4,5], sum_all(Sum)).
% Sum = 15.

t4a(Sum) :- with_read(sum_first_2(Sum)).
% |: 42.
% |: 7.
% Sum = 49.

t4b(Sum) :- with_read(sum_all(Sum)).
% |: 42.
% |: 7.
% |:
%Sum = 715.

t5a(Sum) :- play(sum_first_2(Sum), fromList([1,2])).
% Sum = 3.

t5b(Sum) :- play(sum_all(Sum), fromList([1,2,3,4,5])).
% Sum = 15.

t6a(Sum) :- play(sum_first_2(Sum), enumFromTo(7,10)).
% Sum = 15.

t6b(Sum) :- play(sum_all(Sum), enumFromTo(0,100)).
% Sum = 4950.

play(G1, G2) :-
    reset(G1, Term1, Cont1),
    (   Cont1 == 0
    ->  true
    ;   reset(G2, Term2, Cont2),
        Cont2 \= 0,
        sync(Term1, Term2),
        play(Cont1, Cont2)
    ).

sync(ask(X), yield(X)).
sync(yield(X), ask(X)).

mapL([], []).
mapL([X|Xs], [Y|Ys]) :-
    yield(X),
    ask(Y),
    mapL(Xs, Ys).
scanSum(Acc) :-
    ask(X),
    NAcc is Acc + X,
    yield(NAcc),
    scanSum(NAcc).

t7(L) :- play(mapL([1,2,3,4],L), scanSum(0)).
% L = [1,3,6,10].

transduce(IG, TG) :-
    reset(TG, TermT, ContT),
    transduce_(TermT, ContT, IG).
transduce_(0, _, _).
transduce_(yield(NValue), ContT, IG) :-
    yield(NValue),
    transduce(IG, ContT).
transduce_(ask(Value), ContT, IG) :-
    reset(IG, TermI, ContI),
    (   ContI == 0
    ->  true
    ;   TermI = yield(Value),
        transduce(ContI, ContT)
    ).

doubler :-
    ask(Value),
    NValue is Value * 2,
    yield(NValue),
doubler.

t8a(Sum) :- play(sum_first_2(Sum), transduce(fromList([1,2]), doubler)).
% Sum = 6.

t8b(Sum) :- play(sum_all(Sum), transduce(fromList([1,2,3,4]), doubler)).
% Sum = 20.


% implementing catch/3, throw/1 with reset/3, shift/1:
% catch_(Goal, _Catcher, _Handler) :-
%     nb_setval(thrown, nothrow),
%     catch_1(Goal).
% catch_(_Goal, Catcher, Handler) :-
%     nb_getval(thrown, Term),
%     Term = ball(Ball),
%     nb_setval(thrown, nothrow),
%     (   Ball = Catcher
%     ->  call(Handler)
%     ;   throw_(Term)
%     ).

% catch_1(Goal) :-
%     reset(Goal, Term, Cont),
%     (   Cont == 0
%     ->  true                    % no ball was thrown
%     ;   !,
%         nb_setval(thrown, Term),
%         fail
%     ).

% throw_(Ball) :-
%     copy_term(Ball, BC),
%     shift(ball(BC)).
