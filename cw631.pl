% Examples from the CW631 paper
% https://www.cs.kuleuven.be/publicaties/rapporten/cw/CW631.pdf
% edited and updated:
%   - reset/3 has different order of parameters and only sets Cont=0 (not Term)
%   - call_continuation/1 not needed
%   - renamed example sum/3 to sum_iterator/3 to avoid name collision with library(clpfdf)
%   - some predicates have been extended with a Stream parameter
%   - added some more examples (e.g., the paper's sum/3 examples only
%     processed the first 2 items; these have been extended to process
%     all items, and the sum/3 predicate renamed correspondingly)
%  Examples have been put into unit-test form; you can run the by
%  ?- test_cw631.
%
% TODO: add examples from the ICLP paper, for example composing affect
%       handlers; and also the state monad and the DCG example.
%    https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/iclp2013.pdf

:- module(cw631, [test_cw631/0,
                  p0/0,
                  p1/0]).

:- use_module(library(plunit)).
:- use_module(library(readutil), [read_line_to_string/2]).

test_cw631 :-
    run_tests([ cw631
              ]).

/* The p0/0 and p1/0 predicates are examples of a program using
   reset/shift and a close equivalent that could be achieved by
   program transformation. Section 4 describes the transformation
   completely. */

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

% p1 is the transformed form of p0
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

:- begin_tests(cw631).

%%% Iterators %%%

/* Coroutine-based iterators exist in many languages
   (e.g. Python). Iterators are created by generators that use the
   yield keyword to a suspend and return an intermediate value before
   continuing with the generation of more values. We support a similar
   yield/1 operation in Prolog, which allows us to define various
   kinds of generators: */

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

%%% Generators %%%

/* Generators resemble lazy and potentially infinite streams. The init
   iterator/2 predicate packages a generator goal in an iterator
   structure that captures the last yielded element and the
   generator’s continuation. The next/3 predicate extracts this
   element and builds the new iterator from the continuation. */

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

/* Consumers of iterators are independent of the particular
   generator. Note that in a sense yield/1 generalizes Prolog’s
   write/1 built-in: the coroutine runs in a context that consumes its
   output in a user-defined way. */

sum_iterator(Iterator, Acc, Sum) :-
    (   next(Iterator, X, NIterator)
    ->  NAcc is Acc + X,
        sum_iterator(NIterator, NAcc, Sum)
    ;   Acc = Sum
    ).

test(t1, Sum == 12) :-
    init_iterator(fromList([7,2,3]), It),
    sum_iterator(It, 0, Sum).

test(t2, Sum == 10) :-
    % The paper has Sum=15:
    %   this is wrong because EnumFromTo is open range [1,5)
    init_iterator(enumFromTo(1,5), It),
    sum_iterator(It, 0, Sum).

/* Iteratees are the opposite of iterators: they suspend to request
   external input. We provide the ask/1 predicate for this purpose.
   For instance, this predicate requests two numbers and adds them up:
   */

sum_first_2(Sum) :-
    ask(X),
    ask(Y),
    Sum is X + Y.

/* The ask/1 predicate generalizes Prolog’s read/1 built-in: the
   coroutine’s context determines the source of the data. */

sum_all(Sum) :-
    (   ask(X)
    ->  sum_all(Sum2),
        Sum is X + Sum2
    ;   Sum = 0
    ).

ask(X) :-
    shift(ask(X)).

with_read(Stream, Goal) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  true
    ;   Term = ask(X)
    ->  read(Stream, X),
        X \= end_of_file,
        with_read(Stream, Cont)
    ).

/* The data source can be modularly replaced: */

with_list(L, Goal) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  true
    ;   Term = ask(X)
    ->  L = [X|T],
        with_list(T, Cont)
    ).


test(t3a, Sum == 3) :-
    with_list([1,2,3,4,5],
              sum_first_2(Sum)).

test(t3b, Sum == 15) :-
    with_list([1,2,3,4,5],
              sum_all(Sum)).

test(t4a, Sum == 49) :-
    open_string("42.
                 7.
                ",
               Inputs),
    with_read(Inputs, sum_first_2(Sum)).
% |: 42.
% |: 7.
% Sum = 49.
test(t4a2, Sum == 49) :-
    open_string("42.
                 7.
                 666.
                ",
               Inputs),
    with_read(Inputs, sum_first_2(Sum)).

test(t4b, Sum == 715) :-
    open_string("42.
                 7.
                 666.
               ",
               Inputs),
    with_read(Inputs,
              sum_all(Sum)).

%%% General Coroutines %%%

/* Iterator and iteratee coroutines can easily be played against each
   other: */

test(t5a, Sum == 3) :-
    play(sum_first_2(Sum),
         fromList([1,2])).

test(t5b, Sum == 15) :-
    play(sum_all(Sum),
         fromList([1,2,3,4,5])).

test(t6a, Sum == 15) :-
    play(sum_first_2(Sum),
         enumFromTo(7,10)).

test(t6b, Sum = 4950) :-
    play(sum_all(Sum),
         enumFromTo(0,100)).

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

/* More generally, coroutines can mix yield/1 and ask/1 to communicate
   in two directions.
  */

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

test(t7, L == [1,3,6,10]) :-
    play(mapL([1,2,3,4],L),
         scanSum(0)).

/* Compare this coroutine-based approach to Sterling and
   Kirschenbaum’s approach of applying techniques to
   skeletons[28]. The former are much more lightweight and uniform. In
   contrast, the latter rely on program transformation or
   meta-interpretation and are more ad-hoc. */

%%% Transducers %%%

/* A transducer transforms an iterator of one kind into an iterator of
   another kind. A transducer communicates with two parties: it asks
   values from an underlying iterator and uses these to produce other
   values it yields to an iteratee.  The transduce/2 predicate applies
   a transducer to an iterator.
  */

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

/* The doubler/2 predicate is an example of a transducer that doubles
   the values it receives. */

doubler :-
    ask(Value),
    NValue is Value * 2,
    yield(NValue),
doubler.

test(t8a, Sum == 6) :-
    play(sum_first_2(Sum),
         transduce(fromList([1,2]),
                   doubler)).

test(t8b, Sum == 20) :-
    play(sum_all(Sum),
         transduce(fromList([1,2,3,4]),
                   doubler)).


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

:- end_tests(cw631).

