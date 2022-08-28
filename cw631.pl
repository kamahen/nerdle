% Examples from the CW631 paper
% https://www.cs.kuleuven.be/publicaties/rapporten/cw/CW631.pdf
% and
% https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/iclp2013.pdf
% edited and updated:
%   - reset/3 has different order of parameters and only sets Cont=0 (not Term)
%   - call_continuation/1 not needed
%   - renamed example sum/3 to sum_iterator/3 to avoid name collision with library(clpfdf)
%   - renamed example phrase/3 to dcg_phrase/3 to avoid name collision with builtin
%   - some predicates have been extended with a Stream parameter
%   - added some more examples (e.g., the paper's sum/3 examples only
%     processed the first 2 items; these have been extended to process
%     all items, and the sum/3 predicate renamed correspondingly)
%   - for all examples, code has been added to allow composing effect handlers
%   - bug fixes
%  Examples have been put into unit-test form; you can run them by
%  ?- test_cw631.

:- module(cw631, [test_cw631/0,
                  reset_shift/4,
                  true_false/2,
                  % fromList/1,
                  % enumFromTo/2,
                  % enumFrom/1,
                  % yield/1,
                  % init_iterator/2,
                  % next/3,
                  % sum_iterator/3,
                  % sum_first_2/1,
                  % sum_all/1,
                  % ask/1,
                  % with_read/2,
                  % with_list/2,
                  % sync/2,
                  % mapL/2,
                  % scanSum/1,
                  % transduce/2,
                  % doubler/0,
                  % state_get/1,
                  % state_put/1,
                  % run_state/3,
                  % inc/0,
                  % c/1,
                  % dcg_phrase/3,
                  % ab/1,
                  % ab/0,
                  p0/0,
                  p1/0
                 ]).

:- meta_predicate
    reset_shift(0, 3, 1, ?),
    true_false(0, -).

% :- set_prolog_flag(autoload, false).

:- use_module(library(plunit)).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(check)).

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

%%% General reset-shift %%%

% reset_shift/4 is not in either of the papers, but encapuslates a
%     general form that almost all the predicates use (so far, the
%     exception is transduce/2, and even that is similar, so there may
%     be an even more general form.

%! reset_shift(:Goal, :Catch, :End, +Args).
% Call reset/3 with Goal:

% - if Goal called shift(Term), then call Catch with Term, the
%   Continuation from reset/3, and Args. If Catch does not succeed at
%   least once, propagate the Term. There is one more argument:
%   Success. This is used to handle the situation where the Term
%   matched, so we don't want to propagate the Term; but the Catch
%   goal failed. Set Success to `true` to indicate that it succeeded
%   and `false` if it failed. See true_false/2 for how to make a
%   regular predicate return `true` or `false`.
% - if Goal didn't call shift/1, call End with Args.
reset_shift(Goal, Catch, End, Args) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  call(End, Args)
    ;   call(Catch, Term, Cont, Args, Success)
    *-> Success == true
    ;   shift(Term),                % propagate unknown
        reset_shift(Cont, Catch, End, Args)
    ).

true_false(Goal, Success) :-
    (   Goal
    *-> Success = true
    ;   Success = false
    ).

:- begin_tests(cw631).

pp(1). % For testing true_false/2.
pp(2).

test(true_false, all(P-S == [1-true, 2-true])) :-
    true_false(pp(P), S).
test(true_false, S = true) :-
    true_false(pp(2), S).
test(true_false, S == false) :-
    true_false(pp(3), S).

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
    reset_shift(Goal, init_iterator_catch, init_iterator_end, [Iterator]).

% init_iterator_catch(?Term, :Cont, Args, -Success).
init_iterator_catch(yield(Element), Cont, [next(Element, Cont)], true).

init_iterator_end([done]).

next(next(Element, Cont), Element, Iterator) :-
    init_iterator(Cont, Iterator).

% This is the original code, Without using reset_shift/4:

:- if(false). % commented out original code from paper
init_iterator(Goal, Iterator) :-
    reset(Goal, YE, Cont),
    (   Cont == 0
    ->  Iterator = done
    ;   YE = yield(Element)
    ->  Iterator = next(Element, Cont)
    ;   shift(YE), % propagate unknown
        init_iterator(Cont, Iterator)
    ).
:- endif. % commented out original code from paper

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

test(sum_iterator, Sum == 12) :-
    init_iterator(fromList([7,2,3]), It),
    sum_iterator(It, 0, Sum).

test(sum_iterator, Sum == 10) :-
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

:- if(false). % commented out original code from paper
with_read(Stream, Goal) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  true
    ;   Term = ask(X)
    ->  read(Stream, X),
        X \= end_of_file,
        with_read(Stream, Cont)
    ;   shift(Term), % propagate unknown
        with_read(Stream, Cont)
    ).
:- endif. % commented out original code from paper

with_read(Stream, Goal) :-
    reset_shift(Goal, with_read_catch, with_read_end, [Stream]).

with_read_catch(ask(X), Cont, [Stream], Success) :-
    % TODO: rewrite this using true_false/2
    (   read(Stream, X),
        X \= end_of_file
    ->  Success = true,
        with_read(Stream, Cont)
    ;   Success = false
    ).

with_read_end([_Stream]).

/* The data source can be modularly replaced: */

:- if(false). % commented out original code from paper
with_list(L, Goal) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  true
    ;   Term = ask(X)
    ->  L = [X|T],
        with_list(T, Cont)
    ;   shift(Term), % propagate unknown
        with_list(L, Cont)
    ).
:- endif. % commented out original code from paper

with_list(L, Goal) :-
    reset_shift(Goal, with_list_catch, write_read_end, [L]).

with_list_catch(ask(X), Cont, [L], Success) :-
    % TODO: rewrite this using true_false/2
    (   L = [X|T]
    ->  Success = true,
        with_list(T, Cont)
    ;   Success = false
    ).

write_read_end([_L]).

test(with_list_sum_first_2, Sum == 3) :-
    with_list([1,2,3,4,5],
              sum_first_2(Sum)).

test(with_list_sum_all, Sum == 15) :-
    with_list([1,2,3,4,5],
              sum_all(Sum)).

test(with_read_sum_first_2, Sum == 49) :-
    open_string("42.
                 7.
                ",
               Inputs),
    with_read(Inputs, sum_first_2(Sum)).
% |: 42.
% |: 7.
% Sum = 49.
test(with_read_sum_first_2, Sum == 49) :-
    open_string("42.
                 7.
                 666.
                ",
               Inputs),
    with_read(Inputs, sum_first_2(Sum)).

test(with_read_sum_all, Sum == 715) :-
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

test(sum_first_2_fromList, Sum == 3) :-
    play(sum_first_2(Sum),
         fromList([1,2])).

test(sum_all_fromList, Sum == 15) :-
    play(sum_all(Sum),
         fromList([1,2,3,4,5])).

test(sum_first_2_enumFromTo, Sum == 15) :-
    play(sum_first_2(Sum),
         enumFromTo(7,10)).

test(sum_all_enumFromTo, Sum = 4950) :-
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
    % TODO: propagate unknown?
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

test(play_mapL_scanSum, L == [1,3,6,10]) :-
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
    % TODO: propagate unknown?
    ).

/* The doubler/2 predicate is an example of a transducer that doubles
   the values it receives. */

doubler :-
    ask(Value),
    NValue is Value * 2,
    yield(NValue),
doubler.

test(play_sum_first_2_transduce_fromList_doubler, Sum == 6) :-
    play(sum_first_2(Sum),
         transduce(fromList([1,2]),
                   doubler)).

test(play_sum_all_transduce_fromList_doubler, Sum == 20) :-
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

%%% DCG and state monad from ICLP paper %%%

% -- State

state_get(S) :- shift(get(S)).

state_put(S) :- shift(put(S)).

:- if(false). % commented out original code from paper
run_state(Goal, Sin, Sout) :-
    reset(Goal, Command, Cont),
    (   Cont == 0
    ->  Sout = Sin
    ;   Command = get(S)
    ->  S = Sin,
        run_state(Cont, Sin, Sout)
    ;   Command = put(S)
    ->  run_state(Cont, S, Sout)
    ;   shift(Command),  % propagate unknown
        run_state(Cont, Sin, Sout)
    ).
:- endif. % commented out original code from paper

run_state(Goal, Sin, Sout) :-
    reset_shift(Goal, run_state_catch, run_state_end, [Sin, Sout]).

run_state_catch(get(S), Cont, [S, Sout], true) :- run_state(Cont, S, Sout).
run_state_catch(put(S), Cont, [_, Sout], true) :- run_state(Cont, S, Sout).

run_state_end([S, S]).

inc :-
    state_get(S),
    S1 is S + 1,
    state_put(S1).

test(run_state_inc, S == 1) :-
    run_state(inc, 0, S).
test(run_state_inc, S == 4) :-
    run_state((inc, inc, inc), 1, S).

% -- DCG

c(E) :- shift(c(E)).

:- if(false). % commented out original code from paper
dcg_phrase(Goal, Lin, Lout) :-
    reset(Goal, Term, Cont),
    (   Cont == 0
    ->  Lin = Lout
    ;   Term = c(E)
    ->  Lin = [E|Lmid],
        dcg_phrase(Cont, Lmid, Lout)
    ;   shift(Term),  % propagate unknown
        dcg_phrase(Cont, Lin, Lout)
    ).
:- endif. % commented out original code from paper

dcg_phrase(Goal, Lin, Lout) :-
    reset_shift(Goal, dcg_phrase_catch, dcg_phrase_end, [Lin, Lout]).

dcg_phrase_catch(c(E), Cont, [[E|Lmid], Lout], true) :-
    dcg_phrase(Cont, Lmid, Lout).

dcg_phrase_end([L, L]).

ab(0).
ab(N) :-
    c(a),
    c(b),
    ab(M),
    N is M + 1.

test(dcg, [nondet, N == 2]) :-
    dcg_phrase(ab(N), [a,b,a,b], []).
test(dcg, [nondet, L == [a,b,a,b,a,b]]) :-
    dcg_phrase(ab(3), L, []).

ab.
ab :-
    c(a),
    c(b),
    inc,
    ab.

test(dcg_state, [nondet, S == 2]) :-
    run_state(dcg_phrase(ab, [a,b,a,b], []), 0, S).

:- end_tests(cw631).
