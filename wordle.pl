% -*- mode: Prolog; coding: utf-8 -*-

:- module(wordle, [assert_words/0,
                   word/5, word/1, word_chars/1,
                   match_word/4,
                   match_words/5,
                   match_words_and_score/4
                  ]).

:- encoding(utf8).

:- dynamic word/5.                % created with assert_words/0.
:- dynamic number_words/1.        % created with assert_words/0.
:- dynamic letter_probability/2.  % created with assert_words/0.
:- initialization(assert_words, after_load).

word(W) :-
    word_chars(Wcodes),
    string_chars(W, Wcodes).

word_chars([W1,W2,W3,W4,W5]) :-
    word(W1,W2,W3,W4,W5).

match_words_and_score(Pattern, In, NotIn, WordScoreList) :-
    setof(W, match_word(Pattern, In, NotIn, W), WordList),
    maplist(score_word(NotIn), WordList, WordScoreList0),
    keysort(WordScoreList0, WordScoreList2),
    pairs_values(WordScoreList2, WordScoreList).

score_word(NotIn, Word, ScoreNegative-Word) :-
    string_chars(Word, WordChars0),
    sort(WordChars0, WordChars), % dedup
    maplist(letter_probability(NotIn), WordChars, Probabilities),
    sumlist(Probabilities, Score),
    ScoreNegative is -Score.

letter_probability(NotIn, Letter, Probability) :-
    letter_probability(Letter, Probability0),
    (   memberchk(Letter, NotIn)
    ->  Probability is -Probability0  % TODO: this is rather crude
    ;   Probability = Probability0
    ).

match_words(Pattern, In, NotIn, WordList, NegativeWordListLen) :-
    setof(W, match_word(Pattern, In, NotIn, W), WordList),
    length(WordList, WordListLen),
    NegativeWordListLen is -WordListLen.

%! match_word(?Exact:string, +In:list(char), +NotIn:list(char), -Word:string) is nondet.
%
% Pattern is a string length 5 with each position either "." or a
% letter (atoms), with "." meaning "don't know" and a letter meaning
% that it must be this letter at this position.
%
% In is a list of letters (atoms) that must be in the word. If it
% contains a duplicate, then that letter must be in the word twice.
% For example, match_word("s....", [t,t], ...) would match "street"
% but not "steak".
%
% NotIn is a list of letters (atoms) that must not be in the word.
% This is checked after the letters in In; for example,
% `match_word("a....", [s], [s], W)` word not match "abyss", which
% has two "s"s.
%
% Word is a string containing a valid word that complies with the constraints.

match_word(Pattern, In, NotIn, Word) :-
    string_chars(Pattern, PatternChars),
    foldl(pattern_char, PatternChars, WordChars, [], NonExactChars),
    word_chars(WordChars),
    foldl(select, In, NonExactChars, RestChars),
    maplist(not_in(RestChars), NotIn),
    string_chars(Word, WordChars).

not_in(WordChars, NotIn) :- \+ member(NotIn, WordChars).

pattern_char('.', C,  V0, V) => V = [C|V0].
pattern_char(C,   C2, V0, V) => C = C2, V = V0.

letter_probability_(Letter, Probability) :-
    setof(W, word_contains_letter(Letter, W), Ws),
    length(Ws, NumberWords),
    number_words(N),
    Probability is float(NumberWords) / float(N).

word_contains_letter(Letter, WordChars) :-
    word_chars(WordChars),
    memberchk(Letter, WordChars).

assert_words :-
    assert_words('/home/peter/src/nerdle/wordle-small.txt').

assert_words(File) :-
    retractall(word(_,_,_,_,_)),
    setup_call_cleanup(open(File, read, In),
                       assert_words_(In),
                       close(In)),
    setof(W, word(W), Ws),
    retractall(number_words(_)),
    length(Ws, L),
    assertz(number_words(L)),
    string_chars("abcdefghijklmnopqrstuvwxyz", Letters),
    setof(letter_probability(Letter, P),
          ( member(Letter, Letters),
            letter_probability_(Letter, P) ),
          AssertsLP),
    maplist(assertz, AssertsLP).

assert_words_(In) :-
    repeat,
    (   read_line_to_string(In, Line),
        Line \== end_of_file
    ->  string_chars(Line, LineChars),
        WC =.. [word|LineChars],
        assertz(WC),
        fail
    ;   !
    ).

% ----- play a game -----

