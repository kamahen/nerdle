% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_nerdle,
	  [ test_nerdle/0
	  ]).

:- encoding(utf8).

:- use_module(library(plunit)).
:- use_module(nerdle).
:- use_module(expr).
:- use_module(library(lists), [append/3]).
:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(debug), [assertion/1]).

test_nerdle :-
    run_tests([ nerdle
	      ]).

:- begin_tests(nerdle).

test(eval, all(T-R == [0-0])) :-
    phrase(expr(T), ['0']),
    eval(T, R).
test(eval, all(T-R == [1-1])) :-
    phrase(expr(T), ['1']),
    eval(T, R).
test(eval, all(T-R == [(1*10+2)*10+3-123])) :-
    phrase(expr(T), ['1','2','3']),
    eval(T, R).
test(eval, all(T-R == [(4/8*(1*10+8)+5)-14])) :-
     phrase(expr(T), ['4','/','8','*','1','8','+','5']),
     eval(T, R).
test(eval, all(E == [5+((2*10+0)*10+4),
                     5+((2*10+1)*10+4),
                     5+((2*10+2)*10+4),
                     5+((2*10+3)*10+4),
                     5+((2*10+4)*10+4),
                     5+((2*10+5)*10+4),
                     5+((2*10+6)*10+4),
                     5+((2*10+7)*10+4),
                     5+((2*10+8)*10+4),
                     5+((2*10+9)*10+4),
                     5+2*4,
                     5+2/4,
                     5+2+4,
                     5+2-4])) :-
    R = [_,_,_,_,_],
    phrase(expr(E), R),
    R = ['5','+','2',_,'4'].
test(eval, fail) :-
    phrase(expr(_R), ['0','1']).
test(eval, fail) :-
    phrase(expr(_R), ['-','1']).
test(eval, error(existence_error(matching_rule,_))) :-
    eval('abc', _R).

puzzle_solve_all(GuessResults, Answer, PuzzleStrs) :-
    puzzle_solve_all(GuessResults, PuzzleStrs),
    append(GuessResults,
           [Answer-"gggggggg"],
           GuessResultsAll),
    puzzle_solve_all(GuessResultsAll, [Answer]),
    maplist(check_guess_result(Answer), GuessResults).

check_guess_result(Answer, Guess-Result) :-
    assertion(pgr(Answer, Guess, _, Result)).

pgr(Puzzle, Guess, ResultChars0, Result) :-
    string_chars(Puzzle, PuzzleChars),
    string_chars(Guess, GuessChars),
    puzzle_guess_result(PuzzleChars, GuessChars, ResultChars0),
    maplist(grb, ResultChars0, ResultChars),
    string_chars(Result, ResultChars).

grb(緑, g).
grb(紅, r).
grb(黒, b).

test(p1, Ps == ["69-50=19",
                "61-50=11",
                "65-50=15",
                "66-50=16"]) :-
    puzzle_solve_all(["7+8-5=10"-
                      "bbbrrggr",
                      "52-40=12"-
                      "rbgbgggb",
                      "63-50=13"-
                      "gbgggggb"],
                     "69-50=19",
                     Ps).

test(p2, Ps == ["98/7-6=8",
                "98/7-8=6"]) :-
    puzzle_solve_all(["7+8-0=15"-
                      "rbrrbrbb",
                      "23-2*7=9"-
                      "bbrbbrgr",
                      "64/4-9=7"-
                      "rbgbgrgr"],
                     "98/7-6=8",
                     Ps).

test(p3, Ps == ["2-10/5=0"]) :-
    puzzle_solve_all(["8+9-3=14"-
                      "bbbrbrrb",
                      "10-5-3=2"-
                      "rrrrbbgr",
                      "5-20/5=1"-
                      "bgrggggr"],
                     Ps).

test(p4, Ps == ["8-12+9=5",
                "8-15+9=2",
                "9-12+8=5",
                "9-15+8=2",
                "5-11+8=2",
                "5-12+8=1",
                "6-12+8=2",
                "8-11+5=2",
                "8-12+5=1",
                "8-12+6=2"]) :-
    puzzle_solve_all(["7+8-2=13"-
                      "brrrrrrb",
                      "0-4+12=8"-
                      "bgbrrrgr"],
                    % "8-12+9=5"-
                    % "rggggbgb"
                    "6-12+8=2",
                    Ps).
test(p4, Ps == ["6-12+8=2"]) :-
    puzzle_solve_all(["7+8-2=13"-
                      "brrrrrrb",
                      "0-4+12=8"-
                      "bgbrrrgr",
                      "8-12+9=5"-
                      "rggggbgb"],
                     "6-12+8=2",
                     Ps).
% same solution as above, but different starting guess
test(p4, Ps == ["6-12+8=2"]) :-
     puzzle_solve_all(["9/3*6=18"-
                       "bbbbrrrr",
                       "0+14-8=6"-
                       "brgbrggr"],
                      "6-12+8=2",
                      Ps).

test(p5, Ps == ["1/8*72=9",
                "2/4*18=9",
                "2/9*18=4",
                "1/2*18=9",
                "1/9*18=2",
                "4/8*18=9"]) :-
    puzzle_solve_all(["9/3*6=18"-
                      "rgbgbrrr",
                      "0/1*89=0"- % bad guess, because of bug
                      "bgrgrrgb"],
                     "1/8*72=9",
                     Ps).

test(p6, Ps == ["42/7+2=8","48/8+2=8"]) :-
    puzzle_solve_all(["9/3*6=18"-
                      "brbbbrbg",
                      "20/5+4=8"-
                      "rbgbgrgg"],
                     "42/7+2=8",
                     Ps).

test(p7, Ps == ["9*2*5=90","9*5*2=90"]) :-
    puzzle_solve_all(["6/1*9=54"-
                      "bbbgrgrb",
                      "3+9*8=75"-
                      "bbrgbgbr"],
                     "9*2*5=90",
                     Ps).

test(p8, Ps == ["3*46=138","8*81=648","81*8=648"]) :-
    puzzle_solve_all(["6/1*9=54"-
                      "rbrrbrbr",
                      "16-2*4=8"-
                      "rrbbrrrg"],
                     "81*8=648",
                    Ps).
test(p8, Ps == ["61*8=488","81*6=486","81*8=648"]) :-
    puzzle_solve_all(["4*9-1=35"-
                      "rrbbrrbb",
                      "1/4*28=7"-
                      "rbrrbrrb"],
                     "81*8=648",
                    Ps).
test(p8, Ps == ["81*8=648"]) :-
    puzzle_solve_all(["6+9-3=12"-
                      "rbbbbrrb",
                      "1*56/7=8"-
                      "rrbrbbrg"],
                     "81*8=648",
                     Ps).

test(p9, Ps == ["14+49=63","19+44=63"]) :-
    puzzle_solve_all(["6+9-3=12"-
                      "rrrbrgrb",
                      "13+46=59"-
                      "grggrgbr"],
                     "19+44=63",
                     Ps).

test(p10, Ps == ["5*51=255"]) :-
    puzzle_solve_all(["6+9-3=12"-
                      "bbbbbrrr",
                      "1*20/4=5"-
                      "rgrbbbrg"],
                     "5*51=255",
                     Ps).

test(p11, Ps == ["31-4*6=7","36-5*7=1","36-7*5=1","43-7*6=1","31-5*5=6","31-5*6=1"]) :-
    puzzle_solve_all(["6+9-3=12"-
                      "rbbrrrrb",
                      "10-6/3=8"-
                      "rbgrbrgb"],
                     "31-5*6=1",
                     Ps).
test(p11, Ps == ["31-5*6=1"]) :-
     puzzle_solve_all(["9/3*8=24"-
                        "bbrrbrbb",
                        "3*7-15=6"-
                        "grbrrrgr"],
                      "31-5*6=1",
                      Ps).

test(p12, Ps == ["405/9=45"]) :-
     puzzle_solve_all(["3*8/1=24"-
                       "bbbgbgbr",
                       '450/6=75'-  % atom instead of string
                        grrgbgbg],
                      "405/9=45",
                      Ps).

test(p13, Ps == ["97-31=66"]) :-
    puzzle_solve_all(["3*8/1=24"-
                      "rbbbggbb",
                      "19+31=50"-
                      "brbgggbb"],
                     "97-31=66",
                     Ps).

test(p14, Ps == ["3*3*8=72","3*8*3=72","3*8+8=32","3*8-22=2","3*8-2=22"]) :-
    puzzle_solve_all(["3*54=162"-
                      "ggbbrbbg"],
                     "3*8-22=2",
                     Ps).
test(p14, Ps == ["3*8-22=2","5*5-23=2"]) :-
    puzzle_solve_all(["4/3*9=12"-
                       bbrrbrbg,
                      "20-3*6=2"-
                       rbrrrbgg],
                     "3*8-22=2",
                     Ps).

test(p15, Ps == ["30/5+2=8"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "brrbbrbr",
                      "20/5+3=7"-
                      "rggggrgb"],
                     "30/5+2=8",
                     Ps).

test(p16, Ps == ["32+49=81","52+41=93"]) :-
    puzzle_solve_all(['4*9/3=12'-
                      'rbrbrgrr',
                      '12+37=49'-
                      'rggrbgrr'],
                     "32+49=81",
                     Ps).

test(p17, Ps == ["10/4*2=5","0/1*42=0","0/14*2=0","0/41*2=0","14*0/2=0","14*1/2=7","14/4*2=7","41*0/2=0","7/14*2=1"]) :-
    puzzle_solve_all(["6*8/12=4"-
                      "brbrrggr"],
                     "7/14*2=1",
                     Ps).
test(p17, Ps == ["2/14*7=1","7/14*2=1"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "rrbrbrrr",
                      "1/4*20=5"-
                      "rgrrrbgb",
                      "6/12*8=4"-
                      "bggrgbgr"],
                     "7/14*2=1",
                     Ps).
test(p18, Ps == ["53-9*5=8"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "brrbrrbb",
                      "57-6*9=3"-
                      "gbgbgrgr"],
                     "53-9*5=8",
                     Ps).
test(p19, Ps == ["24+73=97","24+23=47"]) :-
    puzzle_solve_all(["7+8-5=10"-
                      "rrbbbgbb",
                      "4*6+3=27"-
                      "rbbrggrg"],
                     "24+23=47",
                     Ps).

test(p20, Ps == ["6*7+0=42","6*7-0=42","7*6+0=42","7*6-0=42","4*9+6=42","5*7+7=42","5*9+7=52","7*5+7=42","9*4+6=42","9*5+7=52","6*6+6=42","7*7-7=42","7*9+9=72","9*7+9=72","9*9-9=72"]) :-
    puzzle_solve_all(["3*8/2=12"-
                      "bgbbbgbg"],
                     "9*9-9=72",
                     Ps).

test(p21, Ps == ["3*6+6=24"]) :-
    puzzle_solve_all(["4*9/3=12"-
                     "rgbbrgbr",
                      "3*4+8=20"-
                     "ggrgbggb",
                      "3*7+4=25"-
                     "ggbgrggb"],
                     "3*6+6=24",
                     Ps).
test(p22, Ps == ["3*7+4=25","3*6+6=24"]) :-
    puzzle_solve_all(["4*9/3=12"-
                     "rgbbrgbr",
                      "3*4+8=20"-
                     "ggrgbggb"],
                     "3*6+6=24",
                     Ps).

test(p23, Ps == ["83-41=42"]) :-
    puzzle_solve_all(["4*9/3=12"-
                     "rbbbrgrg",
                      "14+38=52"-
                     "rrbrrgbg"],
                     "83-41=42",
                     Ps).

test(p23, Ps == ["6+7*8=62","6+8*7=62","6+2*8=22","6+8*2=22","8+2*7=22","8+7*2=22","8+8*8=72"]) :-
    puzzle_solve_all(["4*9/3=12"-
                     "brbbbgbg"],
                     "6+8*2=22",
                    Ps).
test(p23, Ps == ["6+8*2=22"]) :-
    puzzle_solve_all(["4*9/3=12"-
                     "brbbbgbg",
                      "6+7*8=62"-
                     "ggbgrgbg"],
                     "6+8*2=22",
                    Ps).

:- end_tests(nerdle).
