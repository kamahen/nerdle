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

:- set_test_options([jobs(20)]).

:- begin_tests(eval).

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

:- end_tests(eval).

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

:- begin_tests(nerdle0).

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

test(p6, Ps == ["42/7+2=8", "48/8+2=8"]) :-
    puzzle_solve_all(["9/3*6=18"-
                      "brbbbrbg",
                      "20/5+4=8"-
                      "rbgbgrgg"],
                     "42/7+2=8",
                     Ps).

test(p7, Ps == ["9*2*5=90", "9*5*2=90"]) :-
    puzzle_solve_all(["6/1*9=54"-
                      "bbbgrgrb",
                      "3+9*8=75"-
                      "bbrgbgbr"],
                     "9*2*5=90",
                     Ps).

test(p8, Ps == ["3*46=138", "8*81=648", "81*8=648"]) :-
    puzzle_solve_all(["6/1*9=54"-
                      "rbrrbrbr",
                      "16-2*4=8"-
                      "rrbbrrrg"],
                     "81*8=648",
                    Ps).
test(p8, Ps == ["61*8=488", "81*6=486", "81*8=648"]) :-
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

test(p9, Ps == ["14+49=63", "19+44=63"]) :-
    puzzle_solve_all(["6+9-3=12"-
                      "rrrbrgrb",
                      "13+46=59"-
                      "grggrgbr"],
                     "19+44=63",
                     Ps).

:- end_tests(nerdle0).

:- begin_tests(nerdle1).

test(p10, Ps == ["5*51=255"]) :-
    puzzle_solve_all(["6+9-3=12"-
                      "bbbbbrrr",
                      "1*20/4=5"-
                      "rgrbbbrg"],
                     "5*51=255",
                     Ps).

test(p11, Ps == ["31-4*6=7", "36-5*7=1", "36-7*5=1", "43-7*6=1", "31-5*5=6", "31-5*6=1"]) :-
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

test(p14, Ps == ["3*8+8=32", "3*8-22=2", "3*8-2=22", "3*3*8=72", "3*8*3=72"]) :-
    puzzle_solve_all(["3*54=162"-
                      "ggbbrbbg"],
                     "3*8-22=2",
                     Ps).
test(p14, Ps == ["3*8-22=2", "5*5-23=2"]) :-
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

test(p16, Ps == ["32+49=81", "52+41=93"]) :-
    puzzle_solve_all(['4*9/3=12'-
                      'rbrbrgrr',
                      '12+37=49'-
                      'rggrbgrr'],
                     "32+49=81",
                     Ps).

test(p17, Ps == ["10/4*2=5", "14*1/2=7", "14/4*2=7", "7/14*2=1"]) :-
    puzzle_solve_all(["6*8/12=4"-
                      "brbrrggr"],
                     "7/14*2=1",
                     Ps).
test(p17, Ps == ["2/14*7=1", "7/14*2=1"]) :-
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
test(p19, Ps == ["24+73=97", "24+23=47"]) :-
    puzzle_solve_all(["7+8-5=10"-
                      "rrbbbgbb",
                      "4*6+3=27"-
                      "rbbrggrg"],
                     "24+23=47",
                     Ps).

:- end_tests(nerdle1).

:- begin_tests(nerdle2).

test(p20, Ps == ["6*7+0=42", "6*7-0=42", "7*6+0=42", "7*6-0=42", "4*9+6=42", "5*7+7=42", "5*9+7=52", "7*5+7=42", "9*4+6=42", "9*5+7=52", "6*6+6=42", "7*7-7=42", "7*9+9=72", "9*7+9=72", "9*9-9=72"]) :-
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
test(p22, Ps == ["3*7+4=25", "3*6+6=24"]) :-
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

test(p23, Ps == ["6+7*8=62", "6+8*7=62", "6+2*8=22", "6+8*2=22", "8+2*7=22", "8+7*2=22", "8+8*8=72"]) :-
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

test(p24, Ps == ["47*8=376", "48*7=336"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "grbbrrbb",
                      "43-5*7=8"-
                      "grbbrrrr"],
                      "47*8=376",
                     Ps).

test(p25, Ps == ["15-2-6=7"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbbbbrrr",
                      "0+12-5=7"-
                      "bbrggrgg"],
                      "15-2-6=7",
                     Ps).

test(p26, Ps == ["5*1*4=20"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "rbbgbgrr",
                      "1+4*6=25"-
                      "rbrgbggr"],
                     "5*1*4=20",
                     Ps).

test(p27, Ps == ["3*63=189"]) :-
    puzzle_solve_all(["7+8-3=12"-
                      "bbrbrrrb",
                      "18*3/6=9"-
                      "rrrgbrrg"],
                     "3*63=189",
                     Ps).
test(p27, Ps == ["3*63=189"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbrrrrrb",
                      "3*8-15=9"-
                      "ggrbrbrg"],
                     "3*63=189",
                     Ps).

test(p28, Ps == ["216/3=72"]) :-
     puzzle_solve_all(["7+8-3=12"-
                       "rbbbggrg"],
                      "216/3=72",
                      Ps).
test(p28, Ps == ["160/5=32", "310/5=62", "156/3=52", "186/3=62", "216/3=72", "312/6=52", "132/6=22"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "brrbbgrg"],
                     "216/3=72",
                     Ps).
:- end_tests(nerdle2).

:- begin_tests(nerdle2a).

% TODO: this test is s-l-o-w (was: ~12 seconds; now: ~24 seconds with trivial_term/1)
test(p29, [/*blocked(slow),*/ Ps == ["3-10+9=2", "6-13+9=2", "9+3-10=2", "9+6-13=2", "9-10+3=2", "9-13+6=2", "93+9=102", "99+3=102", "13-2-9=2", "13-9-2=2"]]) :-
    puzzle_solve_all(["4/9*3=12"-
                      "bbrbrrrg"],
                     "3-10+9=2",
                    Ps).

:- end_tests(nerdle2a).

:- begin_tests(nerdle3).

test(p30, Ps == ["53+42=95"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "rbrbrgbr",
                      "23+45=68"-
                      "rgggrgbb"],
                     "53+42=95",
                     Ps).

test(p31, Ps == ["234/78=3"]) :-
    puzzle_solve_all(["7+8-3=12"-
                      "rbrbrrbr",
                      "37*8=296"-
                      "rrbrrrbb"],
                     "234/78=3",
                     Ps).

test(p32, Ps == ["5*7+4=39", "5*9+3=48", "7*5+4=39", "8*4+7=39", "9*4+0=36", "9*4-0=36", "9*4-6=30", "9*5+3=48", "3*9+7=34", "9*4+7=43", "9*9+3=84", "9*4+3=39", "9*4-3=33"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "rbrrrgbb"],
                     "5*9+3=48",
                     Ps).

test(p33, Ps == ["192/64=3"]) :- % This one was done by human
    puzzle_solve_all(["7+8-2=13"-
                      "bbbbrrrg",
                      "6*6/12=3"-
                      "rbbgrrgg",
                      "129/43=3"-
                      "grrgrbgg",
                      "162/54=3"-
                      "grggbggg"],
                     "192/64=3",
                     Ps).
test(p33, Ps == ["192/64=3"]) :- % This one used program's recommendations
    puzzle_solve_all(["7+8-2=13"-
                      "bbbbrrrg",
                      "162/54=3"-
                      "grggbggg"],
                     "192/64=3",
                     Ps).

test(p34, Ps == ["372/93=4"]) :-
    puzzle_solve_all(["6*5/3=10"-
                      "bbbgrrbb",
                      "378/42=9"-
                      "ggbgrrgr"],
                     Ps).

test(p34, Ps == ["2*53=106"]) :-
    puzzle_solve_all(["3*8/1=24"-
                      "rgbbrrrb",
                      "2*53=106"-
                      "gggggggg"],
                     "2*53=106",
                     Ps).

test(p35, Ps == ["28/4-4=3"]) :-
    puzzle_solve_all(["12+3-9=6"-
                      "brbrgbgb",
                      "24/8-9=3"-
                      "grgrgbgg"],
                     "28/4-4=3",
                     Ps).

test(p36, Ps == ["45/9-4=1", "54/9-1=5"]) :-
    puzzle_solve_all(["3*8/1=24"-
                      "bbbrrrbr",
                      "1+40/5=9"-
                      "rbrbrrgr"],
                     "54/9-1=5",
                     Ps).

test(p37, Ps == ["84/7-3=9", "747/83=9"]) :-
     puzzle_solve_all(["4/3*9=12"-
                       "rrrbrrbb",
                       "3+45/9=8"-
                       "rbrbrrgr"],
                      "747/83=9",
                      Ps).

% slow for 1st result:
test(p38, Ps == ["13-9/1=4", "147/49=3"]) :-
    puzzle_solve_all(["4/3*9=12"-rrrbrrrb,
                      "15/3+4=9"-
                      "gbrrbrgr"],
                     "147/49=3",
                     Ps).

test(p39, Ps == ["9+3*2=15"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbggrggr"],
                     "9+3*2=15",
                     Ps).

:- end_tests(nerdle3).

:- begin_tests(nerdle4).

test(p40, Ps == ["20/2-4=6", "24/4+0=6", "24/4-0=6", "24/6+0=4", "24/6-0=4", "24/6-4=0"]) :-
    puzzle_solve_all(["1/4*28=7"-
                      "brrbrbgb",
                      "230/46=5"-
                      "gbrrrrgb"],
                     "20/2-4=6",
                     Ps).

test(p41, Ps == ["92-50=42", "97-55=42", "99-57=42", "94-52=42"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "rbbbrgbg",
                      "98-46=52"-
                      "gbgrbgrg"],
                     "92-50=42",
                     Ps).

test(p42, Ps == ["15*3/9=5"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "brrrrrrb",
                      "1*63/7=9"-
                      "grbggbgr"],
                     "15*3/9=5",
                     Ps).

test(p43, Ps == ["158/2=79", "196/7=28", "118/2=59", "178/2=89", "180/2=90", "189/7=27", "190/2=95", "196/2=98", "712/8=89", "728/8=91", "182/2=91", "192/2=96", "198/2=99"]) :-
     puzzle_solve_all(["4/3*9=12"-
                       "brbbrgrr"],
                      "190/2=95",
                      Ps).

test(p44, Ps == ["4*4+5=21", "4*5+1=21", "5*4+1=21"]) :-
    puzzle_solve_all(["7+8-3=12"-
                      "brbbbgrr",
                      "16+20=45"-
                      "rbrrbgrr"],
                     "4*4+5=21",
                     Ps).

test(p45, Ps == ["207/3=69", "267/3=89", "296/8=37", "392/7=56", "632/8=79", "259/7=37", "270/3=90", "285/3=95", "288/3=96", "232/8=29", "237/3=79", "279/3=93", "297/3=99"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "brrbrgbr"],
                     "207/3=69",
                     Ps).

test(p46, Ps == ["10-8+6=8", "16-8+0=8", "6-8+10=8"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbbbbrrb",
                      "0+15-7=8"-
                      "rrrbrbgg"],
                     "10-8+6=8",
                     Ps).

:- end_tests(nerdle4).

:- begin_tests(nerdle4a). % Slow test run separately.

test(p47, [/*blocked(slow),*/ Ps == ["7+8-13=2", "7-13+8=2", "8+7-13=2", "8-13+7=2", "10-3-5=2", "10-5-3=2", "13-5-6=2", "13-6-5=2", "11-3-6=2", "11-6-3=2", "12-3-7=2", "12-7-3=2", "13-3-8=2", "13-8-3=2"]]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbrbbrrg"],
                     "12-7-3=2",
                     Ps).
test(p47, Ps == ["12-7-3=2"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbrbbrrg",
                      "7+8-13=2"-
                      "rbbrrggg"],
                     "12-7-3=2",
                     Ps).

:- end_tests(nerdle4a).

:- begin_tests(nerdle4b).

test(p48, Ps == ["28+63=91", "68+23=91"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "bbrbrgrr",
                      "12+83=95"-
                      "rrgrgggb"],
                     "68+23=91",
                     Ps).

test(p49, Ps == ["5*34=170", "5*3-11=4", "5*3-14=1", "6*3-14=4", "7*3-14=7", "7*3-17=4"]) :-
    puzzle_solve_all(["4/3*9=12"-
                      "rbgrbrrb"],
                     "5*3-14=1",
                     Ps).
test(p49, Ps == ["3*5-10=5", "3*5-14=1", "3*5-15=0", "5*3-10=5", "5*3-14=1", "5*3-15=0"]) :-
    puzzle_solve_all(["7+8-3=12"-
                      "bbbgrrrb",
                      "6*9-51=3"-
                      "bgbgrrgr"],
                     "5*3-14=1",
                     Ps).

:- end_tests(nerdle4b).

:- begin_tests(nerdle5).

test(p50, Ps == ["12-0-7=5", "12-5-7=0", "12-7-0=5"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "bbbbbrrr",
                      "0+12-5=7"-
                      "rbrrgrgr"],
                     "12-5-7=0",
                     Ps).

test(p51, Ps == ["10+46=56"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "rbbbbgrb",
                      "14+56=70"-
                      "grgrggbr"],
                     "10+46=56",
                     Ps).

test(p52, Ps == ["5*6-8=22", "6*5-8=22", "7*8+6=62", "8*7+6=62", "2*7+8=22", "2*8+6=22", "7*2+8=22", "8*2+6=22", "8*8+8=72", "8*8-2=62", "2*6*6=72", "6*2*6=72", "6*6*2=72"]) :-
    puzzle_solve_all(["4*9/3=12"-bgbbbgbg],
                     "8*8-2=62",
                     Ps).
test(p52, Ps == ["6*8-6=42", "8*8-2=62"]) :-
    puzzle_solve_all(["7+8-3=12"-
                      "bbggbgbg"],
                     "8*8-2=62",
                     Ps).

test(p53, Ps == ["414/6=69", "414/9=46", "455/5=91", "497/7=71", "441/9=49"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "gbrgbgrb"],
                     "414/6=69",
                     Ps).

test(p54, Ps == ["38-10=28", "38-18=20"]) :-
    puzzle_solve_all(["1+5*9=46"-
                      "rbbbbgbb",
                      "30/2-8=7"-
                      "grbrrrrb"],
                     "38-10=28",
                     Ps).
test(p54, Ps == ["38-10=28"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "bbbbrgrr",
                      "6*7+8=50"-
                      "bbbbrgbr"],
                     "38-10=28",
                     Ps).

test(p55, Ps == ["2+7*8=58"]) :-
    puzzle_solve_all(["3*8-4=20"-
                      "brrbbgrb",
                      "6/1+9=15"-
                      "bbbrbgbr"],
                     "2+7*8=58",
                     Ps).
test(p55, Ps == ["2+7*8=58"]) :-
    puzzle_solve_all(["9/3+7=10"-
                      "bbbrrgbb",
                      "8*7-4=52"-
                      "rrgbbggr"],
                     "2+7*8=58",
                     Ps).

test(p56, Ps == ["7-14+8=1"]) :-
    puzzle_solve_all(["3*8/1=24"-bbrbrrbr,
                      "1+56/7=9"-rrbbbrgb],
                     "7-14+8=1",
                     Ps).

test(p57, Ps == ["49-8*6=1"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrrrbb, "1+56/7=9"-rbbrbbgr],
                     "49-8*6=1",
                     Ps).
test(p57, Ps == ["49-8*6=1"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrrrbb, "49-6*8=1"-gggrgrgg],
                     "49-8*6=1",
                     Ps).

test(p58, Ps == ["55/5-3=8", "55/5-8=3"]) :-
    puzzle_solve_all(["12/3+0=4"-bbgrbbgb, "51-6*7=9"-gbrbbbgb],
                     "55/5-3=8",
                     Ps).

test(p59, Ps == ["40/2/5=4", "40/4/2=5", "40/4/5=2", "40/5/2=4"]) :-
    puzzle_solve_all(["5*9-8=37"-rbbbbrbb, "1+20/4=6"-bbrrgrgb],
                     "40/4/5=2",
                     Ps).

:- end_tests(nerdle5).

:- begin_tests(nerdle6).

test(p60, Ps == ["18/2/3=3", "18/3/2=3", "18/3/3=2"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbbrrrb,
                      "1+20/4=6"-gbrbgbgb],
                     "18/3/3=2",
                     Ps).

test(p61, Ps == ["6+2*8=22"]) :-
    puzzle_solve_all(["5*9-8=37"-brbbggbb,
                      "1+20/4=6"-bggbbbrr],
                     "6+2*8=22",
                     Ps).
test(p61, Ps == ["2+8*8=66", "6+2*8=22"]) :-
    puzzle_solve_all(["5*9-8=37"-brbbggbb,
                      "8+4/2=10"-rgbbrgbb],
                     "6+2*8=22",
                     Ps).

test(p62, Ps == ["70-38=32"]) :-
    puzzle_solve_all(["9/3+7=10"-bbrbrgbr,
                      "8*7-4=52"-rbrrbgbg],
                     "70-38=32",
                      Ps).

test(p63, Ps == ["1+10-2=9", "1-2+10=9", "10+1-2=9"]) :-
    puzzle_solve_all(["12/3+9=4"-grbbrrgb,
                      "51-6*7=9"-brrbbbgg],
                     "1+10-2=9",
                     Ps).

test(p64, Ps == ["120/20=6", "120/60=2"]) :-
    puzzle_solve_all(["12/3+0=4"-ggrbbggb],
                     "120/60=2",
                     Ps).

test(p65, Ps == ["24/2-7=5", "45/5-7=2"]) :-
    puzzle_solve_all(["12/3+0=4"-brgbbbgr,
                      "51-6*7=9"-rbrbbggb],
                     "24/2-7=5",
                     Ps).

test(p66, Ps == ["11+48=59", "11+84=95", "14+81=95", "15+84=99", "18+41=59"]) :-
    puzzle_solve_all(["12/3+0=4"-gbbbrbrr,
                      "8*9-5=67"-rbrbrgbb],
                     "18+41=59",
                     Ps).

test(p67, Ps == ["28*1/4=7", "4*7/28=1", "7*4/28=1", "2*14/4=7", "2*7/14=1", "2/4*14=7", "4*7/14=2", "7*2/14=1", "7*4/14=2"]) :-
    puzzle_solve_all(["12/3+0=4"-rrrbbbgr,
                      "51-6*7=9"-brbbrrgb],
                     "2/4*14=7",
                     Ps).

test(p68, Ps == ["5*79=395", "7*57=399", "7*77=539", "7*79=553"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbrbbrb,
                      "8*9-5=67"-bgrbrrbr],
                     "7*77=539",
                     Ps).

test(p69, Ps == ["67-25=42", "67-45=22"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbbbrr,
                      "8*9-5=67"-bbbrggrr],
                     "67-25=42",
                     Ps).

:- end_tests(nerdle6).

:- begin_tests(nerdle7).

test(p70, Ps == ["2*9*3=54"]) :-
    puzzle_solve_all(["5*9-8=37"-rggbbgrb,
                      "8+4/2=10"-bbrbrgbb],
                     "2*9*3=54",
                     Ps).

test(p71, Ps == ["13+12=25", "13+22=35"]) :-
    puzzle_solve_all(["12/3+0=4"-grbrrbrb,
                      "8*9-5=67"-bbbbrgbb],
                     "13+22=35",
                     Ps).

test(p72, Ps == ["6*7-36=6", "7*6-36=6"]) :-
    puzzle_solve_all(["1+20/4=6"-bbbbbbgg,
                      "51-6*7=9"-bbrrrrgb],
                     "6*7-36=6",
                     Ps).

test(p73, Ps == ["5*94=470"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbbbrrr,
                      "8*9-5=67"-bggbrrbr],
                     "5*94=470",
                     Ps).

test(p74, Ps == ["9+3*4=21", "9+4*3=21"]) :-
    puzzle_solve_all(["12/3+0=4"-rrbrrbrr,
                      "8*9-5=67"-brrbbgbb],
                     "9+3*4=21",
                     Ps).

test(p75, Ps == ["48*9=432", "49*8=392", "94*3=282", "98*3=294", "98*4=392"]) :-
    puzzle_solve_all(["3*8-4=20"-rrrbrrrb,
                      "1+56/7=9"-bbbbbbrr],
                     "98*3=294",
                     Ps).

test(p76, Ps == ["79-34=45", "79-44=35"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrggbb,
                      "6/1+9=15"-bbbbrgbg],
                     "79-44=35",
                     Ps).

test(p77, Ps == ["3*71=213", "3*77=231"]) :-
    puzzle_solve_all(["3*8-4=20"-ggbbbrrb,
                      "1+56/7=9"-rbbbbrrb],
                     "3*71=213",
                     Ps).

test(p78, Ps == ["10-6+2=6", "12-6+0=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrrr,
                      "1+56/7=9"-grbgbbgb],
                     "10-6+2=6",
                     Ps).

test(p79, Ps == ["340/68=5","360/45=8","430/86=5"]) :-
    puzzle_solve_all(["12/3+0=4"-bbrrbrgr,
                      "51-6*7=9"-rbbrbbgb],
                     "340/68=5",
                     Ps).

:- end_tests(nerdle7).

:- begin_tests(nerdle8).

test(p80, Ps == ["306/9=34"]) :-
    puzzle_solve_all(["3*8-4=20"-gbbbrgbr,
                      "6/1+9=15"-rrbbggbb],
                     "306/9=34",
                     Ps).
test(p80, Ps == ["306/9=34","396/9=44"]) :-
    puzzle_solve_all(["9/3*7=21"-rrrbbgbb,
                      "356/4=89"-gbggrgbr],
                     "306/9=34",
                     Ps).
test(p80, Ps == ["356/4=89","380/4=95","534/6=89","306/9=34","345/5=69","354/6=59","360/4=90","360/9=40","384/4=96","390/6=65","465/5=93","498/6=83","558/6=93","396/4=99","396/9=44","396/6=66"]) :-
    puzzle_solve_all(["9/3*7=21"-rrrbbgbb],
                    "306/9=34",
                     Ps).

test(p81, Ps == ["5-11+9=3","5-13+9=1","9-11+5=3","9-13+5=1","9-13+9=5","9-15+9=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrbrbb,
                      "1+56/7=9"-rrrbbbgr],
                     "9-15+9=3",
                     Ps).
test(p82, Ps == ["4+9+9=22","9+9+4=22"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbrbrr,
                      "8*9-5=67"-bbgbbgbb],
                     "4+9+9=22",
                     Ps).

test(p83, Ps == ["12*2/3=8","12*2/8=3"]) :-
    puzzle_solve_all(["12/3+0=4"-ggrrbbgb,
                      "51-6*7=9"-brbbrbgb],
                     "12*2/3=8",
                     Ps).

test(p84, Ps == ["68-20=48","68-28=40","68-40=28","68-48=20"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbbrrr,
                      "8*9-5=67"-rbbrbgrb],
                     "68-28=40",
                     Ps).

test(p85, Ps == ["9+7/7=10"]) :-
    puzzle_solve_all(["3*8-4=20"-
                       bbbbbgbg,
                      "6/1+9=15"-
                       brbrrggb],
                     "9+7/7=10",
                     Ps).

test(p86, Ps == ["5-3+9=11"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrbgbb,
                      "6/1+9=15"-bbrggggr],
                     "5-3+9=11",
                     Ps).

test(p89, Ps == ["12-9+6=9","16+2-9=9","16-9+2=9"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrrb,
                      "1+56/7=9"-grbrbbgg],
                     "16-9+2=9",
                     Ps).

:- end_tests(nerdle8).

:- begin_tests(nerdle9).

test(90, Ps == ["5+16/4=9"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrrbb,
                      "1+56/7=9"-rgrggbgg],
                     "5+16/4=9",
                     Ps).

p(91, Ps == ["10+48=58","18+40=58","40+18=58","48+10=58"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrbrgbr,
                      "6/1+9=15"-bbrrbgbr],
                     "40+18=58",
                     Ps).

p(92, Ps == ["11-1-4=6","11-1-6=4","11-4-1=6","11-4-6=1","14-4-4=6","14-4-6=4","16-4-6=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrrrbb,
                      "1+56/7=9"-gbbrbbgb],
                     "11-1-4=6",
                     Ps).

p(93, Ps == ["49+48=97"]) :-
    nerdle:puzzle_solve_all(["3*8-4=20"-bbrbrgbb,
                             "6/1+9=15"-bbbrrgbb],
                            "49+48=97",
                            Ps).

p(94, Ps == ["8*6-39=9","8*9-63=9"]) :-
    puzzle_solve_all(["3*8-4=20"-rgrgbrbb,
                      "1+56/7=9"-bbbrbbgg],
                     "8*9-63=9",
                     Ps).

p(95, Ps == ["7-12/6=5"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrrb,
                      "1+56/7=9"-rbrrgrgb],
                     "7-12/6=5",
                     Ps).

p(96, Ps == ["2*5*7=70","2*7*5=70","5*2*7=70","5*7*2=70","7*2*5=70","7*5*2=70","2*5*5=50","5*2*5=50","5*5*2=50"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbbbgrg,
                      "6/1+9=15"-bbbbbgbr],
                     "2*5*7=70",
                     Ps).

p(97, Ps == ["89-19=70","99-19=80"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrbgbg,
                      "6/1+9=15"-bbrbggbb],
                     "99-19=80",
                     Ps).

p(98, Ps == ["44-6*6=8"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrrrbb,
                      "1+56/7=9"-bbbgbbgb],
                     "44-6*6=8",
                     Ps).

p(99, Ps == ["13-1-7=5","13-5-7=1","15-3-7=5","15-5-7=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrbrbb,
                      "1+56/7=9"-gbrbbggb],
                     "13-5-7=1",
                     Ps).

:- end_tests(nerdle9).

:- begin_tests(nerdle10).

p(100, Ps == ["89-61=28","89-68=21"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrbggb,
                      "6/1+9=15"-rbrbrgbb],
                     "89-61=28",
                     Ps).

test(101, Ps == ["192/6=32"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbbbgrb,
                      "6/1+9=15"-rrrbrgbb],
                     "192/6=32",
                     Ps).

test(102, Ps == ["30-3*9=3","30-9*3=3"]) :-
    puzzle_solve_all(["3*8-4=20"-grbrbrbr,
                      "1+56/7=9"-bbbbbbgr],
                     "30-3*9=3",
                     Ps).

test(103, Ps == ["2+6+8=16","8+2+6=16","8+6+2=16"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrbbgrb,
                      "6/1+9=15"-rbbgbggb],
                     "2+6+8=16",
                     Ps).

test(104, Ps == ["62+14=76"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbggrb,
                      "6/1+9=15"-gbrrbgbb],
                     "62+14=76",
                     Ps).

test(105, Ps == ["55-20=35"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrbgrr,
                      "6/1+9=15"-bbbbbgbg],
                     "55-20=35",
                     Ps).

test(106, Ps == ["9+5-14=0"]) :-
    puzzle_solve_all(["5*9-8=37"-rbrgbrbb,
                      "1+20/4=6"-rgbrbggb],
                     "9+5-14=0",
                     Ps).

test(107, Ps == ["7+8-3=12"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbgrgrr,
                      "8+4/2=10"-rgbbrggb],
                     "7+8-3=12",
                     Ps).

test(108, Ps == ["21/3-7=0","21/7-0=3","21/7-3=0","3-21/7=0","7-21/3=0","20/2-7=3","7-12/3=3","27/3-7=2"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbrbrrr,
                      "52-8*6=4"-brrbbbgb],
                     "7-12/3=3",
                     Ps).

test(109, Ps == ["20+68=88","60+28=88"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbbggbb,
                      "8+4/2=10"-rrbbrgbr],
                     "60+28=88",
                     Ps).

:- end_tests(nerdle10).

:- begin_tests(nerdle11).

test(110, Ps == ["42/2/3=7","42/7/2=3","42/7/3=2"]) :-
    puzzle_solve_all(["12/3+0=4"-bggrbbgr,
                      "51-6*7=9"-bbbbbrgb],
                     "42/2/3=7",
                    Ps).

test(111, Ps == ["6*7-38=4","7*6-38=4"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbrbbgg,
                      "51-6*7=9"-bbrrrrgb],
                      "6*7-38=4",
                     Ps).

test(112, Ps == ["15+18=33"]) :-
    puzzle_solve_all(["12/3+0=4"-gbbrrbrb,
                      "8*9-5=67"-rbbbrgbb],
                     "15+18=33",
                     Ps).

test(113, Ps == ["4-24/8=1","8-14/2=1","8-28/4=1","4-12/4=1"]) :-
    puzzle_solve_all(["12/3+0=4"-rrrbbbgr,
                      "51-6*7=9"-brrbbbgb],
                     "8-28/4=1",
                     Ps).

test(114, Ps == ["5*8/1=40","5*8/4=10"]) :-
    puzzle_solve_all(["12/3+0=4"-rbrbbrrr,
                      "8*9-5=67"-rgbbrgbb],
                     "5*8/1=40",
                     Ps).

test(115, Ps == ["70/7-9=1"]) :-
    puzzle_solve_all(["12/3+0=4"-rbgbbrgb,
                      "51-6*7=9"-brrbbrgr],
                     "70/7-9=1",
                     Ps).

test(116, Ps == ["25-2*8=9","25-8*2=9"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbbbgb,
                      "5*7-26=9"-rrbrrbgg],
                     "25-2*8=9",
                     Ps).

test(117, Ps == ["20+65=85","25+60=85","60+25=85","65+20=85","20+60=80","20+66=86","20+68=88","25+55=80","26+60=86","28+22=50","28+52=80","28+60=88","55+25=80","58+22=80","60+20=80","60+26=86","60+28=88","66+20=86","68+20=88"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbrrrb,
                      "0+8*9=72"-rrrbbgbr],
                     "20+65=85",
                    Ps).

test(118, Ps == ["13-0-5=8","13-0-8=5","13-5-8=0","13-8-5=0"]) :-
    puzzle_solve_all(["12/3+0=4"-gbbrbrgb,
                      "51-6*7=9"-rrgbbbgb],
                     "13-5-8=0",
                     Ps).

test(119, Ps == ["6+8-14=0","8+6-14=0","4+7-11=0","7+4-11=0","7+7-14=0"]) :-
    puzzle_solve_all(["12/3+0=4"-rbbbrrgr,
                      "0+14-9=5"-rgrrrbgb],
                     "6+8-14=0",
                    Ps).

:- end_tests(nerdle11).

:- begin_tests(nerdle12).

test(120, Ps == ["10+24=34","14+20=34"]) :-
    puzzle_solve_all(["12/3+0=4"-grbrrrrg,
                      "8*9-5=67"-bbbbbgbb],
                     "10+24=34",
                     Ps).

test(121, Ps == ["1-7+15=9"]) :-
    puzzle_solve_all(["12/3+0=4"-gbbbrbgb,
                      "51-6*7=9"-rrrbbrgg],
                     "1-7+15=9",
                     Ps).

test(122, Ps == ["6+7*7=55","7+7*7=56"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbbrbrb,
                      "8*9-5=67"-brbbrgrr],
                     "6+7*7=55",
                     Ps).

test(123, Ps == ["33+63=96","33+66=99","36+63=99"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbrrbrb,
                      "8*9-5=67"-bbrbbgrb],
                     "33+63=96",
                     Ps).

test(124, Ps == ["12/6*4=8","12/8*4=6","12/1-4=8","12/4*2=6","12/2-4=2","12/4-1=2","12/4-2=1"]) :-
    puzzle_solve_all(["12/3+0=4"-gggbbbgr],
                     "12/4-2=1",
                     Ps).
test(124, Ps == ["12/4-1=2","12/4-2=1"]) :-
    puzzle_solve_all(["12/3+0=4"-gggbbbgr,
                      "12/6*4=8"-gggbbrgb],
                     "12/4-2=1",
                     Ps).

test(125, Ps == ["2-3+10=9","2-9+10=3","3+9-10=2","9+3-10=2"]) :-
    puzzle_solve_all(["12/3+0=4"-rrbrrggb],
                     "2-3+10=9",
                     Ps).

test(126, Ps == ["90-20=70","90-70=20","97-27=70","97-77=20","99-20=79","99-29=70","99-70=29","99-79=20"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbbrrb,
                      "8*9-5=67"-bbrrbgbr],
                     "99-70=29",
                     Ps).

test(127, Ps == ["9+9*7=72"]) :-
    puzzle_solve_all(["3*8-4=20"-brbbbgrb,
                      "6/1+9=15"-bbbrrgbb],
                     "9+9*7=72",
                     Ps).

test(128, Ps == ["19-6*3=1"]) :-
    puzzle_solve_all(["3*8-4=20"-rrbrbrbb,
                      "1+56/7=9"-gbbgbbgr],
                     "19-6*3=1",
                     Ps).

test(129, Ps == ["540/90=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrrbr,
                      "1+56/7=9"-bbrrrbgr],
                     "540/90=6",
                     Ps).

:- end_tests(nerdle12).

:- begin_tests(nerdle13).

test(130, Ps == ["89-9*9=8"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrbrbb,
                      "1+56/7=9"-bbbbbbgr],
                     "89-9*9=8",
                     Ps).

test(131, Ps == ["6*7-34=8","6*7-38=4","7*6-34=8","7*6-38=4"]) :-
    puzzle_solve_all(["3*8-4=20"-rgrgrrbb,
                      "1+56/7=9"-bbbrbrgb],
                     "6*7-34=8",
                     Ps).

test(132, Ps == ["59-32=27","59-37=22","95-72=23","95-73=22","52-23=29"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrbggb,
                      "6/1+9=15"-bbbbrgbr],
                     "59-37=22",
                     Ps).

test(133, Ps == ["36/9-1=3","36/9-3=1"]) :-
    puzzle_solve_all(["3*8-4=20"-gbbrbrbb,
                      "1+56/7=9"-rbbrrbgr],
                     "36/9-1=3",
                     Ps).

test(134, Ps == ["9*55=495"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbbrrbb,
                      "1+56/7=9"-bbgbbbrr],
                     "9*55=495",
                      Ps).

test(135, Ps == ["94*7=658"]) :-
    puzzle_solve_all(["3*8-4=20"-brrbrrbb,
                      "51-6*7=9"-rbbrrrrr],
                     "94*7=658",
                     Ps).

test(136, Ps == ["21/1/3=7","21/3/1=7","21/7/1=3","21/7/3=1","213/71=3","217/31=7"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbbbrrb,
                      "51-6*7=9"-bgbbbrgb],
                     "213/71=3",
                     Ps).

test(137, Ps == ["6*63=378"]) :-
    puzzle_solve_all(["3*8-4=20"-rgrbbrbb,
                      "51-6*7=9"-bbbrrrrb],
                     "6*63=378",
                     Ps).

test(138, Ps == ["1*5+6=11","1*6+5=11"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbbbgbb,
                      "6/1+9=15"-rbrgbggr],
                     "1*6+5=11",
                     Ps).

test(139, Ps == ["0+18/3=6","0+18/6=3","18/3+0=6","180/30=6","180/60=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrbbrbr,
                      "51-6*7=9"-brbrbbgb],
                     "180/30=6",
                     Ps).


:- end_tests(nerdle13).

:- begin_tests(nerdle14).


test(140, Ps == ["11+49=60","41+19=60"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrgbg,
                      "6/1+9=15"-rbrrggrb],
                     "11+49=60",
                     Ps).
test(140, Ps == ["11+49=60","19+49=68"]) :-
    puzzle_solve_all(["12+57=69"-gbgbbggr,
                      "14+49=63"-gbgggggb],
                     "11+49=60",
                     Ps).

test(141, Ps == ["5*4/10=2","5*4/20=1"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbbrrrr,
                      "1+56/7=9"-rbrbrbgb],
                     "5*4/20=1",
                     Ps).

test(142, Ps == ["1/4*24=6","1/6*24=4","116/29=4","144/24=6"]) :-
    puzzle_solve_all(["12+35=47"-grbbbrrb,
                      "10-4/2=8"-gbbrrrgb],
                     "1/4*24=6",
                     Ps).

test(143, Ps == ["89-15=74","98-41=57","98-47=51","98-51=47","98-57=41","99-15=84","99-41=58","99-48=51","99-51=48","99-58=41"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrrgbb,
                      "6/1+9=15"-bbrbrgbr],
                     "99-41=58",
                     Ps).
test(143, Ps == ["99-41=58","99-48=51","99-51=48","99-58=41"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrrgbb,
                      "6/1+9=15"-bbrbrgbr,
                      "89-15=74"-rggrrgbr],
                     "99-41=58",
                     Ps).
test(143, Ps == ["91-40=51","91-41=50","95-11=84","95-14=81","99-41=58","99-48=51","95-41=54","95-44=51"]) :-
    puzzle_solve_all(["12+35=47"-rbbbrgrb,
                      "4*6-9=15"-rbbrrgrr],
                     "99-41=58",
                     Ps).
test(143, Ps == ["99-41=58","95-41=54"]) :-
    puzzle_solve_all(["12+35=47"-rbbbrgrb,
                      "4*6-9=15"-rbbrrgrr,
                      "91-40=51"-grggbggb],
                     "99-41=58",
                     Ps).

test(144, Ps == ["4+9-1=12","4+9-2=11","9+4-1=12","9+4-2=11"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbgrgrb,
                      "6/1+9=15"-bbrrrggb],
                     "4+9-1=12",
                    Ps).

test(145, Ps == ["11+5-7=9","15+1-7=9"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrbb,
                      "1+56/7=9"-grrbbggg],
                     "11+5-7=9",
                     Ps).

test(146, Ps == ["7+3*5=22","7+5*3=22"]) :-
    puzzle_solve_all(["3*8-4=20"-rrbbbggb,
                      "6/1+9=15"-bbbrbgbr],
                     "7+3*5=22",
                     Ps).

test(147, Ps == ["11*1-7=4","11-1*7=4","14*1-7=7","14-1*7=7"]) :-
    puzzle_solve_all(["3*8-4=20"-brbrrrbb,
                      "1+56/7=9"-gbbbbggb],
                     "14-1*7=7",
                     Ps).

test(148, Ps == ["2*7+5=19","7*2+5=19"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbbbgrb,
                      "6/1+9=15"-bbbgrggr],
                     "2*7+5=19",
                     Ps).

test(149, Ps == ["3*10/5=6","3*10/6=5"]) :-
    puzzle_solve_all(["3*8-4=20"-ggbbbrbr,
                      "1+56/7=9"-rbrrgbgb],
                     "3*10/5=6",
                     Ps).

:- end_tests(nerdle14).


:- begin_tests(nerdle15).

test(150, Ps == ["11-1*8=3","11-8*1=3","19-8-8=3"]) :-
     puzzle_solve_all(["12+35=47"-gbbrbrbb,
                       "10-6/3=8"-gbgbbrgr],
                      "11-1*8=3",
                      Ps).

test(151, Ps == ["7+3*9=34","7+4*9=43"]) :-
    puzzle_solve_all(["3*8-4=20"-rrbbrgbb,
                      "6/1+9=15"-bbbrggbb],
                     "7+3*9=34",
                     Ps).

test(152, Ps == ["10-2*4=2","10-4*2=2"]) :-
    puzzle_solve_all(["3*8-4=20"-brbrrrrr,
                      "1+56/7=9"-gbbbbbgb],
                     "10-2*4=2",
                     Ps).

test(153, Ps == ["17-3-6=8","17-3-8=6","17-8-3=6","17-8-6=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrrbrbb,
                      "1+56/7=9"-gbbrbrgb],
                     "17-8-6=3",
                     Ps).

test(154, Ps == ["89-13=76","89-16=73","98-31=67","98-37=61","98-61=37","98-67=31","96-13=83","99-13=86","99-16=83","99-31=68","99-38=61","99-61=38","99-68=31"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrrbgbb,
                      "6/1+9=15"-rbrbrgbb],
                     "99-31=68",
                     Ps).
test(154, Ps == ["99-31=68","99-38=61","99-61=38","99-68=31"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrrbgbb,
                      "6/1+9=15"-rbrbrgbb,
                      "89-13=76"-rggrrgbr],
                     "99-31=68",
                     Ps).

test(155, Ps == ["3+11-6=8","3+11-8=6","3-16/8=1","3-18/9=1","3-6+11=8","39/3-5=8","39/3-8=5","3+13-8=8","33/3-3=8","33/3-8=3"]) :-
    puzzle_solve_all(["3*8-20=4"-gbrrbbgb],
                     "3+13-8=8",
                     Ps).

test(156, Ps == ["5*5-21=4","5*5-24=1"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbgrrrb,
                      "1+56/7=9"-rbgbbbgb],
                     "5*5-21=4",
                     Ps).

test(157, Ps == ["8*3-16=8","8*3-18=6"]) :-
    puzzle_solve_all(["3*8-4=20"-rgrgbrbb,
                      "1+56/7=9"-rbbrbbgb],
                     "8*3-16=8",
                     Ps).

test(158, Ps == ["10+72=82","12+70=82","70+12=82","72+10=82"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrbbgrr,
                      "6/1+9=15"-bbrrbgbb],
                     "10+72=82",
                     Ps).

test(159, Ps == ["2+5+7=14","2+7+5=14","5+2+7=14","5+7+2=14","7+2+5=14","7+5+2=14"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrgrb,
                      "6/1+9=15"-bbbgbggr],
                     "2+5+7=14",
                    Ps).

:- end_tests(nerdle15).

:- begin_tests(nerdle16).

test(160, Ps == ["6*7+6=48","6*6+8=44"]) :-
    puzzle_solve_all(["3*8-4=20"-bgrbrgbb,
                      "6/1+9=15"-gbbgbgbb],
                     "6*6+8=44",
                     Ps).

test(161, Ps == ["55-17=38","55-18=37","73-15=58","88-15=73","88-31=57","88-37=51","88-51=37","88-57=31","53-15=38"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrrbgbb,
                      "6/1+9=15"-bbrbbgbr],
                     "88-31=57",
                     Ps).

test(162, Ps == ["27*9=243","73*4=292","93*4=372"]) :-
    puzzle_solve_all(["3*8-4=20"-rrbbrrrb,
                      "1+56/7=9"-bbbbbrrr],
                     "73*4=292",
                     Ps).

test(163, Ps == ["89-41=48","89-48=41"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrrgbb,
                      "6/1+9=15"-bbrbrgbb],
                     "89-41=48",
                     Ps).

test(164, Ps == ["12/6+7=9","21/7+6=9","16/2+1=9","72/9+1=9"]) :-
    puzzle_solve_all(["4*9/3=12"-bbrrbrrr,
                      "0+18/2=9"-brrbrrgg],
                     "16/2+1=9",
                     Ps).

test(165, Ps == ["26+49=75","29+46=75","46+29=75","49+26=75","22+45=67","22+47=69","22+54=76","22+74=96","27+49=76","27+67=94","29+47=76","42+25=67","42+27=69","47+29=76","49+27=76","52+24=76"]) :-
    puzzle_solve_all(["7+8-2=13"-rrbbrgbb,
                      "6*7+0=42"-rbrrbgrr],
                     "22+47=69",
                     Ps).

test(165, Ps == ["9+3+4=16","9+4+3=16"]) :-
    puzzle_solve_all(["3*8/1=24"-rbbbrgbr,
                      "4+9-0=13"-rgrbbggr],
                     "9+3+4=16",
                     Ps).

test(166, Ps == ["67-41=26","67-46=21"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrrggb,
                      "6/1+9=15"-gbrbbgbb],
                     "67-41=26",
                     Ps).
test(167, Ps == ["3+14/7=5"]) :-
    puzzle_solve_all(["12+35=47"-rbrrrrrr],
                     "3+14/7=5",
                     Ps).
test(167, Ps == ["3+14/7=5"]) :-
    puzzle_solve_all(["3*8-4=20"-gbbbrrbb,
                      "1+56/7=9"-rgrbgggb],
                     "3+14/7=5",
                     Ps).

test(168, Ps == ["48/2/4=6","48/2/6=4","48/4/2=6","48/4/6=2","64/2/4=8","64/2/8=4","64/4/2=8","64/4/8=2","64/8/2=4","64/8/4=2"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrbrrrb,
                      "1+56/7=9"-bbbrgbgb],
                     "48/2/6=4",
                     Ps).

test(168, Ps == ["11+28=39","11+82=93","12+81=93","18+21=39","21+18=39","28+11=39","81+12=93","82+11=93"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrbbgrb,
                      "6/1+9=15"-bbrrrgrb],
                     "18+21=39",
                     Ps).

test(169, Ps == ["14-3-4=7","14-4-3=7","14-7-3=4","14-7-4=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrrrbb,
                      "1+56/7=9"-gbbbbrgb],
                     "14-3-4=7",
                     Ps).

:- end_tests(nerdle16).

:- begin_tests(nerdle17).

test(170, Ps == ["7*9-6=57","7*9-7=56","9*7-6=57","9*7-7=56","9*9-5=76"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbgbgbb,"6/1+9=15"-rbbbrgbr],
                     "9*7-7=56",
                     Ps).

test(171, Ps == ["4+5+7=16","4+7+5=16","5+4+7=16","7+4+5=16"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrgbb,
                      "6/1+9=15"-rbbgbggr],
                     "4+5+7=16",
                     Ps).

test(172, Ps == ["0+14-6=8","0+14-8=6","4+10-6=8","4+10-8=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrrrbr,
                      "1+56/7=9"-rgbrbbgb],
                     "4+10-8=6",
                     Ps).

test(173, Ps == ["1+2+7=10","1+7+2=10","2+7+1=10","7+2+1=10"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbbgrg,
                      "6/1+9=15"-bbrgbggb],
                     "1+2+7=10",
                     Ps).

test(174, Ps == ["72-16=56","76-25=51","77-15=62","77-21=56","77-26=51"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbgrb,
                      "6/1+9=15"-rbrbbgbr],
                     "76-25=51",
                     Ps).

test(175, Ps == ["3*4+9=21","3*9+1=28","3*6+3=21","9*2+3=21"]) :-
    puzzle_solve_all(["2+3*5=17"-rrrrbgrb],
                     "3*4+9=21",
                     Ps).

test(177, Ps == ["24+11=35","25+10=35","25+11=36","22+13=35","23+12=35"]) :-
    puzzle_solve_all(["2+3*5=17"-grrbrgrb,
                      "21+38=59"-grgrbgrb],
                     "25+10=35",
                     Ps).
test(177, Ps == ["25+10=35","22+13=35","23+12=35"]) :-
    puzzle_solve_all(["2+3*5=17"-grrbrgrb,
                      "21+38=59"-grgrbgrb,
                      "24+11=35"-gbggbggg],
                     "25+10=35",
                     Ps).

test(178, Ps == ["228/57=4","270/54=5","285/57=5","275/55=5"]) :-
    puzzle_solve_all(["2+3*5=17"-gbbbgrbr],
                     "275/55=5",
                     Ps).

test(179, Ps == ["6*2+9=21","9*9+1=82"]) :-
    puzzle_solve_all(["2+3*5=17"-rrbrbgrb],
                     "9*9+1=82",
                     Ps).

:- end_tests(nerdle17).

:- begin_tests(nerdle18).

test(180, Ps == ["4+8-11=1","8+4-11=1"]) :-
    puzzle_solve_all(["2+3*5=17"-bgbbbrrb,
                      "0+14-6=8"-bgrrrbgr],
                     "4+8-11=1",
                     Ps).

test(181, Ps == ["0+13-6=7","3+10-6=7","3+16/4=7","0+10-3=7","1+18/3=7","3+13-9=7","1+13-7=7","3+11-7=7"]) :-
    puzzle_solve_all(["2+3*5=17"-bgrbbrrg],
                     "3+11-7=7",
                     Ps).

% The following test resulted from a typo, which resulted in
% inconsistent constraints - constrain_from_guess/3 fails and, because
% it's marked as "det", a determinism_error is thrown, which is rather
% confusing.

% 2+3*5=17
% bgbbrggb

% 5+9-0=14
% rgrbbggb

% 9+6/1=15
% rgbbrggg

% 1+5+9=15
% gggggggg

% puzzle_solve_all(["2+3*5=17"-bgbbrggb, "5+9-0=14"-rgrbbggb, "9+6/1=15"-rgbbrggg], Z).

test(182, [blocked(inconsistent_constraints), Ps == ["9+6/1=15","6+5+8=19","8+5+6=19","9+0+6=15","9+6+0=15","9+5+1=15"]]) :-
    puzzle_solve_all(["2+3*5=17"-bgbbrggb, "5+9-9=14"-rgrbbggb],
                     "1+5+9=15",
                     Ps).
test(182, Ps == ["9+6/1=15","6+5+8=19","8+5+6=19","1+5+9=15","9+5+1=15"]) :-
    puzzle_solve_all(["2+3*5=17"-bgbbrggb,
                      "5+9-0=14"-rgrbbggb],
                     "1+5+9=15",
                     Ps).

test(183, Ps == ["1+6*7=43","1+7*6=43","1+8*9=73","1+9*8=73","7+4*6=31","7+6*4=31","3+4*7=31","3+7*4=31","7+8*3=31"]) :-
    puzzle_solve_all(["2+3*5=17"-bgrgbgrr],
                     "7+4*6=31",
                     Ps).

test(184, Ps == ["392/49=8","332/83=4"]) :-
    puzzle_solve_all(["2+3*5=17"-rbrbbrbb,
                      "3-24/8=0"-gbgrrrgb],
                     "392/49=8",
                     Ps).

:- end_tests(nerdle18).
