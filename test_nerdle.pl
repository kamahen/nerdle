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

test(p53, Ps = ["414/6=69", "414/9=46", "455/5=91", "497/7=71", "441/9=49"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "gbrgbgrb"],
                     "414/6=69",
                     Ps).

test(p54, Ps = ["38-10=28", "38-18=20"]) :-
    puzzle_solve_all(["1+5*9=46"-
                      "rbbbbgbb",
                      "30/2-8=7"-
                      "grbrrrrb"],
                     "38-10=28",
                     Ps).
test(p54, Ps = ["38-10=28"]) :-
    puzzle_solve_all(["4*9/3=12"-
                      "bbbbrgrr",
                      "6*7+8=50"-
                      "bbbbrgbr"],
                     "38-10=28",
                     Ps).

test(p55, Ps = ["2+7*8=58"]) :-
    puzzle_solve_all(["3*8-4=20"-
                      "brrbbgrb",
                      "6/1+9=15"-
                      "bbbrbgbr"],
                     "2+7*8=58",
                     Ps).
test(p55, Ps = ["2+7*8=58"]) :-
    puzzle_solve_all(["9/3+7=10"-
                      "bbbrrgbb",
                      "8*7-4=52"-
                      "rrgbbggr"],
                     "2+7*8=58",
                     Ps).

test(p56, Ps = ["7-14+8=1"]) :-
    puzzle_solve_all(["3*8/1=24"-bbrbrrbr,
                      "1+56/7=9"-rrbbbrgb],
                     "7-14+8=1",
                     Ps).

test(p57, Ps = ["49-8*6=1"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrrrbb, "1+56/7=9"-rbbrbbgr],
                     "49-8*6=1",
                     Ps).
test(p57, Ps = ["49-8*6=1"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrrrbb, "49-6*8=1"-gggrgrgg],
                     "49-8*6=1",
                     Ps).

test(p58, Ps = ["55/5-3=8", "55/5-8=3"]) :-
    puzzle_solve_all(["12/3+0=4"-bbgrbbgb, "51-6*7=9"-gbrbbbgb],
                     "55/5-3=8",
                     Ps).

test(p59, Ps = ["40/2/5=4", "40/4/2=5", "40/4/5=2", "40/5/2=4"]) :-
    puzzle_solve_all(["5*9-8=37"-rbbbbrbb, "1+20/4=6"-bbrrgrgb],
                     "40/4/5=2",
                     Ps).

:- end_tests(nerdle5).

:- begin_tests(nerdle6).

test(p60, Ps = ["18/2/3=3", "18/3/2=3", "18/3/3=2"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbbrrrb,
                      "1+20/4=6"-gbrbgbgb],
                     "18/3/3=2",
                     Ps).

test(p61, Ps = ["6+2*8=22"]) :-
    puzzle_solve_all(["5*9-8=37"-brbbggbb,
                      "1+20/4=6"-bggbbbrr],
                     "6+2*8=22",
                     Ps).
test(p61, Ps = ["2+8*8=66", "6+2*8=22"]) :-
    puzzle_solve_all(["5*9-8=37"-brbbggbb,
                      "8+4/2=10"-rgbbrgbb],
                     "6+2*8=22",
                     Ps).

test(p62, Ps = ["70-38=32"]) :-
    puzzle_solve_all(["9/3+7=10"-bbrbrgbr,
                      "8*7-4=52"-rbrrbgbg],
                     "70-38=32",
                      Ps).

test(p63, Ps = ["1+10-2=9", "1-2+10=9", "10+1-2=9"]) :-
    puzzle_solve_all(["12/3+9=4"-grbbrrgb,
                      "51-6*7=9"-brrbbbgg],
                     "1+10-2=9",
                     Ps).

test(p64, Ps = ["120/20=6", "120/60=2"]) :-
    puzzle_solve_all(["12/3+0=4"-ggrbbggb],
                     "120/60=2",
                     Ps).

test(p65, Ps = ["24/2-7=5", "45/5-7=2"]) :-
    puzzle_solve_all(["12/3+0=4"-brgbbbgr,
                      "51-6*7=9"-rbrbbggb],
                     "24/2-7=5",
                     Ps).

test(p66, Ps = ["11+48=59", "11+84=95", "14+81=95", "15+84=99", "18+41=59"]) :-
    puzzle_solve_all(["12/3+0=4"-gbbbrbrr,
                      "8*9-5=67"-rbrbrgbb],
                     "18+41=59",
                     Ps).

test(p67, Ps = ["28*1/4=7", "4*7/28=1", "7*4/28=1", "2*14/4=7", "2*7/14=1", "2/4*14=7", "4*7/14=2", "7*2/14=1", "7*4/14=2"]) :-
    puzzle_solve_all(["12/3+0=4"-rrrbbbgr,
                      "51-6*7=9"-brbbrrgb],
                     "2/4*14=7",
                     Ps).

test(p68, Ps = ["5*79=395", "7*57=399", "7*77=539", "7*79=553"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbrbbrb,
                      "8*9-5=67"-bgrbrrbr],
                     "7*77=539",
                     Ps).

test(p69, Ps = ["67-25=42", "67-45=22"]) :-
    puzzle_solve_all(["12/3+0=4"-brbbbbrr,
                      "8*9-5=67"-bbbrggrr],
                     "67-25=42",
                     Ps).

:- end_tests(nerdle6).

:- begin_tests(nerdle7).

test(p70, Ps = ["2*9*3=54"]) :-
    puzzle_solve_all(["5*9-8=37"-rggbbgrb,
                      "8+4/2=10"-bbrbrgbb],
                     "2*9*3=54",
                     Ps).

test(p71, Ps = ["13+12=25", "13+22=35"]) :-
    puzzle_solve_all(["12/3+0=4"-grbrrbrb,
                      "8*9-5=67"-bbbbrgbb],
                     "13+22=35",
                     Ps).

test(p72, Ps = ["6*7-36=6", "7*6-36=6"]) :-
    puzzle_solve_all(["1+20/4=6"-bbbbbbgg,
                      "51-6*7=9"-bbrrrrgb],
                     "6*7-36=6",
                     Ps).

test(p73, Ps = ["5*94=470"]) :-
    puzzle_solve_all(["12/3+0=4"-bbbbbrrr,
                      "8*9-5=67"-bggbrrbr],
                     "5*94=470",
                     Ps).

test(p74, Ps = ["9+3*4=21", "9+4*3=21"]) :-
    puzzle_solve_all(["12/3+0=4"-rrbrrbrr,
                      "8*9-5=67"-brrbbgbb],
                     "9+3*4=21",
                     Ps).

test(p75, Ps = ["48*9=432", "49*8=392", "94*3=282", "98*3=294", "98*4=392"]) :-
    puzzle_solve_all(["3*8-4=20"-rrrbrrrb,
                      "1+56/7=9"-bbbbbbrr],
                     "98*3=294",
                     Ps).

test(p76, Ps = ["79-34=45", "79-44=35"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrggbb,
                      "6/1+9=15"-bbbbrgbg],
                     "79-44=35",
                     Ps).

test(p77, Ps = ["3*71=213", "3*77=231"]) :-
    puzzle_solve_all(["3*8-4=20"-ggbbbrrb,
                      "1+56/7=9"-rbbbbrrb],
                     "3*71=213",
                     Ps).

test(p78, Ps = ["10-6+2=6", "12-6+0=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrrr,
                      "1+56/7=9"-grbgbbgb],
                     "10-6+2=6",
                     Ps).

:- end_tests(nerdle7).
