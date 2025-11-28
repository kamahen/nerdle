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

:- current_prolog_flag(cpu_count, Cores),
    set_test_options([jobs(Cores), load(always)]).

:- begin_tests(eval).

test(eval, all(T-R == [0-0])) :-
    phrase(expr(T), ['0']),
    eval(T, R).
test(eval, all(T-R == [1-1])) :-
    phrase(expr(T), ['1']),
    eval(T, R).
test(eval, all(T-R == [123-123])) :-
    phrase(expr(T), ['1','2','3']),
    eval(T, R).
test(eval, all(T-R == [(4/8*18+5)-14])) :-
     phrase(expr(T), ['4','/','8','*','1','8','+','5']),
     eval(T, R).
test(eval, all(E == [5+204,
                     5+214,
                     5+224,
                     5+234,
                     5+244,
                     5+254,
                     5+264,
                     5+274,
                     5+284,
                     5+294,
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
test(eval, all(Str == [['1',+,'2',*,'3']])) :-
    between(1,6,L),
    length(Str, L),
    E = 1+2*3,
    phrase(expr(E), Str).

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
    % atom_chars(ResultAtom, ResultChars),
    % assertion(puzzle_play(Puzzle, Guess, ResultAtom)),
    string_chars(Result, ResultChars).

grb(緑, g).
grb(紅, r).
grb(黒, b).

:- begin_tests(nerdle0).

test(p1, Ps == ["69-50=19", "61-50=11", "65-50=15", "66-50=16"]) :-
    puzzle_solve_all(["7+8-5=10"-"bbbrrggr",
                      "52-40=12"-"rbgbgggb",
                      "63-50=13"-"gbgggggb"],
                     "69-50=19",
                     Ps).

test(p2, Ps == ["98/7-6=8",
                "98/7-8=6"]) :-
    puzzle_solve_all(["7+8-0=15"-"rbrrbrbb",
                      "23-2*7=9"-"bbrbbrgr",
                      "64/4-9=7"-"rbgbgrgr"],
                     "98/7-6=8",
                     Ps).

test(p3, Ps == ["2-10/5=0"]) :-
    puzzle_solve_all(["8+9-3=14"-"bbbrbrrb",
                      "10-5-3=2"-"rrrrbbgr",
                      "5-20/5=1"-"bgrggggr"],
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
    puzzle_solve_all(["7+8-2=13"-"brrrrrrb",
                      "0-4+12=8"-"bgbrrrgr"],
                    % "8-12+9=5"-"rggggbgb"
                    "6-12+8=2",
                    Ps).
test(p4, Ps == ["6-12+8=2"]) :-
    puzzle_solve_all(["7+8-2=13"-"brrrrrrb",
                      "0-4+12=8"-"bgbrrrgr",
                      "8-12+9=5"-"rggggbgb"],
                     "6-12+8=2",
                     Ps).
% same solution as above, but different starting guess
test(p4, Ps == ["6-12+8=2"]) :-
     puzzle_solve_all(["9/3*6=18"-"bbbbrrrr",
                       "0+14-8=6"-"brgbrggr"],
                      "6-12+8=2",
                      Ps).

test(p5, error(nerdle_assertion(fail,nerdle:verify_guess(['0',/,'1',*,'8','9',=,'0'])))) :-  % Ps == ["1/8*72=9","2/4*18=9","2/9*18=4","1/2*18=9","1/9*18=2","4/8*18=9"]) :-
    puzzle_solve_all(["9/3*6=18"-"rgbgbrrr",
                      "0/1*89=0"- % bad guess, because of bug
                      "bgrgrrgb"],
                     "1/8*72=9",
                     _Ps).

test(p6, Ps == ["42/7+2=8", "48/8+2=8"]) :-
    puzzle_solve_all(["9/3*6=18"-"brbbbrbg",
                      "20/5+4=8"-"rbgbgrgg"],
                     "42/7+2=8",
                     Ps).

test(p7, Ps == ["9*2*5=90", "9*5*2=90"]) :-
    puzzle_solve_all(["6/1*9=54"-"bbbgrgrb",
                      "3+9*8=75"-"bbrgbgbr"],
                     "9*2*5=90",
                     Ps).

test(p8, Ps == ["3*46=138", "8*81=648", "81*8=648"]) :-
    puzzle_solve_all(["6/1*9=54"-"rbrrbrbr",
                      "16-2*4=8"-"rrbbrrrg"],
                     "81*8=648",
                    Ps).
test(p8, Ps == ["61*8=488", "81*6=486", "81*8=648"]) :-
    puzzle_solve_all(["4*9-1=35"-"rrbbrrbb",
                      "1/4*28=7"-"rbrrbrrb"],
                     "81*8=648",
                    Ps).
test(p8, Ps == ["81*8=648"]) :-
    puzzle_solve_all(["6+9-3=12"-"rbbbbrrb",
                      "1*56/7=8"-"rrbrbbrg"],
                     "81*8=648",
                     Ps).

test(p9, Ps == ["14+49=63", "19+44=63"]) :-
    puzzle_solve_all(["6+9-3=12"-"rrrbrgrb",
                      "13+46=59"-"grggrgbr"],
                     "19+44=63",
                     Ps).

test(p10, Ps == ["5*51=255"]) :-
    puzzle_solve_all(["6+9-3=12"-"bbbbbrrr",
                      "1*20/4=5"-"rgrbbbrg"],
                     "5*51=255",
                     Ps).

test(p11, Ps == ["31-4*6=7", "36-5*7=1", "36-7*5=1", "43-7*6=1", "31-5*5=6", "31-5*6=1"]) :-
    puzzle_solve_all(["6+9-3=12"-"rbbrrrrb",
                      "10-6/3=8"-"rbgrbrgb"],
                     "31-5*6=1",
                     Ps).
test(p11, Ps == ["31-5*6=1"]) :-
     puzzle_solve_all(["9/3*8=24"-"bbrrbrbb",
                        "3*7-15=6"-"grbrrrgr"],
                      "31-5*6=1",
                      Ps).

test(p12, Ps == ["405/9=45"]) :-
     puzzle_solve_all(["3*8/1=24"-"bbbgbgbr",
                       '450/6=75'-  % atom instead of string
                        grrgbgbg],
                      "405/9=45",
                      Ps).

test(p13, Ps == ["97-31=66"]) :-
    puzzle_solve_all(["3*8/1=24"-"rbbbggbb",
                      "19+31=50"-"brbgggbb"],
                     "97-31=66",
                     Ps).

test(p14, Ps == ["3*8+8=32", "3*8-22=2", "3*8-2=22", "3*3*8=72", "3*8*3=72"]) :-
    puzzle_solve_all(["3*54=162"-"ggbbrbbg"],
                     "3*8-22=2",
                     Ps).
test(p14, Ps == ["3*8-22=2", "5*5-23=2"]) :-
    puzzle_solve_all(["4/3*9=12"-bbrrbrbg,
                      "20-3*6=2"-rbrrrbgg],
                     "3*8-22=2",
                     Ps).

test(p15, Ps == ["30/5+2=8"]) :-
    puzzle_solve_all(["4/3*9=12"-"brrbbrbr",
                      "20/5+3=7"-"rggggrgb"],
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
    puzzle_solve_all(["6*8/12=4"-"brbrrggr"],
                     "7/14*2=1",
                     Ps).
test(p17, Ps == ["2/14*7=1", "7/14*2=1"]) :-
    puzzle_solve_all(["4*9/3=12"-"rrbrbrrr",
                      "1/4*20=5"-"rgrrrbgb",
                      "6/12*8=4"-"bggrgbgr"],
                     "7/14*2=1",
                     Ps).
test(p18, Ps == ["53-9*5=8"]) :-
    puzzle_solve_all(["4*9/3=12"-"brrbrrbb",
                      "57-6*9=3"-"gbgbgrgr"],
                     "53-9*5=8",
                     Ps).
test(p19, Ps == ["24+73=97", "24+23=47"]) :-
    puzzle_solve_all(["7+8-5=10"-"rrbbbgbb",
                      "4*6+3=27"-"rbbrggrg"],
                     "24+23=47",
                     Ps).

:- end_tests(nerdle0).

:- begin_tests(nerdle2).

test(p20, % Ps == ["6*7+0=42", "6*7-0=42", "7*6+0=42", "7*6-0=42", "4*9+6=42", "5*7+7=42", "5*9+7=52", "7*5+7=42", "9*4+6=42", "9*5+7=52", "6*6+6=42", "7*7-7=42", "7*9+9=72", "9*7+9=72", "9*9-9=72"]) :-
     Ps == ["4*9+6=42", "5*7+7=42", "5*9+7=52", "7*5+7=42", "9*4+6=42", "9*5+7=52", "6*6+6=42", "7*7-7=42", "7*9+9=72", "9*7+9=72", "9*9-9=72"]) :-
    puzzle_solve_all(["3*8/2=12"-"bgbbbgbg"],
                     "9*9-9=72",
                     Ps).

test(p21, Ps == ["3*6+6=24"]) :-
    puzzle_solve_all(["4*9/3=12"-"rgbbrgbr",
                      "3*4+8=20"-"ggrgbggb",
                      "3*7+4=25"-"ggbgrggb"],
                      "3*6+6=24",
                     Ps).
test(p22, Ps == ["3*7+4=25", "3*6+6=24"]) :-
    puzzle_solve_all(["4*9/3=12"-"rgbbrgbr",
                      "3*4+8=20"-"ggrgbggb"],
                      "3*6+6=24",
                     Ps).

test(p23, Ps == ["83-41=42"]) :-
    puzzle_solve_all(["4*9/3=12"-"rbbbrgrg",
                      "14+38=52"-"rrbrrgbg"],
                      "83-41=42",
                     Ps).

test(p23, Ps == ["6+7*8=62", "6+8*7=62", "6+2*8=22", "6+8*2=22", "8+2*7=22", "8+7*2=22", "8+8*8=72"]) :-
    puzzle_solve_all(["4*9/3=12"-"brbbbgbg"],
                      "6+8*2=22",
                    Ps).
test(p23, Ps == ["6+8*2=22"]) :-
    puzzle_solve_all(["4*9/3=12"-"brbbbgbg",
                      "6+7*8=62"-"ggbgrgbg"],
                      "6+8*2=22",
                    Ps).

test(p24, Ps == ["47*8=376", "48*7=336"]) :-
    puzzle_solve_all(["4*9/3=12"-"grbbrrbb",
                      "43-5*7=8"-"grbbrrrr"],
                      "47*8=376",
                     Ps).

test(p25, Ps == ["15-2-6=7"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbbbbrrr",
                      "0+12-5=7"-"bbrggrgg"],
                      "15-2-6=7",
                     Ps).

test(p26, Ps == ["5*1*4=20"]) :-
    puzzle_solve_all(["4/3*9=12"-"rbbgbgrr",
                      "1+4*6=25"-"rbrgbggr"],
                     "5*1*4=20",
                     Ps).

test(p27, Ps == ["3*63=189"]) :-
    puzzle_solve_all(["7+8-3=12"-"bbrbrrrb",
                      "18*3/6=9"-"rrrgbrrg"],
                     "3*63=189",
                     Ps).
test(p27, Ps == ["3*63=189"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbrrrrrb",
                      "3*8-15=9"-"ggrbrbrg"],
                     "3*63=189",
                     Ps).

test(p28, Ps == ["216/3=72"]) :-
     puzzle_solve_all(["7+8-3=12"-"rbbbggrg"],
                      "216/3=72",
                      Ps).
test(p28, Ps == ["160/5=32", "310/5=62", "156/3=52", "186/3=62", "216/3=72", "312/6=52", "132/6=22"]) :-
    puzzle_solve_all(["4/3*9=12"-"brrbbgrg"],
                     "216/3=72",
                     Ps).
test(29, error(nerdle_assertion(fail,nerdle:verify_guess(['4',/,'9',*,'3',=,'1','2'])))) :-
    puzzle_solve_all(["4/9*3=12"-"bbrbrrrg"],
                     "3-10+9=2",
                     _Ps).

test(p30, Ps == ["53+42=95"]) :-
    puzzle_solve_all(["4/3*9=12"-"rbrbrgbr",
                      "23+45=68"-"rgggrgbb"],
                     "53+42=95",
                     Ps).

test(p31, Ps == ["234/78=3"]) :-
    puzzle_solve_all(["7+8-3=12"-"rbrbrrbr",
                      "37*8=296"-"rrbrrrbb"],
                     "234/78=3",
                     Ps).

test(p32, % Ps == ["5*7+4=39", "5*9+3=48", "7*5+4=39", "8*4+7=39", "9*4+0=36", "9*4-0=36", "9*4-6=30", "9*5+3=48", "3*9+7=34", "9*4+7=43", "9*9+3=84", "9*4+3=39", "9*4-3=33"]) :-
     Ps == ["5*7+4=39", "5*9+3=48", "7*5+4=39", "8*4+7=39", "9*4-6=30", "9*5+3=48", "3*9+7=34", "9*4+7=43", "9*9+3=84", "9*4+3=39", "9*4-3=33"]) :-
    puzzle_solve_all(["4/3*9=12"-"rbrrrgbb"],
                     "5*9+3=48",
                     Ps).

test(p33, Ps == ["192/64=3"]) :- % This one was done by human
    puzzle_solve_all(["7+8-2=13"-"bbbbrrrg",
                      "6*6/12=3"-"rbbgrrgg",
                      "129/43=3"-"grrgrbgg",
                      "162/54=3"-"grggbggg"],
                     "192/64=3",
                     Ps).
test(p33, Ps == ["192/64=3"]) :- % This one used program's recommendations
    puzzle_solve_all(["7+8-2=13"-"bbbbrrrg",
                      "162/54=3"-"grggbggg"],
                     "192/64=3",
                     Ps).

test(p34, Ps == ["372/93=4"]) :-
    puzzle_solve_all(["6*5/3=10"-"bbbgrrbb",
                      "378/42=9"-"ggbgrrgr"],
                     Ps).

test(p34, Ps == ["2*53=106"]) :-
    puzzle_solve_all(["3*8/1=24"-"rgbbrrrb",
                      "2*53=106"-"gggggggg"],
                     "2*53=106",
                     Ps).

test(p35, error(nerdle_assertion(fail,nerdle:verify_guess(['2','4',/,'8',-,'9',=,'3'])))) :- % Ps == ["28/4-4=3"]) :-
    puzzle_solve_all(["12+3-9=6"-"brbrgbgb",
                      "24/8-9=3"-"grgrgbgg"],
                     "28/4-4=3",
                     _Ps).

test(p36, Ps == ["45/9-4=1", "54/9-1=5"]) :-
    puzzle_solve_all(["3*8/1=24"-"bbbrrrbr",
                      "1+40/5=9"-"rbrbrrgr"],
                     "54/9-1=5",
                     Ps).

test(p37, Ps == ["84/7-3=9", "747/83=9"]) :-
    puzzle_solve_all(["4/3*9=12"-"rrrbrrbb",
                      "3+45/9=8"-"rbrbrrgr"],
                     "747/83=9",
                     Ps).

% slow for 1st result:
test(p38, Ps == ["13-9/1=4", "147/49=3"]) :-
    puzzle_solve_all(["4/3*9=12"-rrrbrrrb,
                      "15/3+4=9"-"gbrrbrgr"],
                     "147/49=3",
                     Ps).

test(p39, Ps == ["9+3*2=15"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbggrggr"],
                     "9+3*2=15",
                     Ps).

:- end_tests(nerdle2).

:- begin_tests(nerdle4).

test(p40, % Ps == ["20/2-4=6", "24/4+0=6", "24/4-0=6", "24/6+0=4", "24/6-0=4", "24/6-4=0"]) :-
     Ps == ["20/2-4=6", "24/6-4=0"]) :-
    puzzle_solve_all(["1/4*28=7"-"brrbrbgb",
                      "230/46=5"-"gbrrrrgb"],
                     "20/2-4=6",
                     Ps).

test(p41, Ps == ["92-50=42", "97-55=42", "99-57=42", "94-52=42"]) :-
    puzzle_solve_all(["4/3*9=12"-"rbbbrgbg",
                      "98-46=52"-"gbgrbgrg"],
                     "92-50=42",
                     Ps).

test(p42, Ps == ["15*3/9=5"]) :-
    puzzle_solve_all(["4/3*9=12"-"brrrrrrb",
                      "1*63/7=9"-"grbggbgr"],
                     "15*3/9=5",
                     Ps).

test(p43, Ps == ["158/2=79", "196/7=28", "118/2=59", "178/2=89", "180/2=90", "189/7=27", "190/2=95", "196/2=98", "712/8=89", "728/8=91", "182/2=91", "192/2=96", "198/2=99"]) :-
     puzzle_solve_all(["4/3*9=12"-"brbbrgrr"],
                      "190/2=95",
                      Ps).

test(p44, error(nerdle_assertion(fail,nerdle:verify_guess(['1','6',+,'2','0',=,'4','5'])))) :- % Ps == ["4*4+5=21", "4*5+1=21", "5*4+1=21"]) :-
    puzzle_solve_all(["7+8-3=12"-"brbbbgrr",
                      "16+20=45"-"rbrrbgrr"],
                     "4*4+5=21",
                     _Ps).

test(p45, Ps == ["207/3=69", "267/3=89", "296/8=37", "392/7=56", "632/8=79", "259/7=37", "270/3=90", "285/3=95", "288/3=96", "232/8=29", "237/3=79", "279/3=93", "297/3=99"]) :-
    puzzle_solve_all(["4/3*9=12"-"brrbrgbr"],
                     "207/3=69",
                     Ps).

test(p46, % Ps == ["10-8+6=8", "16-8+0=8", "6-8+10=8"]) :-
     Ps == ["10-8+6=8", "6-8+10=8"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbbbbrrb",
                      "0+15-7=8"-"rrrbrbgg"],
                     "10-8+6=8",
                     Ps).

test(p47, Ps == ["7+8-13=2","7-13+8=2","8+7-13=2","8-13+7=2","10-3-5=2","10-5-3=2","13-5-6=2","13-6-5=2","11-3-6=2","11-6-3=2","12-3-7=2","12-7-3=2","13-3-8=2","13-8-3=2"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbrbbrrg"],
                     "12-7-3=2",
                     Ps).
test(p47, Ps == ["12-7-3=2"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbrbbrrg",
                      "7+8-13=2"-"rbbrrggg"],
                     "12-7-3=2",
                     Ps).

test(p48, Ps == ["28+63=91", "68+23=91"]) :-
    puzzle_solve_all(["4/3*9=12"-"bbrbrgrr",
                      "12+83=95"-"rrgrgggb"],
                     "68+23=91",
                     Ps).

test(p49, Ps == ["5*34=170", "5*3-11=4", "5*3-14=1", "6*3-14=4", "7*3-14=7", "7*3-17=4"]) :-
    puzzle_solve_all(["4/3*9=12"-"rbgrbrrb"],
                     "5*3-14=1",
                     Ps).
test(p49, Ps == ["3*5-10=5", "3*5-14=1", "3*5-15=0", "5*3-10=5", "5*3-14=1", "5*3-15=0"]) :-
    puzzle_solve_all(["7+8-3=12"-"bbbgrrrb",
                      "6*9-51=3"-"bgbgrrgr"],
                     "5*3-14=1",
                     Ps).

test(p50, % Ps == ["12-0-7=5", "12-5-7=0", "12-7-0=5"]) :-
     Ps == ["12-5-7=0"]) :-
    puzzle_solve_all(["4*9/3=12"-"bbbbbrrr",
                      "0+12-5=7"-"rbrrgrgr"],
                     "12-5-7=0",
                     Ps).

test(p51, Ps == ["10+46=56"]) :-
    puzzle_solve_all(["4*9/3=12"-"rbbbbgrb",
                      "14+56=70"-"grgrggbr"],
                     "10+46=56",
                     Ps).

test(p52, Ps == ["5*6-8=22", "6*5-8=22", "7*8+6=62", "8*7+6=62", "2*7+8=22", "2*8+6=22", "7*2+8=22", "8*2+6=22", "8*8+8=72", "8*8-2=62", "2*6*6=72", "6*2*6=72", "6*6*2=72"]) :-
    puzzle_solve_all(["4*9/3=12"-bgbbbgbg],
                     "8*8-2=62",
                     Ps).
test(p52, Ps == ["6*8-6=42", "8*8-2=62"]) :-
    puzzle_solve_all(["7+8-3=12"-"bbggbgbg"],
                     "8*8-2=62",
                     Ps).

test(p53, Ps == ["414/6=69", "414/9=46", "455/5=91", "497/7=71", "441/9=49"]) :-
    puzzle_solve_all(["4*9/3=12"-"gbrgbgrb"],
                     "414/6=69",
                     Ps).

test(p54, Ps == ["38-10=28", "38-18=20"]) :-
    puzzle_solve_all(["1+5*9=46"-"rbbbbgbb",
                      "30/2-8=7"-"grbrrrrb"],
                     "38-10=28",
                     Ps).
test(p54, Ps == ["38-10=28"]) :-
    puzzle_solve_all(["4*9/3=12"-"bbbbrgrr",
                      "6*7+8=50"-"bbbbrgbr"],
                     "38-10=28",
                     Ps).

test(p55, Ps == ["2+7*8=58"]) :-
    puzzle_solve_all(["3*8-4=20"-"brrbbgrb",
                      "6/1+9=15"-"bbbrbgbr"],
                     "2+7*8=58",
                     Ps).
test(p55, Ps == ["2+7*8=58"]) :-
    puzzle_solve_all(["9/3+7=10"-"bbbrrgbb",
                      "8*7-4=52"-"rrgbbggr"],
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

:- end_tests(nerdle4).

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

test(p63, error(nerdle_assertion(fail,nerdle:verify_guess(['1','2',/,'3',+,'9',=,'4'])))) :- % Ps == ["1+10-2=9", "1-2+10=9", "10+1-2=9"]) :-
    puzzle_solve_all(["12/3+9=4"-grbbrrgb,
                      "51-6*7=9"-brrbbbgg],
                     "1+10-2=9",
                     _Ps).

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

test(p78, % Ps == ["10-6+2=6", "12-6+0=6"]) :-
     Ps == ["10-6+2=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrrr,
                      "1+56/7=9"-grbgbbgb],
                     "10-6+2=6",
                     Ps).

test(p79, Ps == ["340/68=5","360/45=8","430/86=5"]) :-
    puzzle_solve_all(["12/3+0=4"-bbrrbrgr,
                      "51-6*7=9"-rbbrbbgb],
                     "340/68=5",
                     Ps).

:- end_tests(nerdle6).

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
    puzzle_solve_all(["3*8-4=20"-bbbbbgbg,
                      "6/1+9=15"-brbrrggb],
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

test(90, Ps == ["5+16/4=9"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrrbb,
                      "1+56/7=9"-rgrggbgg],
                     "5+16/4=9",
                     Ps).

test(91, Ps == ["10+48=58","18+40=58","40+18=58","48+10=58"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrbrgbr,
                      "6/1+9=15"-bbrrbgbr],
                     "40+18=58",
                     Ps).

test(92, Ps == ["11-1-4=6","11-1-6=4","11-4-1=6","11-4-6=1","14-4-4=6","14-4-6=4","16-4-6=6"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrrrbb,
                      "1+56/7=9"-gbbrbbgb],
                     "11-1-4=6",
                     Ps).

test(93, Ps == ["49+48=97"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrbrgbb,
                             "6/1+9=15"-bbbrrgbb],
                            "49+48=97",
                            Ps).

test(94, Ps == ["8*6-39=9","8*9-63=9"]) :-
    puzzle_solve_all(["3*8-4=20"-rgrgbrbb,
                      "1+56/7=9"-bbbrbbgg],
                     "8*9-63=9",
                     Ps).

test(95, Ps == ["7-12/6=5"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrrb,
                      "1+56/7=9"-rbrrgrgb],
                     "7-12/6=5",
                     Ps).

test(96, Ps == ["2*5*7=70","2*7*5=70","5*2*7=70","5*7*2=70","7*2*5=70","7*5*2=70","2*5*5=50","5*2*5=50","5*5*2=50"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbbbgrg,
                      "6/1+9=15"-bbbbbgbr],
                     "2*5*7=70",
                     Ps).

test(97, Ps == ["89-19=70","99-19=80"]) :-
    puzzle_solve_all(["3*8-4=20"-bbrrbgbg,
                      "6/1+9=15"-bbrbggbb],
                     "99-19=80",
                     Ps).

test(98, Ps == ["44-6*6=8"]) :-
    puzzle_solve_all(["3*8-4=20"-brrrrrbb,
                      "1+56/7=9"-bbbgbbgb],
                     "44-6*6=8",
                     Ps).

test(99, Ps == ["13-1-7=5","13-5-7=1","15-3-7=5","15-5-7=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbbrbrbb,
                      "1+56/7=9"-gbrbbggb],
                     "13-5-7=1",
                     Ps).

:- end_tests(nerdle8).

:- begin_tests(nerdle10).

test(100, Ps == ["89-61=28","89-68=21"]) :-
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

test(108, % Ps == ["21/3-7=0","21/7-0=3","21/7-3=0","3-21/7=0","7-21/3=0","20/2-7=3","7-12/3=3","27/3-7=2"]) :-
     Ps == ["21/3-7=0","21/7-3=0","3-21/7=0","7-21/3=0","20/2-7=3","7-12/3=3","27/3-7=2"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbrbrrr,
                      "52-8*6=4"-brrbbbgb],
                     "7-12/3=3",
                     Ps).

test(109, Ps == ["20+68=88","60+28=88"]) :-
    puzzle_solve_all(["5*9-8=37"-bbbbggbb,
                      "8+4/2=10"-rrbbrgbr],
                     "60+28=88",
                     Ps).

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

test(118, % Ps == ["13-0-5=8","13-0-8=5","13-5-8=0","13-8-5=0"]) :-
     Ps == ["13-5-8=0","13-8-5=0"]) :-
    puzzle_solve_all(["12/3+0=4"-gbbrbrgb,
                      "51-6*7=9"-rrgbbbgb],
                     "13-5-8=0",
                     Ps).

test(119, Ps == ["6+8-14=0","8+6-14=0","4+7-11=0","7+4-11=0","7+7-14=0"]) :-
    puzzle_solve_all(["12/3+0=4"-rbbbrrgr,
                      "0+14-9=5"-rgrrrbgb],
                     "6+8-14=0",
                    Ps).

:- end_tests(nerdle10).

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

test(139, % Ps == ["0+18/3=6","0+18/6=3","18/3+0=6","180/30=6","180/60=3"]) :-
     Ps == ["180/30=6","180/60=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrbbrbr,
                      "51-6*7=9"-brbrbbgb],
                     "180/30=6",
                     Ps).


:- end_tests(nerdle12).

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

:- end_tests(nerdle14).

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

test(170, Ps == ["7*9-6=57","7*9-7=56","9*7-6=57","9*7-7=56","9*9-5=76"]) :-
    puzzle_solve_all(["3*8-4=20"-bgbgbgbb,"6/1+9=15"-rbbbrgbr],
                     "9*7-7=56",
                     Ps).

test(171, Ps == ["4+5+7=16","4+7+5=16","5+4+7=16","7+4+5=16"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbbrgbb,
                      "6/1+9=15"-rbbgbggr],
                     "4+5+7=16",
                     Ps).

test(172, % Ps == ["0+14-6=8","0+14-8=6","4+10-6=8","4+10-8=6"]) :-
     Ps == ["4+10-6=8","4+10-8=6"]) :-
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

:- end_tests(nerdle16).

:- begin_tests(nerdle18).

test(180, Ps == ["4+8-11=1","8+4-11=1"]) :-
    puzzle_solve_all(["2+3*5=17"-bgbbbrrb,
                      "0+14-6=8"-bgrrrbgr],
                     "4+8-11=1",
                     Ps).

test(181, % Ps == ["0+13-6=7","3+10-6=7","3+16/4=7","0+10-3=7","1+18/3=7","3+13-9=7","1+13-7=7","3+11-7=7"]) :-
     Ps == ["3+10-6=7","3+16/4=7","1+18/3=7","3+13-9=7","1+13-7=7","3+11-7=7"]) :-
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

test(182, error(nerdle_assertion(fail,nerdle:verify_guess(['5',+,'9',-,'9',=,'1','4'])))) :- % Ps == ["9+6/1=15","6+5+8=19","8+5+6=19","9+0+6=15","9+6+0=15","9+5+1=15"]) :-
    puzzle_solve_all(["2+3*5=17"-bgbbrggb, "5+9-9=14"-rgrbbggb],
                     "1+5+9=15",
                     _Ps).
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

test(185, Ps == ["1+4*7=29","7+2*7=21"]) :-
    puzzle_solve_all(["2*3+7=13"-rrbrggrb],
                     "7+2*7=21",
                     Ps).

test(186, Ps == ["68-22=46","68-24=44","68-42=26","68-44=24","88-24=64","88-42=46","88-64=24"]) :-
    puzzle_solve_all(["2*3+7=13"-rbbbbgbb,
                      "460/5=92"-rrbbbgbr],
                     "68-42=26",
                     Ps).

test(187, Ps == ["76-8*9=4","76-9*8=4","48-7*6=6","78-8*9=6","78-9*8=6","87-9*9=6"]) :-
    puzzle_solve_all(["2*3+7=13"-brbbrrbb,
                      "50-6*7=8"-bbgrgrgr],
                     "87-9*9=6",
                     Ps).

test(188, Ps == ["5*9-43=2","9*5-43=2","4*9-32=4","4*9-34=2","9*4-32=4","9*4-34=2"]) :-
    puzzle_solve_all(["2*3+7=13"-rgrbbrbb,
                      "3*24/8=9"-rgrrbbgr],
                     "9*4-34=2",
                     Ps).

test(189, Ps == ["399/7=57","539/7=77"]) :-
    puzzle_solve_all(["2*3+7=13"-bbrbggbb,
                      "86-37=49"-bbbrggbr],
                     "399/7=57",
                     Ps).

test(190, Ps == ["15/3-2=3","32/8-1=3","102/34=3","129/43=3","12/2-3=3","12/3-1=3","12-3-6=3","12-6-3=3","13-2-8=3","13-8-2=3","18/2/3=3","18/3/2=3","132/44=3"]) :-
    puzzle_solve_all(["2*3+7=13"-rbrbbrrg],
                     "13-2-8=3",
                     Ps).
test(190, Ps == ["12-6-3=3","13-2-8=3"]) :-
    puzzle_solve_all(["2*3+7=13"-rbrbbrrg,
                      "15/3-2=3"-gbbrgrgg],
                     "13-2-8=3",
                     Ps).

test(191, Ps == ["92*4=368","49*8=392","94*3=282","98*3=294","98*4=392"]) :-
    puzzle_solve_all(["2*3+7=13"-rrrbbrbb,
                      "3/8*24=9"-rbrrrrrr],
                     "49*8=392",
                     Ps).

test(192, Ps == ["27/3-3=6","27/3-6=3","27/3-7=2"]) :-
    puzzle_solve_all(["2+3*5=17"-gbrbbrbr,
                      "27/3-9=0"-gggggbgb],
                     "27/3-3=6",
                     Ps).

test(193, Ps == ["10+14=24","14+10=24"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrrbgrb,
                      "12+48=60"-grgrbgbr],
                     "10+14=24",
                     Ps).

test(194, % Ps == ["6*9+7=61","7*9+8=71","9*6+7=61","9*7+8=71","9*9+0=81","8*8+7=71","8*9+9=81","9*8+9=81"]) :-
     Ps == ["6*9+7=61","7*9+8=71","9*6+7=61","9*7+8=71","8*8+7=71","8*9+9=81","9*8+9=81"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbgbgrb],
                     "9*7+8=71",
                     Ps).

test(195, Ps == ["18-4-5=9","18-4-9=5","18-9-4=5","18-9-5=4","14-1-5=8","14-1-8=5","14-8-5=1","15-4-5=6","15-4-6=5","15-6-4=5","15-6-5=4","14-4-5=5"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrrb,
                      "40/5-1=7"-rbbrgrgb],
                     "15-6-4=5",
                     Ps).

test(196, Ps == ["11+5-9=7","11-9+5=7","15+1-9=7","15-9+1=7"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrrrrb,
                      "1+56/7=9"-grrbbrgr],
                     "11+5-9=7",
                     Ps).

test(197, Ps == ["354/59=6","504/56=9","450/50=9","459/51=9","495/55=9"]) :-
    puzzle_solve_all(["7*8-54=2"-bbbbgrgb],
                     "459/51=9",
                     Ps).
test(197, Ps == ["450/50=9","459/51=9"]) :-
    puzzle_solve_all(["7*8-54=2"-bbbbgrgb,
                      "354/59=6"-bgrggrgb],
                     "459/51=9",
                     Ps).

test(198, Ps == ["30+18=48","30+19=49","38+10=48","39+10=49","30+11=41","30+14=44","31+10=41","34+10=44"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrbgrr,
                      "13+47=60"-rrgrbgbr],
                     "30+18=48",
                     Ps).

test(199, Ps == ["4*7-2=26","4*7-6=22","4*7-4=24"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbgbb,
                      "4*7-8=20"-ggggbggb],
                     "4*7-4=24",
                     Ps).

:- end_tests(nerdle18).

:- begin_tests(nerdle20).

test(200, Ps == ["31+47=78","34+47=81"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrbgrr,
                      "13+47=60"-rrggggbb],
                     "31+47=78",
                     Ps).


test(201, Ps == ["5+3+9=17","5+3+7=15","5+3+3=11"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbgrggr,
                      "5-3+8=10"-gbggbggb],
                     "5+3+9=17",
                     Ps).

test(202, Ps == ["4*9+2=38","4*8+2=34"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrgbgbr,
                      "4*3+8=20"-ggrgrgrb],
                     "4*9+2=38",
                     Ps).

test(203, Ps == ["4+18/6=7","4+16/4=8"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrbrrb,
                      "1+64/8=9"-rgrrgrgb],
                     "4+18/6=7",
                     Ps).

test(204, Ps == ["7*3-12=9","7*3-19=2","7*3-20=1","7*3-21=0"]) :-
    puzzle_solve_all(["2*3+8=14"-rggbbrrb,
                      "1*42/6=7"-rgbrbbgr],
                     "7*3-12=9",
                     Ps).

test(205, Ps == ["19-2*6=7","19-6*2=7","12*1-5=7","12-1*5=7","12-5*1=7","17-2*5=7","17-5*2=7","11-2*2=7"]) :-
    puzzle_solve_all(["2*3+7=13"-rrbbrrrb,
                      "1/4*28=7"-gbbrrbgg],
                     "19-2*6=7",
                     Ps).
test(205, Ps == ["17-2*6=5","19-2*6=7","17-2*5=7","17-2*8=1","11-2*2=7"]) :-
    puzzle_solve_all(["2*3+7=13"-rrbbrrrb,
                      "14-2*7=0"-gbgggrgb],
                     "19-2*6=7",
                     Ps).

test(206, Ps == ["44-5*7=9","44-7*5=9"]) :-
    puzzle_solve_all(["2*3+8=14"-brbbbrbr,
                      "54-6*9=0"-rggbgrgb],
                     "44-5*7=9",
                     Ps).

test(207, Ps == ["5+8*3=29","8+7*3=29","3+5*5=28","3+8*3=27","7+7*3=28","7+8*2=23","8+5*3=23","8+6*3=26","8+8*3=32"]) :-
    puzzle_solve_all(["2*3+8=14"-rrrrrgbb],
                     "8+7*3=29",
                     Ps).

test(208, Ps == ["469/67=7","666/74=9"]) :-
    puzzle_solve_all(["2*3+8=14"-bbbbbrbr,
                      "54/6-9=0"-brrrbrgb],
                     "666/74=9",
                     Ps).

test(209, Ps == ["2*9-13=5","2*9-15=3","2*53=106","2*65=130","2*7-11=3","2*7-13=1","2*63=126","2*66=132"]) :-
    puzzle_solve_all(["2*3+8=14"-ggrbbrrb],
                     "2*9-13=5",
                     Ps).

test(210, Ps == ["50-23=27","50-27=23","57-20=37","57-27=30","57-30=27","57-37=20","73-20=53","73-23=50","73-50=23","73-53=20"]) :-
    puzzle_solve_all(["2*3+8=14"-rbrbbgbb,
                      "360/5=72"-rbrbrgrr],
                     "57-27=30",
                     Ps).

test(211, Ps == ["16-1-7=8","16-7-1=8","16-8-1=7","16-8-7=1"]) :-
    puzzle_solve_all(["2*3+8=14"-bbbbrrrb,
                      "7-16/8=5"-rrrrbrgb],
                     "16-1-7=8",
                     Ps).

test(212, Ps == ["4+5*6=34","4+6*5=34","6+4*7=34","6+7*4=34","7+9*3=34","9+5*5=34"]) :-
    puzzle_solve_all(["2*3+8=14"-brrrbgbg],
                     "7+9*3=34",
                     Ps).

test(213, % Ps == ["3*8-4=20","4*8-0=32","4*8-2=30","4*8-9=23"]) :-
     Ps == ["3*8-4=20","4*8-2=30","4*8-9=23"]) :-
    puzzle_solve_all(["8+2*3=14"-rbrrrgbr],
                     "4*8-2=30",
                     Ps).

test(214, % Ps == ["0+6*9=54","0+9*6=54","9+5*7=44","9+7*5=44","5+7*7=54","9+5*9=54","9+9*5=54"]) :-
     Ps == ["9+5*7=44","9+7*5=44","5+7*7=54","9+5*9=54","9+9*5=54"]) :-
    puzzle_solve_all(["3*8/1=24"-brbbbgbg],
                     "9+5*7=44",
                     Ps).

test(215, Ps == ["5-1+9=13","5-3+9=11","9-1+5=13","9-3+5=11","9-3+9=15","9-5+9=13"]) :-
    puzzle_solve_all(["3*8/1=24"-rbbbrgbb,
                      "6+7-3=10"-brbrrggb],
                     "5-1+9=13",
                     Ps).

test(216, Ps == ["11-1*2=9","11-1*9=2","11-9*1=2","19-9*2=1"]) :-
    puzzle_solve_all(["3*8/1=24"-brbbrrrb,
                      "17-2*6=5"-gbgrgbgb],
                     "19-9*2=1",
                     Ps).

test(217, Ps == ["19-7-5=7","19-7-7=5","15-1-9=5","15-9-1=5","15-9-5=1","19-9-5=5"]) :-
    puzzle_solve_all(["3*8/1=24"-bbbbrrbb,
                      "10+5-6=9"-gbbrgbgr],
                     "15-1-9=5",
                     Ps).
test(217, Ps == ["15-1-9=5","15-9-1=5"]) :-
    puzzle_solve_all(["3*8/1=24"-bbbbrrbb,
                      "10+5-6=9"-gbbrgbgr,
                      "19-7-5=7"-grgbgrgb],
                     "15-1-9=5",
                     Ps).

test(218, Ps == ["81-35=46","81-36=45","81-45=36","81-46=35","84-13=71","84-33=51","84-53=31","81-33=48","81-43=38","84-43=41"]) :-
    puzzle_solve_all(["3*8/1=24"-rbrbrgbr,
                      "8+9-4=13"-gbbrrgrr],
                     "81-36=45",
                     Ps).

test(219, Ps == ["22-8-5=9","22-8-9=5","22-9-8=5","25-8-8=9"]) :-
    puzzle_solve_all(["3*8/1=24"-bbrbbrrb,
                      "20-5-7=8"-gbgrgbgr],
                     "22-9-8=5",
                     Ps).

:- end_tests(nerdle20).

:- begin_tests(nerdle22).

test(220, Ps == ["3*5+1=16","3*6+1=19","3*6-1=17","3*3+1=10","3*5*1=15"]) :-
    puzzle_solve_all(["3*8/1=24"-ggbbggbb],
                      "3*5+1=16",
                      Ps).

test(221, Ps == ["79-38=41","79-48=31","49-18=31","91-43=48","94-13=81"]) :-
    puzzle_solve_all(["3*8/1=24"-rbrbrgbr,
                      "8+9-4=13"-rbrrrgrr],
                     "79-48=31",
                     Ps).

test(222, Ps == ["5*5-17=8","5*5-18=7"]) :-
    puzzle_solve_all(["3*8/1=24"-bgrbgrbb],
                     "5*5-18=7",
                     Ps).

test(223, Ps == ["8-10+3=1","8-11+3=0"]) :-
    puzzle_solve_all(["3*8/1=24"-rbrbrrbb,
                      "10+3-5=8"-rrrrrbgr],
                     "8-11+3=0",
                     Ps).

test(224, Ps == ["13-2*6=1","17-2*7=3"]) :-
    puzzle_solve_all(["3*8/1=24"-rrbbrrrb,
                      "15-2*3=9"-gbgggrgb],
                     "17-2*7=3",
                     Ps).

test(225, Ps == ["308/7=44","378/7=54"]) :-
    puzzle_solve_all(["3*8/1=24"-gbggbgbg],
                     "378/7=54",
                     Ps).

test(226, Ps == ["32+18=50","38+12=50"]) :-
    puzzle_solve_all(["3*8/1=24"-gbrbrgrb,
                      "31+27=58"-grgrbggr],
                     "32+18=50",
                    Ps).

test(227, Ps == ["17+68=85","17+78=95","18+58=76","19+58=77","58+18=76","59+18=77","67+18=85","17+58=75","57+18=75"]) :-
    puzzle_solve_all(["3*8/1=24"-bbrbrgbb,
                      "7-5+8=10"-rbrrggrb],
                     "19+58=77",
                     Ps).

test(228, Ps == ["54/2/9=3","54/3/2=9","54/3/9=2","54/9/2=3"]) :-
    puzzle_solve_all(["3*8/1=24"-rbbrbrrr,
                      "24/6+3=7"-rggbbrgb],
                     "54/2/9=3",
                     Ps).

test(229, Ps == ["4*6-1=23","6*4-1=23"]) :-
    puzzle_solve_all(["3*8/1=24"-rgbbgggr],
                     "4*6-1=23",
                     Ps).

test(230, Ps == ["50-38=12","30-18=12","38-28=10","39-28=11"]) :-
    puzzle_solve_all(["2*3+8=14"-rbrbgggb],
                     "30-18=12",
                     Ps).

test(231, Ps == ["48/1/6=8","48/1/8=6","48/8/6=1"]) :-
    puzzle_solve_all(["2*3+8=14"-bbbbrrrr,
                      "48/6-1=7"-gggrbrgb],
                     "48/8/6=1",
                     Ps).

test(232, Ps == ["9*3-20=7","9*3-27=0","9*3-22=5","9*3-25=2","7*36=252","7*37=259","9*30=270","6*37=222","7*39=273","9*33=297"]) :-
    puzzle_solve_all(["2*3+8=14"-rggbbrbb],
                     "9*3-25=2",
                     Ps).
test(232, Ps == ["9*3-22=5","9*3-25=2"]) :-
    puzzle_solve_all(["2*3+8=14"-rggbbrbb,
                      "9*3-20=7"-gggggbgb],
                     "9*3-25=2",
                     Ps).

test(233, Ps == ["62-23=39","62-29=33","62-33=29","62-39=23","92-23=69","92-29=63","92-63=29","92-69=23","62-26=36","62-36=26"]) :-
    puzzle_solve_all(["2*3+8=14"-rbrbbgbb,
                      "360/5=72"-rrbbbgbr],
                     "62-26=36",
                     Ps).

test(234, Ps == ["9*89=801","9*99=891"]) :-
    puzzle_solve_all(["2*3+8=14"-bgbbrrrb,
                      "1*56/7=8"-rgbbbbrr],
                     "9*99=891",
                     Ps).

test(235, Ps == ["8*95=760","8*70=560","8*75=600"]) :-
    puzzle_solve_all(["2*3+8=14"-bgbbrrbb,
                      "7*8-50=6"-rgrbrrrr],
                     "8*70=560",
                     Ps).

test(236, Ps == ["43*6=258","40*8=320","42*8=336","44*8=352","58*4=232","80*3=240","80*4=320","82*3=246","84*3=252","88*4=352","82*4=328","83*4=332"]) :-
    puzzle_solve_all(["2*3+8=14"-rrrbrrbr,
                      "3/8*24=9"-rbrrrrrb],
                     "58*4=232",
                     Ps).

test(237, Ps == ["14+65=79","41+56=97","64+15=79","14+55=69","16+49=65","19+46=65","41+55=96","45+46=91","46+19=65","46+45=91","49+16=65","14+45=59","44+15=59"]) :-
    puzzle_solve_all(["3*8/1=24"-bbbbrgbr,
                      "5+9-4=10"-rrrbrgrb],
                     "41+55=96",
                    Ps).
test(237, Ps == ["41+55=96","46+45=91"]) :-
    puzzle_solve_all(["3*8/1=24"-bbbbrgbr,
                      "5+9-4=10"-rrrbrgrb,
                      "14+65=79"-rrgrggbr],
                     "41+55=96",
                     Ps).

test(238, Ps == ["88/8-5=6","88/8-6=5"]) :-
    puzzle_solve_all(["3*8/1=24"-bbrrbrbb,
                      "56/8-7=0"-rrgggbgb],
                     "88/8-6=5",
                     Ps).

test(239, Ps == ["70-49=21","95-74=21","71-49=22"]) :-
    puzzle_solve_all(["2*3+8=14"-rbbbbgrr,
                      "174/6=29"-rrrbbggr],
                     "71-49=22",
                     Ps).

:- end_tests(nerdle22).

:- begin_tests(nerdle24).

test(240, Ps == ["6*8-42=6","6*8-46=2","8*6-42=6","8*6-46=2"]) :-
    puzzle_solve_all(["2*3+8=14"-rgbbrrbr,
                      "4*7-20=8"-rgbgrbgr],
                     "8*6-46=2",
                     Ps).

test(241, Ps == ["7+9+8=24","9+7+8=24","8+8+8=24"]) :-
    puzzle_solve_all(["2*3+8=14"-rbbgggbg],
                     "7+9+8=24",
                     Ps).

test(242, Ps == ["1*5+8=13","5*1+8=13","3*1+8=11"]) :-
    puzzle_solve_all(["2*3+8=14"-bgrggggb],
                     "3*1+8=11",
                     Ps).

% TODO: Test 243 has a lot of possibilities; can a smarter algorithm do better?
%       Also, remove results with "0+" (just as "0*" is removed)
test(243, % Ps == ["0+6*5=30","2+6*5=32","9+6*5=39","6+6*5=36"]) :-
     Ps == ["2+6*5=32","9+6*5=39","6+6*5=36"]) :-
    puzzle_solve_all(["1*5+8=13"-brrrbgbr,
                      "3+6*7=45"-rgggbgbr],
                     "6+6*5=36",
                     Ps).
test(243, % Ps == ["0+6*5=30","9+6*5=39","6+6*5=36"]) :-
     Ps == ["9+6*5=39","6+6*5=36"]) :-
    puzzle_solve_all(["1*5+8=13"-brrrbgbr,
                      "3+6*7=45"-rgggbgbr,
                      "2+6*5=32"-bggggggb],
                     "6+6*5=36",
                     Ps).
test(243, % Ps == ["0+6*5=30","6+6*5=36"]) :-
     Ps == ["6+6*5=36"]) :-
    puzzle_solve_all(["1*5+8=13"-brrrbgbr,
                      "3+6*7=45"-rgggbgbr,
                      "2+6*5=32"-bggggggb,
                      "9+6*5=39"-bggggggb],
                     "6+6*5=36",
                     Ps).

test(244, Ps == ["7*9-56=7","7*9-57=6","9*7-56=7","9*7-57=6","9*9-75=6","9*9-76=5","6*95=570","6*96=576","7*95=665","9*75=675","7*97=679"]) :-
    puzzle_solve_all(["2*3+8=14"-bgbbbrbb],
                     "9*9-76=5",
                     Ps).

test(245, Ps == ["296/8=37","256/8=32","280/8=35","232/8=29","288/8=36"]) :-
    puzzle_solve_all(["2*3+8=14"-gbrbggbb],
                     "288/8=36",
                     Ps).

test(246, Ps == ["63-50=13","63-53=10","31-15=16","31-16=15"]) :-
    puzzle_solve_all(["2*3+8=14"-bbrbbggb,
                      "56-37=19"-rrgrbggb],
                     "31-15=16",
                     Ps).
test(246, Ps == ["31-15=16","31-16=15"]) :-
    puzzle_solve_all(["2*3+8=14"-bbrbbggb,
                      "56-37=19"-rrgrbggb,
                      "63-50=13"-rrgrbggb],
                     "31-15=16",
                     Ps).

test(247, Ps == ["3*7-2=19","3*7-9=12","9*2-3=15","9*2-5=13","3*5-2=13","3*5-3=12","3*6-2=16","3*6-6=12","6*6/3=12","7*2-1=13","7*2-3=11","3*2*2=12"]) :-
    puzzle_solve_all(["2*3+8=14"-rgrbbggb],
                     "7*2-1=13",
                     Ps).
test(247, Ps == ["7*2-1=13","7*2-3=11"]) :-
    puzzle_solve_all(["2*3+8=14"-rgrbbggb,
                      "3*7-2=19"-rgrgrggb],
                     "7*2-1=13",
                     Ps).

test(248, Ps == ["8*9+2=74"]) :-
    puzzle_solve_all(["12+5-9=8"-brrbbrrr,
                      "8*9+2=74"-gggggggg],
                     "8*9+2=74",
                     Ps).
test(248, Ps == ["8*9+2=74"]) :-
    puzzle_solve_all(["2*3+8=14"-rgbgrgbg,
                      "6*8+4=52"-bgrgrgbr],
                     "8*9+2=74",
                     Ps).

test(249, Ps == ["49+26=75","54+42=96"]) :-
    puzzle_solve_all(["12+5-9=8"-brgrbrrb,
                      "25+39=64"-rrgbrgrr],
                     "54+42=96",
                     Ps).

test(250, Ps == ["8*90=720","8*99=792"]) :-
    puzzle_solve_all(["12+5-9=8"-brbbbrrr,
                      "8*9/2=36"-gggbrrbb],
                     "8*90=720",
                     Ps).

test(251, Ps == ["1*48/6=8","1*64/8=8","1/6*48=8","1/8*64=8","14*4/7=8","14/7*4=8","16*3/6=8","16*4/8=8","16/6*3=8","16/8*4=8","104/13=8","136/17=8","144/18=8"]) :-
    puzzle_solve_all(["12+5-9=8"-gbbbbbgg],
                     "14/7*4=8",
                     Ps).

test(252, Ps == ["54*1/6=9","54/6*1=9","3/5*15=9","45*1/5=9","54/1/6=9","54/6/1=9","513/57=9","549/61=9","45/1/5=9","315/35=9","351/39=9","459/51=9","531/59=9"]) :-
    puzzle_solve_all(["12+5-8=9"-rbbrbbgg],
                     "54*1/6=9",
                     Ps).

test(253, % Ps == ["6*7-3=39","7*6-3=39","7*7-0=49","79-30=49","79-40=39","69-30=39","73-34=39","76-37=39","96-47=49","99-30=69","99-60=39","93-44=49"]) :-
     Ps == ["6*7-3=39","7*6-3=39","79-30=49","79-40=39","69-30=39","73-34=39","76-37=39","96-47=49","99-30=69","99-60=39","93-44=49"]) :-
    puzzle_solve_all(["12+5-8=9"-bbbbrbrg],
                     "79-40=39",
                     Ps).

test(254, Ps == ["49/7-1=6","49/7-6=1","30/3-9=1","36/9-1=3","36/9-3=1","40/4-9=1","60/6-9=1","63/9-1=6","63/9-6=1","70/7-9=1","91/7-6=7","91/7-7=6","91/7-9=4","90/9-9=1"]) :-
    puzzle_solve_all(["12+5-8=9"-rbbbgbgr],
                     "30/3-9=1",
                     Ps).

test(255, error(nerdle_assertion(fail,nerdle:verify_guess(['1','2',+,'3',-,'6',=,'0'])))) :- % Ps == ["15/5*2=6","18/6*2=6","1/2*12=6"]) :-
    puzzle_solve_all(["12+3-6=0"-grbbbrgb,
                      "1*42/7=6"-grbrrbgg],
                     "1/2*12=6",
                     _Ps).

test(256, Ps == ["98-71=27","98-77=21"]) :-
    puzzle_solve_all(["12+3-6=9"-rrbbrbrr,
                      "2*9-8=10"-rbrrrgrb,
                      "47-19=28"-brgrrggr],
                     "98-71=27",
                     Ps).

test(257, Ps == ["4*9-27=9","4*9-29=7","6*6-27=9","6*6-29=7","7*4-22=6","9*4-27=9","9*4-29=7","7*4-24=4","9*9-72=9"]) :-
    puzzle_solve_all(["5+7-0=12"-bbrgbrbr],
                     "4*9-27=9",
                     Ps).

test(258, Ps == ["3-8+14=9","4-8+13=9"]) :-
    puzzle_solve_all(["5+7-0=12"-brbrbrrb,
                      "13+4-8=9"-rrrrrrgg],
                     "3-8+14=9",
                     Ps).

test(259, Ps == ["5+3*8=29","5+4*6=29","5+6*4=29","5+8*3=29","5+2*9=23","5+3*6=23","5+6*3=23","5+9*2=23","5+4*5=25","5+5*4=25","5+9+9=23"]) :-
    puzzle_solve_all(["5+7-0=12"-ggbbbgbr],
                     "5+4*6=29",
                     Ps).
test(259, Ps == ["5+4*6=29","5+6*4=29"]) :-
    puzzle_solve_all(["5+7-0=12"-ggbbbgbr,
                      "5+3*8=29"-ggbgbggg],
                     "5+4*6=29",
                     Ps).


:- end_tests(nerdle24).

:- begin_tests(nerdle26).

test(260, Ps == ["24/6-1=3","24/6-3=1","21/3-1=6","21/3-3=4","21/3-4=3","21/3-6=1"]) :-
    puzzle_solve_all(["5+7-0=12"-bbbrbrrr,
                      "13-8/2=9"-rrrbrrgb],
                     "24/6-3=1",
                     Ps).

test(261, Ps == ["270/5=54","296/4=74","220/5=44","256/4=64","264/6=44"]) :-
    puzzle_solve_all(["3*8/1=24"-bbbgbgrg],
                     "270/5=54",
                     Ps).

test(262, Ps == ["4/10*5=2","4/20*5=1","5/10*4=2","5/20*4=1"]) :-
    puzzle_solve_all(["3*8/1=24"-brbrrrrr,
                      "1/4*20=5"-rgrrrrgr],
                     "4/20*5=1",
                     Ps).

test(263, Ps == ["136/68=2","166/83=2","186/62=3"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrbbrrb,
                      "1+56/7=9"-gbbrrbgb],
                     "186/62=3",
                     Ps).

test(264, Ps == ["65/5-6=7","77/7-5=6","77/7-6=5"]) :-
    puzzle_solve_all(["3*8-4=20"-bbbrbrbb,
                      "9-56/7=1"-brrrrrgb],
                     "77/7-6=5",
                     Ps).

test(265, Ps == ["56-18=38","51-18=33","53-18=35"]) :-
    puzzle_solve_all(["3*8-4=20"-rbrrbgbb,
                      "57-18=39"-gbgggggb],
                     "56-18=38",
                     Ps).

test(266, % Ps == ["9*2+0=18","9*1*2=18","9*2*1=18"]) :-
     Ps == ["9*1*2=18","9*2*1=18"]) :-
    puzzle_solve_all(["13-8/2=9"-rbbrbrrr,
                      "2*79=158"-rgbrrrbg],
                     "9*1*2=18",
                     Ps).

test(267, Ps == ["1/4*32=8","1/8*32=4","12/3*2=8","18/3+2=8","128/32=4","186/62=3"]) :-
    puzzle_solve_all(["13-8/2=9"-grbrrggb],
                     "1/8*32=4",
                     Ps).

test(268, Ps == ["97-10=87","97-17=80","98-11=87","98-17=81"]) :-
    puzzle_solve_all(["6+9-3=12"-bbrrbgrb,
                      "89-14=75"-rrggbgrb],
                     "98-11=87",
                     Ps).

test(269, Ps == ["104/8=13","3*5/1=15","3/1*5=15","5*3/1=15","1*3*5=15","3*1*5=15","3*5*1=15","5*3*1=15"]) :-
    puzzle_solve_all(["6+9-3=12"-bbbbrggb],
                     "1*3*5=15",
                     Ps).

test(270, Ps == ["95-33=62","95-63=32"]) :-
    puzzle_solve_all(["6+9-3=12"-rbrrggbg],
                     "95-33=62",
                     Ps).

test(271, % Ps == ["3+9-0=12","3+9-2=10","3-0+9=12","3-2+9=10","9+3-0=12","9+3-2=10","9-0+3=12","9-2+3=10"]) :-
     Ps == ["3+9-2=10","3-2+9=10","9+3-2=10","9-2+3=10"]) :-
    puzzle_solve_all(["12+3-6=9"-rrrrrbrr],
                     "3+9-2=10",
                     Ps).

test(272, % Ps == ["2*4+9=17","2*5+9=19","2+4+9=15","2+7+9=18","2*1+9=11","2/1+9=11","2+0+9=11","2+8+9=19","2+1+9=12"]) :-
     Ps == ["2*4+9=17","2*5+9=19","2+4+9=15","2+7+9=18","2*1+9=11","2/1+9=11","2+8+9=19","2+1+9=12"]) :-
    puzzle_solve_all(["6-2+9=13"-bbrggggb],
                     "2*1+9=11",
                     Ps).
test(272, Ps == ["2*5+9=19","2*1+9=11"]) :-
    puzzle_solve_all(["6-2+9=13"-bbrggggb,
                      "2*4+9=17"-ggbggggb],
                     "2*1+9=11",
                     Ps).
test(272, Ps == ["6-1+9=14","6-4+9=11"]) :-
    puzzle_solve_all(["12+3-6=9"-rbrbrrrr,
                      "6+9-5=10"-grrrbggb],
                     "6-1+9=14",
                     Ps).

test(273, Ps == ["180/90=2","184/92=2","188/94=2","190/95=2","194/97=2","728/91=8","198/99=2"]) :-
    puzzle_solve_all(["6-2+9=13"-bbrbgrrb],
                     "198/99=2",
                     Ps).

test(274, Ps == ["18/2-5=4","25-3*8=1","25-8*3=1","5-18/6=2","5-28/7=1","5-32/8=1","18-2*5=8","18-5*2=8","15-2-5=8","15-5-2=8","15-5-8=2","15-8-5=2","21-5-8=8","21-8-5=8"]) :-
    puzzle_solve_all(["8+9-2=15"-rbbrrrrr],
                     "18-2*5=8",
                     Ps).

test(275, Ps == ["26-3*8=2","27-3*8=3","28-3*7=7"]) :-
    puzzle_solve_all(["8+9-2=15"-rbbrrrbb,
                      "20-3*4=8"-gbgggbgr],
                     "27-3*8=3",
                     Ps).

test(276, Ps == ["4*5+2=22","5*4+2=22"]) :-
    puzzle_solve_all(["8+9-2=15"-brbbggbr,
                      "23+42=65"-rbrrggbr],
                     "5*4+2=22",
                     Ps).

test(277, Ps == ["42-20=22","44-20=24","44-24=20"]) :-
    puzzle_solve_all(["8+9-2=15"-bbbrrgbb,
                      "60-23=37"-brggbgbb],
                     "42-20=22",
                     Ps).

test(278, Ps == ["12*3/6=6","12*1/6=2"]) :-
    puzzle_solve_all(["8+9-2=15"-bbbbrrrb,
                      "1*42/6=7"-grbrgggb],
                     "12*3/6=6",
                     Ps).

test(279, Ps == ["8*3-1=23","8*3-3=21"]) :-
    puzzle_solve_all(["8+9-2=15"-gbbgrgrb],
                     "8*3-3=21",
                     Ps).
test(279, Ps == ["8*3-1=23","8*3-3=21"]) :-
    puzzle_solve_all(["8+9-2=15"-gbbgrgrb],
                     "8*3-1=23",
                     Ps).

:- end_tests(nerdle26).

:- begin_tests(nerdle28).

test(280, Ps == ["5-8+10=7","7-15+8=0"]) :-
    puzzle_solve_all(["8+9-2=15"-rrbrbrrr,
                      "10+3-5=8"-rrrbrrgr],
                     "5-8+10=7",
                     Ps).

test(281, Ps == ["37+49=86","39+47=86","30+48=78","33+47=80","36+38=74","37+43=80","38+36=74","38+40=78","34+44=78","37+47=84"]) :-
    puzzle_solve_all(["8+7-2=13"-rrrbbgbr, "4*8+5=37"-rbrrbgrr],
                     "39+47=86",
                     Ps).

test(282, Ps == ["4*9/1=36","4/1*9=36","5*6/1=30","5/1*6=30","6*5/1=30","6/1*5=30","9*4/1=36","9/1*4=36","156/4=39","1*4*9=36","1*5*6=30","1*6*5=30","1*9*4=36","4*1*9=36","4*9*1=36","5*1*6=30","5*6*1=30","6*1*5=30","6*5*1=30","9*1*4=36","9*4*1=36","140/4=35","195/3=65","305/5=61","306/6=51","364/4=91","369/9=41","6*6/1=36","6/1*6=36","105/3=35","135/3=45","136/4=34","144/4=36","150/3=50","150/5=30","165/3=55","195/5=39","315/9=35","351/9=39","531/9=59","1*6*6=36","6*1*6=36","6*6*1=36","153/3=51","155/5=31","366/6=61"]) :-
    puzzle_solve_all(["8+7-2=13"-bbbbbgrr],
                     "315/9=35",
                     Ps).

test(282, Ps == ["195/5=39","315/9=35","351/9=39"]) :-
    puzzle_solve_all(["8+7-2=13"-bbbbbgrr,
                      "4*9/1=36"-bbrgrggb],
                     "315/9=35",
                     Ps).

test(283, Ps == ["30-4*6=6","30-6*4=6","34-4*7=6","34-7*4=6","43-6*6=7","7*7-43=6","7*7-46=3","36-6*6=0","40-6*6=4","46-6*7=4","46-7*6=4","6*6-30=6","6*6-36=0","6*7-36=6","6-36/6=0","7*6-36=6","6*6-33=3"]) :-
    puzzle_solve_all(["5+12-8=9"-bbbbrbgb],
                     "30-4*6=6",
                     Ps).

test(284, Ps == ["236/59=4","392/56=7","460/92=5","259/37=7","90/5/9=2","90/9/2=5","90/9/5=2","245/49=5","295/59=5"]) :-
    puzzle_solve_all(["5+12-8=9"-rbbrbbgr],
                     "295/59=5",
                     Ps).

test(285, Ps == ["1+48/6=9","18/9+4=6","48/6+1=9","49/7-1=6","49/7-6=1","64/8+1=9","78/6-4=9","9-48/6=1","60/4-6=9","64/4-7=9","68/4-8=9","90/9-4=6","96/8-4=8","96/4/4=6"]) :-
    puzzle_solve_all(["236/59=4"-bbrrbrgr],
                     "49/7-6=1",
                     Ps).

test(286, Ps == ["100/5=20","105/5=21","110/5=22","125/5=25"]) :-
    puzzle_solve_all(["236/59=4"-rbbggbrb],
                     "110/5=22",
                     Ps).

test(297, Ps == ["472/8=59","592/8=74","752/8=94","270/5=54","280/4=70","280/7=40","288/4=72"]) :-
    puzzle_solve_all(["134/2=67"-bbrgrgbr],
                     "280/7=40",
                     Ps).
test(297, Ps == ["280/4=70","280/7=40","288/4=72"]) :-
    puzzle_solve_all(["134/2=67"-bbrgrgbr,
                      "472/8=59"-rrrgrgbb],
                     "280/7=40",
                     Ps).

test(298, Ps == ["308/44=7","378/54=7","343/49=7"]) :-
    puzzle_solve_all(["134/2=67"-brrgbrbg],
                     "343/49=7",
                     Ps).

test(299, Ps == ["63+30=93","63+33=96"]) :-
    puzzle_solve_all(["134/2=67"-bgbbbgrb,
                      "33+56=89"-rggbrgbr],
                     "63+30=93",
                     Ps).

:- end_tests(nerdle28).

:- begin_tests(nerdle30).

test(300, Ps == ["3*54=162","54*3=162"]) :-
    puzzle_solve_all(["134/2=67"-rrrbrrgb],
                     "3*54=162",
                     Ps).

test(301, Ps == ["14-5*2=4","12-4*2=4"]) :-
    puzzle_solve_all(["134/2=67"-gbrbrrbb,
                      "10+2-4=8"-gbbrrrgb],
                     "14-5*2=4",
                     Ps).

test(302, Ps == ["8*4-30=2","8*4-32=0","9*4-32=4","9*4-34=2","8*40=320","8*44=352","8*49=392","9*48=432"]) :-
    puzzle_solve_all(["134/2=67"-brgbrrbb],
                     "9*4-32=4",
                     Ps).

test(303, Ps == ["3*18/6=9","3*16/6=8"]) :-
    puzzle_solve_all(["134/2=67"-rrbrbrrb,
                      "3*10/5=6"-gggbgbgr],
                     "3*18/6=9",
                     Ps).

test(304, Ps == ["7*98=686","8*87=696","87*8=696","98*7=686"]) :-
    puzzle_solve_all(["134/2=67"-bbbbbrrr,
                      "50-6*7=8"-bbbrrrrr],
                     "8*87=696",
                     Ps).

test(305, Ps == ["7*4-6=22","2*4*7=56"]) :-
    puzzle_solve_all(["134/2=67"-bbgbrgrr],
                     "7*4-6=22",
                     Ps).

test(306, Ps == ["35-17=18","55-17=38","55-37=18","71-13=58","71-53=18","71-33=38"]) :-
    puzzle_solve_all(["134/2=67"-rrbbbgbr,
                      "8*9+1=73"-rbbbrgrr],
                     "55-17=38",
                     Ps).

test(307, Ps == ["304/38=8","344/43=8","384/48=8"]) :-
    puzzle_solve_all(["134/2=67"-brggbrbb],
                     "384/48=8",
                     Ps).

test(308, Ps == ["1*2+8=10","1*8+2=10","1+9*9=82","1*2+9=11","1*6+6=12","1*9+2=11","1+2*9=19","1+9*2=19","1+2+8=11","1+2+9=12","1+8+2=11","1+9+2=12"]) :-
    puzzle_solve_all(["12+35=47"-grrbbgbb],
                     "1*2+9=11",
                     Ps).

test(309, Ps == ["152/19=8","15/5-1=2"]) :-
    puzzle_solve_all(["12+35=47"-grbbrrbb,
                      "1*10/2=5"-gbrbrrgr],
                     "152/19=8",
                     Ps).

test(310, Ps == ["1+3*6=19","1+6*3=19","1*3+8=11","1*8+3=11","1+3*3=10","1+3+6=10","1+6+3=10","1+3+9=13","1+6+6=13","1+9+3=13"]) :-
    puzzle_solve_all(["12+35=47"-gbrrbgbb],
                     "1+3*3=10",
                     Ps).

test(311, Ps == ["7*9-54=9","7*9-59=4","9*7-54=9","9*7-59=4","456/57=8"]) :-
    puzzle_solve_all(["12+35=47"-bbbbgrrr],
                     "7*9-54=9",
                     Ps).

test(312, Ps == ["51-30=21","51-31=20"]) :-
    puzzle_solve_all(["12+35=47"-rrbgrgbb,
                      "50-32=18"-grggrgrb],
                     "51-30=21",
                     Ps).

test(313, Ps == ["126/42=3","128/32=4","12/1/4=3","123/41=3","124/31=4"]) :-
    puzzle_solve_all(["12+35=47"-ggbrbrrb,
                      "12-4*3=0"-ggbrbrgb],
                     "126/42=3",
                     Ps).

test(314, Ps == ["8+8-11=5","8+8-15=1"]) :-
    puzzle_solve_all(["12+35=47"-rbrbrrbb,
                      "5+10-6=9"-rgrbrbgb],
                    "8+8-11=5",
                     Ps).

test(315, Ps == ["32*1/4=8","32*1/8=4","32/4*1=8","32/8*1=4","32/1/4=8","32/1/8=4","32/4/1=8","32/4/8=1","32/8/1=4","32/8/4=1","32*4=128","324/81=4","328/41=8"]) :-
    puzzle_solve_all(["12+35=47"-rgbrbrrb],
                     "32/8/4=1",
                     Ps).

test(316, Ps == ["6*9/3=18","6*3/1=18","6*8/3=16","6*1*3=18","6*3*1=18"]) :-
    puzzle_solve_all(["12+35=47"-rbbrbgbb,
                      "3*6-8=10"-rgrbrggb],
                     "6*8/3=16",
                     Ps).

test(317, Ps == ["70/2/7=5","70/5/7=2","70/7/2=5","70/7/5=2"]) :-
    puzzle_solve_all(["12+35=47"-brbbrrbr,
                      "5*7-26=9"-rbrbrbgb],
                     "70/2/7=5",
                     Ps).

test(318, Ps == ["96-43=53","99-46=53"]) :-
    puzzle_solve_all(["12+35=47"-bbbrrgrb,
                      "5*8-4=36"-rbbrrgrr],
                     "99-46=53",
                     Ps).

test(319, Ps == ["261/87=3","27/1/9=3","27/9/1=3","273/91=3","279/31=9","291/97=3","231/77=3"]) :-
    puzzle_solve_all(["12+35=47"-rrbrbrbr,
                      "21-7*3=0"-grbrbrgb],
                     "231/77=3",
                     Ps).
test(319, Ps == ["291/97=3","231/77=3"]) :-
    puzzle_solve_all(["12+35=47"-rrbrbrbr,
                      "21-7*3=0"-grbrbrgb,
                      "261/87=3"-gbggbggg],
                     "231/77=3",
                     Ps).

:- end_tests(nerdle30).

:- begin_tests(nerdle32).

test(320, Ps == ["5*5-4=21","1*5*4=20","1*5*8=40","5*1*4=20","5*1*8=40"]) :-
    puzzle_solve_all(["3*4+5=17"-bgrbrgrb,
                      "6*9/1=54"-bgbbrgrr],
                     "1*5*8=40",
                     Ps).

test(321, Ps == ["6+1+6=13","6+6+1=13"]) :-
    puzzle_solve_all(["3*4+5=17"-rbbgbggb,
                      "6-2+9=13"-gbbgbggg],
                     "6+6+1=13",
                     Ps).

test(322, Ps == ["90-54=36","69-34=35","99-34=65","99-64=35"]) :-
    puzzle_solve_all(["3*4+5=17"-rbrbrgbb,
                      "236/4=59"-brrbggrr],
                     "99-34=65",
                     Ps).

test(323, Ps == ["2+9-10=1","2+9-11=0","9+2-10=1","9+2-11=0"]) :-
    puzzle_solve_all(["3*4+5=17"-bbbrbrrb,
                      "1+10-2=9"-rgrrrrgr],
                     "2+9-10=1",
                     Ps).

test(324, Ps == ["9*4/3=12","4*4-3=13","1*4*3=12"]) :-
    puzzle_solve_all(["3*4+5=17"-rggbbggb],
                     "4*4-3=13",
                     Ps).

test(325, Ps == ["23-6-9=8","23-8-6=9","23-8-9=6","23-9-6=8"]) :-
    puzzle_solve_all(["3*4+5=17"-rbbbbrbb,
                      "6-32/8=2"-rrrrbrgb],
                     "23-6-9=8",
                     Ps).

test(326, Ps == ["50+13=63","53+10=63"]) :-
    puzzle_solve_all(["3*4+5=17"-rbbrrgrb,
                      "12+38=50"-rbgrbgrr],
                     "53+10=63",
                     Ps).

test(327, Ps == ["3+2*6=15","3+6*2=15","3+5*2=13","3+5*3=18"]) :-
    puzzle_solve_all(["3*4+5=17"-grbrrggb],
                     "3+2*6=15",
                     Ps).

test(328, Ps == ["168/24=7","128/64=2","148/74=2","168/84=2","188/94=2","192/48=4","112/28=4"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrrb,
                      "10-4/2=8"-gbbrrrgr],
                     "112/28=4",
                     Ps).

test(329, Ps == ["68-23=45","68-43=25","86-52=34","86-54=32","56-22=34","56-24=32","56-32=24","56-34=22","65-23=42","65-43=22"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbrgbr,
                      "342/6=57"-rrrbrgrb],
                     "56-34=22",
                     Ps).
test(329, Ps == ["56-32=24","56-34=22"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbrgbr,
                      "342/6=57"-rrrbrgrb,
                      "68-23=45"-rbgrrgrr],
                     "56-34=22",
                     Ps).

test(330, Ps == ["18-7-2=9","18-9-2=7","17-7-2=8","19-9-2=8","11-1-2=8","12-2-2=8"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbbrrb,
                      "10-8/2=6"-gbgrbggb],
                     "12-2-2=8",
                     Ps).

test(331, Ps == ["94-83=11","44-33=11"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbggr,
                      "60-43=17"-bbgrgggb],
                     "44-33=11",
                     Ps).

test(332, Ps == ["16-4-5=7","15-4-4=7"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrrb,
                      "40/5-1=7"-rbbrgrgg],
                     "15-4-4=7",
                     Ps).

test(333, Ps == ["20*1/4=5","20*1/5=4"]) :-
    puzzle_solve_all(["2*4+5=13"-grrbrrrb,
                      "24*5=120"-grgrrrbr],
                     "20*1/5=4",
                     Ps).

test(334, Ps == ["3+20/5=7","30/5+2=8","32/8+5=9","5+32/8=9","3+25/5=8","35/5+2=9","35/7+2=7"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrrbr],
                     "32/8+5=9",
                     Ps).

test(335, Ps == ["10+24=34","13+14=27","13+24=37","14+13=27","14+20=34","14+23=37","11+13=24","11+23=34","13+11=24","13+21=34"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrrbgrr,
                      "12+36=48"-grgrbgrb],
                     "13+14=27",
                     Ps).

test(336, Ps == ["48/4-5=7","48/4-7=5","45/5-4=5","45/5-5=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrbb,
                      "54/6-9=0"-rrgbgbgb],
                     "45/5-4=5",
                     Ps).

test(337, Ps == ["49+35=84","64+35=99"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrggbr,
                      "30+45=75"-rbgrggbb],
                     "49+35=84",
                     Ps).

test(338, Ps == ["12-4-1=7","12-4-6=2","12-4-7=1","12-4-4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrrb,
                      "10-4/2=8"-gbggbrgb],
                     "12-4-6=2",
                     Ps).
test(338, Ps == ["12-4-6=2","12-4-4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrrb,
                      "10-4/2=8"-gbggbrgb,
                      "12-4-1=7"-gggggbgb],
                     "12-4-6=2",
                     Ps).

test(338, Ps == ["30/6+1=6","36/6+1=7"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbrrr,
                      "1+63/9=8"-rrrrrbgb],
                     "36/6+1=7",
                     Ps).

test(339, Ps == ["3-6+10=7","3-7+10=6","1-3+10=8","1-6+13=8","1-8+13=6","3-6+11=8","3-7+13=9","3-8+11=6","3-9+13=7","1-3+11=9","1-7+13=7","3-7+11=7","3-8+13=8"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbgbrrr],
                     "3-8+11=6",
                     Ps).

:- end_tests(nerdle32).

:- begin_tests(nerdle34).

test(340, Ps == ["87-30=57","87-37=50","87-50=37","87-57=30","80-30=50","80-50=30","83-33=50","83-53=30","88-30=58","88-38=50","88-50=38","88-58=30"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrgbr,
                      "390/6=65"-rbrbbgbr],
                     "88-58=30",
                     Ps).
test(340, Ps == ["83-53=30","88-58=30"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrgbr,
                      "390/6=65"-rbrbbgbr,
                      "87-30=57"-gbgrrgrb],
                     "88-58=30",
                     Ps).

test(341, Ps == ["172/86=2","176/88=2","162/27=6"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbbrrb,
                      "10-6/2=7"-gbbrrrgr],
                     "172/86=2",
                     Ps).

test(342, Ps == ["42/3-7=7","42/6-3=4"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrbr,
                      "3-24/8=0"-rrrrrbgb],
                     "42/3-7=7",
                     Ps).

test(343, Ps == ["6*8/24=2","8*6/24=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbrbb,
                      "4*7-20=8"-rgbbgbgr],
                     "6*8/24=2",
                     Ps).

test(344, Ps == ["16-4*2=8","14-4*2=6","14-6*2=2","16-6*2=4"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrbbrrb,
                      "1/6*42=7"-gbrrrggb],
                     "16-6*2=4",
                     Ps).

test(345, Ps == ["7+2+5=14","4+2+5=11"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrggggb],
                     "4+2+5=11",
                     Ps).

test(346, Ps == ["185/37=5","150/30=5","175/35=5","355/71=5","155/31=5"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrrrr,
                      "5-36/9=1"-rbrbrbgr],
                     "185/37=5",
                     Ps).

test(347, Ps == ["8*4+8=40","9*4+4=40","9*4+8=44"]) :-
    puzzle_solve_all(["2*4+5=13"-bgggbgbb],
                     "9*4+4=40",
                     Ps).

test(348, Ps == ["13-3-7=3","13-7-3=3","17-7-7=3"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbbrrg,
                      "81/9-6=3"-brbbgbgg],
                     "13-3-7=3",
                     Ps).

test(349, Ps == ["6*3-10=8","6*3-18=0","3*8-16=8","3*8-18=6","8*3-16=8","8*3-18=6","8*8-63=1"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbbrrr,
                      "1*63/7=9"-rgrrbbgb],
                     "3*8-16=8",
                     Ps).

test(350, Ps == ["85-9*9=4","45-4*9=9","49-5*9=4"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrbb,
                      "54-6*9=0"-rrgbgggb],
                     "85-9*9=4",
                     Ps).

test(351, Ps == ["21-6-6=9","21-6-9=6"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbbrrb,
                      "2-16/8=0"-grrgbbgb],
                     "21-6-9=6",
                     Ps).

test(352, Ps == ["15-2-7=6","15-6-2=7","15-7-2=6","16-2-5=9","16-2-9=5","16-5-2=9","16-9-2=5","12-1-5=6","12-5-1=6","12-6-1=5","12-6-5=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrrb,
                      "5-18/6=2"-rrrbbrgr],
                     "16-5-2=9",
                     Ps).
test(352, Ps == ["16-5-2=9","16-9-2=5","12-6-1=5","12-6-5=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrrb,
                      "5-18/6=2"-rrrbbrgr,
                      "15-2-7=6"-grgrgbgr],
                     "16-5-2=9",
                     Ps).

test(353, Ps == ["12+4-7=9","12+4-9=7","14+2-7=9","14+2-9=7","14-7+2=9","14-9+2=7","11+2-9=4","11-4+2=9","11-9+2=4","12+1-9=4","12+4-8=8","12-4+1=9","12-9+1=4","14+2-8=8","14-8+2=8"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrrbrrb,
                      "1+20/4=6"-grrbbrgb],
                     "11-9+2=4",
                     Ps).

test(354, Ps == ["9-1+9=17","9-7+9=11"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbgbggb,
                      "7-6+9=10"-rgbggggb],
                     "9-1+9=17",
                     Ps).

test(356, Ps == ["2*2*7=28","2*7*2=28"]) :-
    puzzle_solve_all(["2*4+5=13"-ggbbbgbb,
                      "2*6*8=96"-ggbgrgbb],
                     "2*2*7=28",
                     Ps).

test(357, Ps == ["4+2*3=10","4+3*2=10","8+2*3=14","8+3*2=14","3+2*4=11"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrrbggr],
                     "4+2*3=10",
                     Ps).

test(358, Ps == ["9*9-74=7","9*9-77=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrbbrbb,
                      "6*8-40=8"-bgbgrbgb],
                     "9*9-74=7",
                     Ps).

test(359, Ps == ["426/71=6","427/61=7"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrrb,
                      "10-4/2=8"-rbbrrrgb],
                     "427/61=7",
                     Ps).

:- end_tests(nerdle34).
:- begin_tests(nerdle36).

test(360, Ps == ["6*9-50=4","6*9-54=0","9*6-50=4","9*6-54=0","7*9-54=9","7*9-59=4","9*7-54=9","9*7-59=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrbgrbb],
                     "6*9-50=4",
                     Ps).

test(361, Ps == ["22+18=40","28+12=40","28+21=49","24+18=42","28+14=42"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrrbgrb,
                      "21+47=68"-grgrbgbr],
                     "22+18=40",
                     Ps).

test(362, Ps == ["39-5*6=9","35-5*6=5"]) :-
    puzzle_solve_all(["2*4+5=13"-brbbrrbr,
                      "76*5=380"-brrgrrbb],
                     "35-5*6=5",
                     Ps).

test(363, Ps == ["5*9-7=38","5*9-8=37","9*5-7=38","9*5-8=37","5*8-3=37","5*9-6=39","5*9-9=36","6*7-7=35","7*6-7=35","8*5-3=37","9*5-6=39","9*5-9=36","3*5*6=90","5*3*6=90","5*6*3=90","6*5*3=90","5*5*3=75"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbrgbr],
                     "5*9-7=38",
                     Ps).

test(364, Ps == ["5-8+12=9","5-9+12=8","1-5+12=8","1-8+12=5"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbgrrrb],
                     "5-8+12=9",
                     Ps).

test(365, Ps == ["4*7-2=26","4*7-6=22","4*7-4=24"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbgbb,
                      "4*7-8=20"-ggggbggb],
                     "4*7-6=22",
                     Ps).

test(366, Ps == ["2+56/8=9","27/9+5=8","28/7+5=9","2+20/5=6","2+27/9=5","20/5+2=6","20/5+5=9","27/9+2=5","2+25/5=7","25/5+2=7"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbrrrbb],
                     "2+25/5=7",
                     Ps).

test(367, Ps == ["230/5=46","235/5=47"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrbggbr],
                     "230/5=46",
                     Ps).

test(368, Ps == ["80-21=59","80-29=51","80-51=29","80-59=21","50-21=29","50-29=21"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgrb,
                      "102-7=95"-rgrrbgrr],
                     "80-29=51",
                     Ps).

test(369, Ps == ["504/6=84","504/9=56","464/8=58","544/8=68","564/6=94","594/6=99","594/9=66"]) :-
    puzzle_solve_all(["2*4+5=13"-bbgbrgbb],
                     "594/6=99",
                     Ps).

test(370, Ps == ["96-23=73","99-26=73"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbbgbg,
                      "90-27=63"-gbggrgrg],
                     "96-23=73",
                     Ps).

test(371, Ps == ["270/45=6","20/4-5=0","20/5-4=0","24/2-5=7","24/2-7=5","28/4-2=5","28/4-5=2","240/48=5","260/65=4","200/40=5","220/44=5","245/49=5","252/42=6","256/64=4","225/45=5"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrbrrbb],
                     "28/4-5=2",
                     Ps).

% TODO: improve the order of results? (fewer duplicate digits)
test(372, Ps == ["5*5-5=20","5*6-5=25","6*5-5=25","6*2*5=60","7*2*5=70","8*2*5=80","9*2*5=90","5*2*5=50"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbggbb],
                     "8*2*5=80",
                     Ps).
test(372, Ps == ["6*2*5=60","8*2*5=80","9*2*5=90","5*2*5=50"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbggbb,
                      "7*2*5=70"-bgggggbg],
                     "8*2*5=80",
                     Ps).
test(372, Ps == ["8*2*5=80","9*2*5=90","5*2*5=50"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbggbb,
                      "7*2*5=70"-bgggggbg,
                      "6*2*5=60"-bgggggbg],
                     "8*2*5=80",
                     Ps).

test(373, Ps == ["340/5=68","430/5=86","345/5=69","370/5=74","435/5=87","69-35=34","74-35=39","75-35=40","75-45=30","79-35=44","79-45=34","80-35=45","80-45=35","84-35=49","84-45=39","89-35=54","89-55=34","93-45=48","99-35=64","99-65=34","83-35=48","83-45=38","94-35=59","94-55=39"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbggbr],
                     "83-35=48",
                     Ps).
test(373, Ps == ["93-45=48","83-35=48","83-45=38"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbggbr,
                      "340/5=68"-rrbbggbg],
                     "83-35=48",
                     Ps).

test(374, Ps == ["2-7+14=9","2-8+10=4","2-9+14=7","2-8+14=8","2-9+11=4"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrgbrrb],
                     "2-7+14=9",
                     Ps).

test(375, Ps == ["6*8-5=43","8*6-5=43"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrbggbg],
                     "6*8-5=43",
                     Ps).

test(376, Ps == ["2/10*5=1","2/5*15=6","2/6*15=5","21-2*8=5","21-8*2=5","26-5*5=1","25*6=150","25*1/5=5","25/5*1=5","20*5=100","21*5=105","25*7=175","26*6=156","25*5=125"]) :-
    puzzle_solve_all(["2*4+5=13"-grbbrrrb],
                     "25/5*1=5",
                     Ps).

test(377, Ps == ["84/7-9=3","40/4-7=3","48/4-9=3","49/7-4=3","63/9-4=3","96/4/8=3","96/8/4=3","36/4-6=3","44/4-8=3","48/8-3=3","84/4/7=3","84/7/4=3","36/3/4=3","36/4/3=3","48/4/4=3"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbrbg],
                     "63/9-4=3",
                     Ps).

test(378, Ps == ["3-8+14=9","3-9+10=4","3-9+14=8","4-8+13=9","4-9+13=8"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrgbrrr],
                     "3-9+14=8",
                     Ps).

test(379, Ps == ["69*4=276","92*7=644","42*7=294"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrbbrbb,
                      "74-8*9=2"-rrbbrrrr],
                     "69*4=276",
                     Ps).

:- end_tests(nerdle36).
:- begin_tests(nerdle38).

test(380, Ps == ["3*8/1=24","4*6-3=21","4*8/1=32","8*3/1=24","1*3*8=24","1*8*3=24","1*8*4=32","3*1*8=24","3*8*1=24","4*1*8=32","4*8*1=32","8*1*3=24","8*1*4=32","8*3*1=24"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbgrr],
                     "1*3*8=24",
                     Ps).

test(381, Ps == ["3*7+5=26","3*8+5=29","7*3+5=26","8*3+5=29","3*5+5=20","3*9+5=32","5*3+5=20","9*3+5=32"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbgggbr],
                     "3*8+5=29",
                     Ps).
test(381, Ps == ["3*8+5=29","3*5+5=20"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbgggbr,
                      "3*7+5=26"-ggbggggb],
                     "3*8+5=29",
                     Ps).

test(382, Ps == ["304/76=4","384/96=4","744/93=8","304/38=8","344/86=4","384/64=6","664/83=8","344/43=8","384/48=8"]) :-
    puzzle_solve_all(["2*4+5=13"-bbgbbrbr],
                     "744/93=8",
                     Ps).

test(383, Ps == ["1+7+4=12","6+6+2=14","7+1+4=12"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrgbggb,
                      "4-2+8=10"-rbrgbggb],
                     "6+6+2=14",
                     Ps).

test(384, Ps == ["3+6*8=51","3+8*6=51"]) :-
    puzzle_solve_all(["2*4+5=13"-brbrrgrr,
                      "1+5*7=36"-rgrgbgrr],
                     "3+6*8=51",
                     Ps).
test(384, Ps == ["1+5*7=36","3+6*8=51","3+8*6=51","1+5*6=31"]) :-
    puzzle_solve_all(["2*4+5=13"-brbrrgrr],
                     "3+6*8=51",
                     Ps).

test(385, Ps == ["38-13=25","52-17=35"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgrr,
                      "130/2=65"-rrbbrgbg],
                     "38-13=25",
                     Ps).

test(386, Ps == ["4+4*9=40","8+4*8=40","8+4*9=44"]) :-
    puzzle_solve_all(["2*4+5=13"-brgrbgbb],
                     "8+4*8=40",
                     Ps).

test(387, Ps == ["5*7+8=43","7*5+8=43","5*8+3=43","7*7+4=53","8*5+3=43"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrgrgbg],
                     "7*5+8=43",
                     Ps).
test(387, Ps == ["5*7+8=43","7*5+8=43","5*8+3=43","7*7+4=53","8*5+3=43"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrgrgbg],
                     "5*7+8=43", % Note: different answer from above
                     Ps).

test(388, Ps == ["54*1/6=9","54*1/9=6","54/6*1=9","54/9*1=6","45*1/5=9","45*1/9=5","45/5*1=9","45/9*1=5"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrrb,
                      "1/5*40=8"-rrrrrbgb],
                     "45*1/5=9",
                     Ps).

test(389, Ps == ["3*6-10=8","3*6-18=0"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbbrrr,
                      "1*63/7=9"-rggrbbgb],
                     "3*6-10=8",
                     Ps).

test(390, Ps == ["56-26=30","56-36=20","58-26=32","58-36=22","63-28=35","63-38=25","85-23=62","85-63=22","88-23=65","88-56=32","88-63=25","65-33=32"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgbr,
                      "392/7=56"-rbrbbgrr],
                     "56-36=20",
                     Ps).

test(391, Ps == ["72-31=41","72-41=31","73-31=42","73-32=41","73-41=32","73-42=31"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbgrr,
                      "192/3=64"-rbrbrgbr,
                      "34-14=20"-rrgrbgrb],
                     "73-41=32",
                     Ps).

test(392, Ps == ["1*4+8=12","4*4+2=18"]) :-
    puzzle_solve_all(["2*4+5=13"-rgggbggb],
                     "4*4+2=18",
                     Ps).

test(393, Ps == ["5+9-4=10","9+5-4=10","5+9/1=14","9+5/1=14"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrrggb],
                     "5+9-4=10",
                     Ps).

test(394, Ps == ["88-29=59","88-59=29"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgbb,
                      "582/6=97"-rgrbbgrb],
                     "88-59=29",
                     Ps).

test(395, Ps == ["200/8=25","252/9=28","225/9=25"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbrgbb],
                     "252/9=28",
                     Ps).

test(396, Ps == ["70-27=43","72-29=43"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbgbg,
                      "87-24=63"-brggrgbg],
                     "70-27=43",
                     Ps).

test(397, Ps == ["56/4-6=8","56/4-7=7","56/4-8=6","56/7-4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrbb,
                      "54/6-9=0"-grgrgbgb],
                     "56/4-8=6",
                     Ps).

test(398, Ps == ["78-32=46","78-42=36","87-23=64","87-63=24","62-28=34","62-38=24","64-26=38","64-28=36","64-36=28","64-38=26","82-36=46","82-46=36"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbgbr,
                      "368/4=92"-rrrbrgbr],
                     "82-46=36",
                     Ps).
test(398, Ps == ["64-28=36","82-46=36"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbgbr,
                      "368/4=92"-rrrbrgbr,
                      "78-32=46"-brgrrgrg],
                     "82-46=36",
                     Ps).

test(399, Ps == ["4-6+10=8","4-8+10=6","1-6+14=9","1-7+10=4","1-7+14=8","1-8+14=7","1-9+14=6","4-6+11=9","4-7+10=7","4-7+11=8","4-8+11=7","4-9+11=6","1-8+11=4","4-9+14=9"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrgbrrb],
                     "1-7+14=8",
                     Ps).

:- end_tests(nerdle38).
:- begin_tests(nerdle40).

test(400, Ps == ["3*5+6=21","3*8+1=25","5*3+6=21","8*3+1=25"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbgrgrr],
                     "8*3+1=25",
                     Ps).

test(401, Ps == ["66+17=83","67+16=83","76+17=93","77+16=93"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbgrg,
                      "10+63=73"-rbgrbgrg],
                     "67+16=83",
                     Ps).

test(402, Ps == ["104/2=52","114/2=57","154/2=77","154/7=22"]) :-
    puzzle_solve_all(["2*4+5=13"-rbgbrgrb],
                     "114/2=57",
                     Ps).

test(403, Ps == ["60-14=46","60-16=44"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbgrb,
                      "67-18=49"-gbggbggb],
                     "60-16=44",
                     Ps).

test(404, Ps == ["4*5-2=18","4*5-8=12","4*5/2=10"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbrggb],
                     "4*5-2=18",
                     Ps).

test(405, Ps == ["72/8-4=5","72/8-5=4","52/4-4=9","52/4-5=8","52/4-8=5","52/4-9=4","54/9-4=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbrrbb,
                      "40/5-2=6"-rbgrgrgb],
                     "72/8-4=5",
                     Ps).

test(406, Ps == ["10*4/5=8","10/5*4=8"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrrb,
                      "1/5*40=8"-grrrrrgg],
                     "10*4/5=8",
                     Ps).

test(407, Ps == ["56/4-6=8","56/4-7=7","56/4-8=6","56/7-4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrbb,
                      "54/6-9=0"-grgrgbgb],
                     "56/4-6=8",
                     Ps).

test(408, Ps == ["10+35=45","13+35=48","14+35=49","19+15=34","19+35=54","43+15=58"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrggrr,
                      "31+45=76"-rrgrggbb],
                     "13+35=48",
                     Ps).
test(408, Ps == ["13+35=48","14+35=49"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrggrr,
                      "31+45=76"-rrgrggbb,
                      "10+35=45"-gbgggggb],
                     "13+35=48",
                     Ps).

test(409, Ps == ["8*37=296","8*79=632","9*82=738"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbbrbr,
                      "3*9-20=7"-rgrbrbrr],
                     "9*82=738",
                     Ps).

test(410, Ps == ["65-8*7=9","68-9*7=5"]) :-
    puzzle_solve_all(["2*4+5=13"-brbbrrbb,
                      "50-6*7=8"-rbgrgggr],
                     "65-8*7=9",
                     Ps).

test(411, Ps == ["3/12*8=2","8/12*3=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbrrr,
                      "1/3*27=9"-rgrrrbgb],
                     "3/12*8=2",
                     Ps).

test(412, Ps == ["3*3+6=15","3*5+1=16","3*5+3=18","5*3+1=16","5*3+3=18"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbgrggr],
                     "5*3+1=16",
                     Ps).

test(413, Ps == ["15+72=87","15+12=27"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrgrb,
                      "12+57=69"-grgrrgbb],
                     "15+12=27",
                     Ps).

test(414, Ps == ["12+3-6=9"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrbrrr,
                      "10+2-3=9"-gbgrgrgg],
                     "12+3-6=9",
                     Ps).

test(415, Ps == ["3*7+9=30","3*8+6=30","7*3+9=30","7*9+3=66","8*3+6=30","8*8+3=67","9*7+3=66","3*9+3=30","3*9+9=36","6*6+3=39","9*3+3=30","9*3+9=36"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbgbgbr],
                     "9*3+3=30",
                     Ps).

test(416, Ps == ["12*2/8=3","12/8*2=3"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbrrg,
                      "1/9*27=3"-grbrrbgg],
                     "12*2/8=3",
                     Ps).

test(417, Ps == ["19-5-7=7","19-7-5=7","15-1-7=7","15-7-1=7","15-7-7=1","17-5-5=7"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrrrb,
                      "7-16/8=5"-rrrbbbgr],
                     "19-5-7=7",
                     Ps).

test(418, Ps == ["20-5-8=7","20-7-8=5","20-8-7=5","25-8-8=9"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbrrbb, "26/2-5=8"-gbbbgrgr],
                     "25-8-8=9",
                     Ps).

test(419, Ps == ["4+1+6=11","4+1+9=14"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrgbggb,
                      "4-1+7=10"-gbggbggb],
                     "4+1+9=14",
                     Ps).

:- begin_tests(nerdle42).

test(420, Ps == ["24+68=92","26+68=94","28+64=92","28+66=94","22+26=48","22+62=84","22+64=86","24+62=86","24+64=88","26+22=48","22+24=46","24+22=46"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrrbgbb,
                      "20+47=67"-gbgrbgrb],
                     "22+26=48",
                     Ps).
test(420, Ps == ["22+26=48","26+22=48"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrrbgbb, "20+47=67"-gbgrbgrb,
                      "24+68=92"-grgrrgbr],
                     "22+26=48",
                     Ps).

test(421, Ps == ["8+9-11=6","8+9-16=1","8-11+9=6","9+8-11=6","9+8-16=1","9-11+8=6"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbrrb,
                      "10+6-7=9"-rbrrrbgr],
                     "9+8-16=1",
                     Ps).

test(422, Ps == ["6+9-2=13","7+8-2=13","8+7-2=13","9+6-2=13","9+8/2=13"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrbggg],
                     "7+8-2=13",
                     Ps).
test(422, Ps == ["7+8-2=13","8+7-2=13"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrbggg,
                      "6+9-2=13"-bgbggggg],
                     "7+8-2=13",
                     Ps).

test(423, Ps == ["86-39=47","86-49=37"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbgbr,
                      "483/7=69"-rrrbrgrr],
                     "86-49=37",
                     Ps).

test(424, Ps == ["90*2=180","98*2=196","91*2=182","99*2=198"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbrrb,
                      "1/8*72=9"-rbrrbrrr],
                     "91*2=182",
                     Ps).

test(425, Ps == ["72/9-2=6","72/9-6=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbbrbb,
                      "72/8-9=0"-gggbgrgb],
                     "72/9-2=6",
                     Ps).

test(426, Ps == ["10/2-1=4","10/2-4=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrrb,
                      "10-4/2=8"-ggrrrrgb],
                     "10/2-1=4",
                     Ps).

test(427, Ps == ["48+29=77","49+28=77"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrrbgbb,
                      "40+27=67"-gbggrgbg],
                     "48+29=77",
                     Ps).

test(428, Ps == ["7*8+3=59","8*7+3=59"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbgrgbr,
                      "6*9+3=57"-bgrggggr],
                     "7*8+3=59",
                     Ps).

test(429, Ps == ["14-3-5=6","14-3-6=5","14-5-3=6","14-6-3=5","13-4-4=5"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrrr,
                      "35/7-1=4"-rrbbgrgr],
                     "14-3-5=6",
                     Ps).

test(430, Ps == ["31-4*6=7","43-7*6=1"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbbrrr,
                      "1/9*36=4"-rbbrrggr],
                     "43-7*6=1",
                     Ps).

test(431, Ps == ["2*24/6=8","2*24/8=6"]) :-
    puzzle_solve_all(["2*4+5=13"-ggrbbrbb],
                     "2*24/8=6",
                     Ps).

test(432, Ps == ["36/4-2=7","36/4-7=2","32/4-2=6","32/4-6=2","32/4-4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbbrbr,
                      "3-24/8=0"-grrgrbgb],
                     "32/4-4=4",
                     Ps).

test(433, Ps == ["80/2/5=8","80/5/2=8","80/5/8=2","80/8/5=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrbb,
                      "8-27/9=5"-gbrbgbgr],
                     "80/2/5=8",
                     Ps).

test(434, Ps == ["4*3*7=84","4*7*3=84","4*3*4=48"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrbbgbr,
                      "4*9-6=30"-ggbbbgrb],
                     "4*3*7=84",
                     Ps).

test(435, Ps == ["16-2*8=0","10*1-8=2","10-1*8=2","10-2*1=8","10-8*1=2","18-2*6=6","18-2*8=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbrrb,
                      "1/8*72=9"-gbrrbrgb],
                     "10-8*1=2",
                     Ps).

test(436, Ps == ["5+8*3=29","5+5*3=20"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbrrgbr,
                      "5+3*7=26"-ggrgbggb],
                     "5+5*3=20",
                     Ps).

test(437, Ps == ["75+20=95","75+22=97"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrgbb,
                      "57+29=86"-rrggrgbb],
                     "75+22=97",
                     Ps).

test(438, Ps == ["30*7=210","39*8=312","70*3=210","72*3=216","73*3=219","31*7=217","36*6=216"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbrgr],
                     "30*7=210",
                     Ps).

test(439, Ps == ["82-65=17","97-85=12","67-55=12","77-65=12","87-75=12","92-75=17","72-55=17"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbgggb],
                     "92-75=17",
                     Ps).
test(439, Ps == ["92-75=17","72-55=17"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbgggb,
                      "82-65=17"-bggbgggg],
                     "92-75=17",
                     Ps).

test(440, Ps == ["3*5-13=2","5*3-13=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbrrrr,
                      "9*2-13=5"-bgrggggr],
                     "3*5-13=2",
                     Ps).

test(441, Ps == ["5-4+9=10","5+4+7=16","5+4+8=17","5+4+9=18","4+4+7=15","5+4+1=10","5+4+6=15","7+4+4=15"]) :-
    puzzle_solve_all(["2*4+5=13"-bbggrggb],
                     "5+4+9=18",
                     Ps).

test(442, Ps == ["5-35/7=0","7-35/5=0"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrrbr,
                      "60/5-3=9"-brrgrrgb],
                     "5-35/7=0",
                     Ps).

test(443, Ps == ["7*8+6=62","8*7+6=62","8*8+2=66"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbgbgbb,
                      "6*2+8=20"-rgrgrgbb],
                     "8*8+2=66",
                     Ps).

test(444, Ps == ["3+3+7=13","3+7+3=13","7+3+3=13"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbgbggg,
                      "6-1+8=13"-bbbgbggg],
                     "3+3+7=13",
                     Ps).

test(445, Ps == ["4*8-7=25","7*8-2=54","7*8-4=52","8*7-2=54","8*7-4=52","5*6-4=26","5*6-6=24","6*5-4=26","6*5-6=24","4*2*7=56","4*7*2=56","7*2*4=56","4*5*2=40","5*2*4=40"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbrgbb],
                     "5*6-4=26",
                     Ps).
test(445, Ps == ["5*6-4=26","5*6-6=24","6*5-4=26","6*5-6=24"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbrgbb,
                      "4*8-7=25"-rgbgbggr],
                     "5*6-4=26",
                     Ps).

test(446, Ps == ["38-13=25","52-17=35"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgrr,
                      "130/2=65"-rrbbrgbg],
                     "52-17=35",
                     Ps).

test(447, Ps == ["84/3/4=7","84/3/7=4","84/7/3=4","48/3/4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbrbr,
                      "36/4-9=0"-rbgrbbgb],
                     "48/3/4=4",
                     Ps).

test(448, Ps == ["11+3-9=5","11-5+3=9","11-9+3=5","13+1-9=5","13+5-9=9","13-5+1=9","13-9+1=5","15+3-9=9","15-9+3=9"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrrrrr,
                      "1+30/5=7"-grrbbrgb],
                     "13+1-9=5",
                     Ps).

:- end_tests(nerdle42).
:- begin_tests(nerdle44).

test(440, Ps == ["21+52=73","22+51=73","21+32=53","22+31=53"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbrrgrg],
                     "21+52=73",
                     Ps).

test(441, Ps == ["1*27/9=3","7*9/21=3","9*7/21=3","1*12-9=3","1*21/7=3","3*8-21=3","6*6/12=3","7*2-11=3","8*2-13=3","8*3-21=3","3*91=273"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbbrrg],
                    "7*9/21=3",
                     Ps).
test(441, Ps == ["7*9/21=3","9*7/21=3"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbbrrg,
                      "1*27/9=3"-rgrrrrgg],
                     "7*9/21=3",
                     Ps).

test(442, Ps == ["5+7*8=61","5+8*7=61","9+6*7=51","9+7*6=51","1+6*9=55","1+7*7=50","1+7*8=57","1+8*7=57","1+8*8=65","1+9*6=55","6+5*9=51"]) :-
    puzzle_solve_all(["2*4+5=13"-brbrrgrb],
                     "1+8*8=65",
                     Ps).

test(443, Ps == ["6*8+2=50","7*9+2=65","8*6+2=50","9*7+2=65","5*9+7=52","6*9+2=56","7*8+2=58","8*2+9=25","8*7+2=58","9*2+7=25","9*5+7=52","9*6+2=56","5*5+2=27"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbgrgbb],
                     "7*9+2=65",
                     Ps).
test(443, Ps == ["7*9+2=65","9*7+2=65"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbgrgbb,
                      "6*8+2=50"-rgbgggrb],
                     "7*9+2=65",
                     Ps).

test(444, Ps == ["6*9-50=4","6*9-54=0","9*6-50=4","9*6-54=0","7*9-54=9","7*9-59=4","9*7-54=9","9*7-59=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrbgrbb],
                     "9*6-54=0",
                     Ps).

test(445, Ps == ["41-5*7=6","41-7*5=6","46-5*9=1","46-9*5=1","41-4*9=5","41-6*6=5","41-9*4=5"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrrb,
                      "1/5*40=8"-rbrrrbgb],
                     "41-5*7=6",
                     Ps).

test(446, Ps == ["62-10=52","62-12=50"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgrb,
                      "102-7=95"-rrrrbgbr],
                     "62-12=50",
                     Ps).

test(447, Ps == ["4*2*3=24","4*3*2=24"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbgbr,
                      "3*8-4=20"-rgbbrggb],
                     "4*2*3=24",
                     Ps).

test(448, Ps == ["48+45=93","38+45=83","48+35=83"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrggbg],
                     "48+35=83",
                     Ps).

test(449, Ps == ["7-5+8=10","8-5+7=10"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbgrggb,
                      "6-5+9=10"-bgggbggg],
                     "7-5+8=10",
                     Ps).

test(450, Ps == ["21+52=73","22+51=73","21+32=53","22+31=53"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbrrgrg],
                     "21+32=53",
                     Ps).

test(451, Ps == ["10-1-9=0","10-9-1=0","100-91=9","100-99=1","11-1-1=9","11-1-9=1","11-9-1=1","19-1-9=9","19-9-1=9","19-9-9=1"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbbrrb,
                      "9-16/8=7"-rrrbbbgb],
                     "19-9-9=1",
                     Ps).

test(452, Ps == ["60+38=98","60+30=90","60+36=96","60+39=99"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbgbr,
                      "30+67=97"-rggrbggb],
                     "60+38=98",
                     Ps).

test(453, Ps == ["6/18*9=3","9/18*6=3","1/6*18=3","10*1-7=3","10-1*7=3","10-7*1=3","18*1/6=3","18/6*1=3","6/16*8=3","8/16*6=3","11*1-8=3","11-1*8=3","11-8*1=3","19*7=133","61*3=183"]) :-
    puzzle_solve_all(["2*4+5=13"-brbbbrrg],
                     "1/6*18=3",
                     Ps).
test(453, Ps == ["1/6*18=3"]) :-
    puzzle_solve_all(["2*4+5=13"-brbbbrrg,
                      "6/18*9=3"-rgrrrbgg],
                     "1/6*18=3",
                     Ps).

test(454, Ps == ["30+28=58","36+22=58","38+20=58","35+23=58"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrgbr,
                      "32+57=89"-grgrbgrb],
                     "38+20=58",
                     Ps).

test(455, Ps == ["4*8-30=2","4*8-32=0","8*3-20=4","8*3-24=0"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbrbr,
                      "3*24/8=9"-rgrrbrgb],
                     "8*3-24=0",
                     Ps).

test(456, Ps == ["60/2/6=5","60/6/2=5","50/2/5=5","50/5/2=5"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrbb,
                      "8-27/9=5"-bbrbgbgg],
                     "60/2/6=5",
                     Ps).

test(457, Ps == ["5*7+4=39","5*9+3=48","6*7+3=45","7*5+4=39","7*6+3=45","9*5+3=48","4*7+7=35","4*8+3=35","5*5+9=34","5*6+4=34","6*5+4=34"]) :-
    puzzle_solve_all(["2*4+5=13"-bgrgrgbr],
                     "7*6+3=45",
                     Ps).

test(458, Ps == ["5+7+9=21","5+9+7=21","7+5+9=21","9+5+7=21","5+8+8=21","8+5+8=21"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbgrgrb],
                     "5+8+8=21",
                     Ps).

test(459, Ps == ["10-6+3=7","10-7+3=6"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbrrr,
                      "1+63/9=8"-grrrbbgb],
                     "10-6+3=7",
                     Ps).

:- end_tests(nerdle44).
:- begin_tests(nerdle46).

test(460, Ps == ["20-6-7=7","20-7-6=7","20-7-7=6"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbbrbb,
                      "20/2-2=8"-ggbbgbgb],
                     "20-7-7=6",
                     Ps).

test(461, Ps == ["56/8-3=4","63/7-4=5","63/7-5=4","64/8-3=5","36/4-4=5","36/4-5=4","54/6-3=6"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrbr,
                      "45/3-6=9"-rrgrgrgb],
                     "63/7-4=5",
                     Ps).

test(462, Ps == ["65/5-5=8","88/8-5=6","88/8-6=5"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrrbb,
                      "56/7-8=0"-rrgbgrgb],
                     "65/5-5=8",
                     Ps).

test(463, Ps == ["67+31=98","37+31=68"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbgrr,
                      "13+67=80"-rrgrrgrb],
                     "67+31=98",
                     Ps).

test(464, Ps == ["364/4=91","114/3=38","144/3=48","144/4=36"]) :-
    puzzle_solve_all(["2*4+5=13"-bbgbbgrr],
                     "114/3=38",
                     Ps).

test(465, Ps == ["5*6-1=29","5*6-9=21","6*5-1=29","6*5-9=21","5*5/1=25","5*5*1=25"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbrgrb],
                     "5*6-1=29",
                     Ps).

test(466, Ps == ["2*5/1=10","2*8-1=15","2*5*1=10"]) :-
    puzzle_solve_all(["2*4+5=13"-ggbbrggb],
                     "2*5*1=10",
                     Ps).

test(467, Ps == ["60/2/5=6","60/5/2=6","60/5/6=2","60/6/5=2","50/5/5=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrbb,
                      "8-27/9=5"-bbrbgbgr],
                     "60/2/5=6",
                    Ps).

test(468, Ps == ["10-1*5=5","10-5*1=5"]) :-
    puzzle_solve_all(["2*4+5=13"-brbbrrrb,
                      "51-6*7=9"-rrgbgbgb],
                     "10-1*5=5",
                     Ps).

test(469, Ps == ["72/1/8=9","72/1/9=8","72/8/1=9","72/8/9=1","72/9/1=8","72/9/8=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbbrrb,
                      "10-6/2=7"-rbbbgrgr],
                     "72/9/8=1",
                     Ps).

test(470, Ps == ["96-13=83","99-16=83"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbbgrg,
                      "80-17=63"-rbggbgrg],
                     "99-16=83",
                     Ps).

test(471, Ps == ["168/56=3","177/59=3","150/50=3","159/53=3","165/55=3","171/57=3","153/51=3"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbgrrg],
                     "165/55=3",
                     Ps).

test(472, Ps == ["1+3+9=13","1+9+3=13","3+9+1=13","9+3+1=13"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbgbggg,
                      "6-1+8=13"-bbrgbggg],
                     "1+3+9=13",
                     Ps).

test(473, Ps == ["6*6-7=29","6*6-9=27"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbbgbb,
                      "8*9-2=70"-bgrgrgrb],
                     "6*6-9=27",
                     Ps).

test(474, Ps == ["10-4-6=0","19-4-6=9","19-4-9=6","19-9-4=6","19-9-6=4","11-1-4=6","11-1-6=4","11-4-6=1","14-4-4=6","14-4-6=4","16-4-6=6"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbrrb,
                      "48/6-1=7"-rbbrgrgb],
                     "11-1-4=6",
                     Ps).
test(474, Ps == ["19-9-4=6","11-1-4=6"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbrrb,
                      "48/6-1=7"-rbbrgrgb,
                      "10-4-6=0"-gbgrgrgb],
                     "11-1-4=6",
                     Ps).

test(475, Ps == ["31-3*9=4","31-9*3=4"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbbrrr,
                      "1/9*36=4"-rbrrrbgg],
                     "31-3*9=4",
                     Ps).

test(476, Ps == ["2/10*5=1","2/5*15=6","2/6*15=5","21-2*8=5","21-8*2=5","26-5*5=1","25*6=150","25*1/5=5","25/5*1=5","20*5=100","21*5=105","25*7=175","26*6=156","25*5=125"]) :-
    puzzle_solve_all(["2*4+5=13"-grbbrrrb],
                     "20*5=100",
                     Ps).
test(476, Ps == ["25*6=150","20*5=100"]) :-
    puzzle_solve_all(["2*4+5=13"-grbbrrrb,
                      "2/10*5=1"-gbrrrrrb],
                     "20*5=100",
                     Ps).

test(477, Ps == ["66-7*9=3","66-9*7=3","67-8*8=3","67*9=603","30-3*9=3","30-9*3=3","39-6*6=3","77*9=693","87*9=783","97*9=873","99*7=693","37*9=333"]) :-
    puzzle_solve_all(["2*4+5=13"-brbbbrbg],
                     "67-8*8=3",
                     Ps).

test(478, Ps == ["6*4+9=33","9*4+7=43"]) :-
    puzzle_solve_all(["2*4+5=13"-bgggbgbg],
                     "9*4+7=43",
                     Ps).

test(479, Ps == ["7+9+9=25","8+5+9=22","8+8+9=25","9+7+9=25"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbgrgbb,
                      "5+6+9=20"-rgbggggb],
                     "8+8+9=25",
                     Ps).

test(490, Ps == ["14+43=57","15+43=58","11+43=54"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrrgrr,
                      "13+46=59"-grggbggb],
                     "11+43=54",
                     Ps).

test(481, Ps == ["520/8=65","528/6=88","528/8=66","656/8=82"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgbb,
                      "86-27=59"-rrbrbgrb],
                     "520/8=65",
                     Ps).

test(482, Ps == ["19-4-7=8","19-7-4=8","19-7-8=4","19-8-7=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbrrb,
                      "48/6-1=7"-rrbbgrgr],
                     "19-4-7=8",
                     Ps).

test(483, Ps == ["5+11-9=7","7+9-11=5","7+9-15=1","9+7-11=5","9+7-15=1"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrrrrb,
                      "1+56/7=9"-rgrbbrgr],
                     "5+11-9=7",
                     Ps).

test(484, Ps == ["201/3=67","261/3=87","210/3=70","210/7=30","216/3=72","273/3=91","279/9=31","291/3=97","213/3=71","216/6=36","217/7=31","231/3=77"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbbgrr],
                     "279/9=31",
                     Ps).
test(484, Ps == ["279/9=31","217/7=31"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbbgrr,
                      "201/3=67"-gbrgrgbr],
                     "279/9=31",
                     Ps).

test(485, Ps == ["79-31=48","79-38=41","79-41=38","79-48=31","97-13=84"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbgrr,
                      "316/4=79"-rrbbrgrr],
                     "79-38=41",
                     Ps).

test(486, Ps == ["4+9-3=10","8+9-3=14","9+8-3=14"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrbggr],
                     "8+9-3=14",
                     Ps).

test(487, Ps == ["9+5+2=16","1+5+6=12","6+5+1=12","8+5+2=15"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbgrggb,
                      "5-2+7=10"-rbrgbggb],
                     "1+5+6=12",
                     Ps).
test(487, Ps == ["1+5+6=12","6+5+1=12"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbgrggb,
                      "5-2+7=10"-rbrgbggb,
                      "9+5+2=16"-bgggrggr],
                     "1+5+6=12",
                     Ps).

test(488, Ps == ["43+49=92","49+43=92"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrrbgbr,
                      "32+46=78"-rrggbgbb],
                     "43+49=92",
                     Ps).

test(489, Ps == ["7*4+8=36","8*4+7=39","6*4+6=30","7*4+6=34","7*4+9=37","8*4+4=36","8*4+6=38","9*4+3=39"]) :-
    puzzle_solve_all(["2*4+5=13"-bgggbgbr],
                     "7*4+8=36",
                     Ps).

test(490, Ps == ["15+59=74","17+54=71","17+57=74"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrrgrb,
                      "14+56=70"-grggbggb],
                     "17+57=74",
                     Ps).

test(491, Ps == ["35/7-3=2","35/5-5=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrbr,
                      "63/9-2=5"-brgbgrgr],
                     "35/7-3=2",
                     Ps).

test(492, Ps == ["7+7*8=63","7+8*7=63","9+8*8=73","6+3*9=33","6+9*3=33","9+3*8=33","9+6*9=63","9+8*3=33","9+9*6=63"]) :-
    puzzle_solve_all(["2*4+5=13"-brbrbgbg],
                     "9+8*8=73",
                     Ps).

test(493, Ps == ["651/7=93","318/6=53","50-17=33","581/7=83","63-10=53","69-16=53","70-17=53","71-18=53","91-38=53","91-58=33","159/3=53","371/7=53","51-18=33","511/7=73","66-13=53"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrgrg],
                     "66-13=53",
                     Ps).
test(494, Ps == ["63-10=53","66-13=53"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrgrg,
                      "651/7=93"-grrbbgbg],
                     "66-13=53",
                     Ps).

test(495, Ps == ["95*2=190","50*2=100","51*2=102"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbrrrb,
                      "17-2*6=5"-rbbgrbrr],
                     "51*2=102",
                     Ps).

test(496, Ps == ["205/5=41","210/5=42"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrbggrb],
                     "210/5=42",
                     Ps).

test(497, Ps == ["7-13+9=3","9-13+7=3","7-11+7=3"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrbrrg,
                      "6+7-10=3"-brrrrbgg],
                     "7-13+9=3",
                     Ps).

test(498, Ps == ["74*2=148","71*2=142","72*2=144"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrbbrrb,
                      "1/6*42=7"-rbbrrrrr,
                      "70*2=140"-gbgggggb],
                     "72*2=144",
                     Ps).
test(498, Ps == ["70*2=140","74*2=148","87*2=174","97*2=194","71*2=142","72*2=144"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrbbrrb,
                      "1/6*42=7"-rbbrrrrr],
                     "72*2=144",
                     Ps).

test(499, Ps == ["11+3-9=5","11-5+3=9","11-9+3=5","13+1-9=5","13+5-9=9","13-5+1=9","13-9+1=5","15+3-9=9","15-9+3=9"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrrrrr,
                      "1+30/5=7"-grrbbrgb],
                     "11-5+3=9",
                     Ps).

test(500, Ps == ["2*9-13=5","2*53=106","2*65=130"]) :-
    puzzle_solve_all(["2*4+5=13"-ggbbrrrr],
                     "2*65=130",
                     Ps).
test(500, Ps == ["2*53=106","2*65=130"]) :-
    puzzle_solve_all(["2*4+5=13"-ggbbrrrr,
                      "2*9-13=5"-ggbbrrrr],
                     "2*65=130",
                     Ps).

test(501, Ps == ["10/1/5=2","10/5/1=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrrb,
                      "5-18/6=2"-rbrbgbgg],
                     "10/1/5=2",
                     Ps).

test(502, Ps == ["17-3*4=5","17-4*3=5","19-3*5=4","19-5*3=4","34*5=170","39*4=156","58*3=174","14-3*3=5","35*4=140","45*3=135"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrrr],
                     "34*5=170",
                     Ps).

test(503, Ps == ["78-10=68","78-18=60","86-10=76","86-16=70","71-10=61","71-11=60","76-10=66","76-16=60","77-10=67"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbbgrb,
                      "106-7=99"-rrrrrgbb],
                     "76-16=60",
                     Ps).
test(504, Ps == ["71-11=60","76-16=60"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbbgrb,
                      "106-7=99"-rrrrrgbb,
                      "78-10=68"-gbggrggb],
                     "76-16=60",
                     Ps).

:- end_tests(nerdle46).
:- begin_tests(nerdle50).

test(505, Ps == ["3*4-10=2","3*4-12=0","6*4-23=1","8*4/32=1","3*4/12=1","3*40=120","3*42=126","3*43=129","4*43=172","8*41=328","3*44=132"]) :-
    puzzle_solve_all(["2*4+5=13"-rggbbrrr],
                     "3*4-12=0",
                     Ps).

test(505_464, Ps == ["15-2*6=3","15-6*2=3","13-2*5=3","13-5*2=3"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbrrrg],
                     "13-5*2=3",
                     Ps).

test(505_465, Ps == ["5+27/9=8","5+28/7=9","56/8+2=9","5+20/5=9"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrrbb],
                     "5+27/9=8",
                     Ps).

test(505_466, Ps == ["84*5=420","85*5=425"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrbrrbb,
                      "47-5*9=2"-rbbgrbrr],
                     "84*5=420",
                     Ps).

test(505_467, Ps == ["59-16=43","67-14=53","53-10=43","56-13=43","57-14=43","64-11=53","84-31=53","84-51=33","94-41=53","94-51=43","54-11=43"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrgrg],
                     "53-10=43",
                     Ps).
test(505_467, Ps == ["53-10=43","57-14=43","54-11=43"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrgrg,
                      "59-16=43"-gbggbggg],
                     "53-10=43",
                     Ps).

test(505_468, Ps == ["7-2+5=10","7/1+5=12","8-1+5=12","8-2+5=11","9-2+5=12","9+2+5=16","1+6+5=12","6+1+5=12","8+2+5=15","5+2+5=12"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbggggb],
                     "1+6+5=12",
                     Ps).
test(505_468, Ps == ["1+6+5=12","6+1+5=12"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbggggb,
                      "7-2+5=10"-bbrggggb],
                     "1+6+5=12",
                     Ps).

test(505_469, Ps == ["51-12=39","91-32=59","91-52=39"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrgrr,
                      "130/2=65"-rrbbggbr],
                     "91-32=59",
                     Ps).

test(505_470, Ps == ["7*8-53=3","8*7-53=3"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbgrbg],
                     "7*8-53=3",
                     Ps).

test(505_471, Ps == ["7*93=651","3*56=168","3*65=195","3*55=165"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbrrrr,
                      "1*30/5=6"-rgrbbrrr],
                     "3*56=168",
                     Ps).
test(505_471, Ps == ["3*56=168","3*55=165"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbrrrr,
                      "1*30/5=6"-rgrbbrrr,
                      "7*93=651"-bgbrgrrr],
                     Ps).

test(505_472, Ps == ["3/24*8=1","8/24*3=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rrrbbrrr,
                      "1/3*24=8"-rgrrrrgr],
                     "3/24*8=1",
                     Ps).

test(505_473, Ps == ["48-37=11","93-74=19","93-79=14","31-17=14"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbggr,
                      "60-43=17"-bbgrrggr],
                     "93-79=14",
                     Ps).
test(505_473, Ps == ["93-74=19","93-79=14"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbggr,
                      "60-43=17"-bbgrrggr,
                      "48-37=11"-rbgrrggb],
                     "93-79=14",
                     Ps).

test(505_474, Ps == ["24/3-2=6","24/3-6=2","24/3-4=4"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrbbrbr,
                      "24/3-8=0"-gggggbgb],
                     "24/3-4=4",
                     Ps).

test(505_475, Ps == ["89-70=19","97-80=17","80-70=10","81-70=11","87-70=17"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbbggb,
                      "78-60=18"-rrgbgggb],
                     "89-70=19",
                     Ps).

test(505_476, Ps == ["5+3*7=26","5+3*8=29","5+7*3=26","5+8*3=29","7+3*6=25","7+6*3=25","3+7*7=52","5+3*9=32","5+5*3=20","5+9*3=32","7+5*3=22"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbrrgbr],
                     "7+5*3=22",
                     Ps).

test(505_477, Ps == ["15-2-7=6","15-6-2=7","15-7-2=6","16-2-5=9","16-2-9=5","16-5-2=9","16-9-2=5","12-1-5=6","12-5-1=6","12-6-1=5","12-6-5=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrrb,
                      "5-18/6=2"-rrrbbrgr],
                     "16-2-5=9",
                     Ps).
test(505_477, Ps == ["16-2-5=9","16-2-9=5"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbrrrb,
                      "5-18/6=2"-rrrbbrgr,
                      "15-2-7=6"-grgggbgr],
                     "16-2-5=9",
                     Ps).

test(505_478, Ps == ["2-3+10=9","2-6+13=9","2-7+13=8","2-8+13=7","2-9+13=6"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbgbrrr],
                     "2-9+13=6",
                     Ps).

test(505_478, Ps == ["18-5-4=9","18-5-9=4","14-5-8=1","15-5-4=6","15-5-6=4","14-5-4=5","14-5-5=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrrb,
                      "40/5-1=7"-rbbggrgb],
                     "14-5-5=4",
                     Ps).
test(505_478, Ps == ["15-5-6=4","14-5-5=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrrb,
                      "40/5-1=7"-rbbggrgb,
                      "18-5-4=9"-gbgggrgb],
                     "14-5-5=4",
                     Ps).

test(505_479, Ps == ["72-25=47","72-45=27"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrbggbb,
                      "460/5=92"-rbbbggbr],
                     "72-25=47",
                     Ps).

test(505_480, Ps == ["11-7+4=8","11-8+4=7","14+1-7=8","14-7+1=8","14-8+1=7","11+1-4=8","11-8+1=4"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrrbrrb,
                      "1+64/8=9"-grbrbrgb],
                     "11-8+4=7",
                     Ps).

test(505_481, Ps == ["5*6/30=1","6*5/30=1"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbrrrr,
                      "1*30/5=6"-rgrrrrgr],
                     "5*6/30=1",
                     Ps).

test(505_482, Ps == ["5+3*7=26","5+3*8=29","5+7*3=26","5+8*3=29","7+3*6=25","7+6*3=25","3+7*7=52","5+3*9=32","5+5*3=20","5+9*3=32","7+5*3=22"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbrrgbr],
                     "7+3*6=25",
                     Ps).

test(505_483, Ps == ["66-29=37","66-39=27","69-32=37"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbbbgbr,
                      "623/7=89"-grrbrgbr],
                     "66-29=37",
                     Ps).

test(505_484, Ps == ["6*4/12=2","6*4/24=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rggbbrrb,
                      "1*42/6=7"-rggrrrgb],
                     "6*4/24=1",
                     Ps).

test(505_485, Ps == ["19-5-7=7","19-7-5=7","15-1-7=7","15-7-1=7","15-7-7=1","17-5-5=7"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrrrb,
                      "7-16/8=5"-rrrbbbgr],
                     "17-5-5=7",
                     Ps).

test(505_486, Ps == ["91-74=17","91-77=14","94-77=17"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbbggb,
                      "67-48=19"-brgrbggr],
                     "91-77=14",
                     Ps).

test(505_487, Ps == ["3*8/24=1","4*8/32=1","8*3/24=1"]) :-
    puzzle_solve_all(["2*4+5=13"-rgrbbrrr,
                      "1*24/3=8"-rgrrrrgr],
                     "3*8/24=1",
                     Ps).

test(505_488, Ps == ["15-2*6=3","15-6*2=3","13-2*5=3","13-5*2=3"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbrrrg],
                     "13-2*5=3",
                     Ps).

test(505_489, Ps == ["12*9=108","18*9=162"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbrrb,
                      "1/8*72=9"-gbrrbrrr],
                     "18*9=162",
                     Ps).

test(505_489, Ps == ["48/6-5=3","56/8-4=3","64/8-5=3","40/5-5=3","45/5-6=3","54/6-6=3","54/9-3=3","9-54/9=3","60/4/5=3","60/5/4=3","35/5-4=3","54/3/6=3","54/6/3=3","45/3/5=3","45/5/3=3"]) :-
    puzzle_solve_all(["2*4+5=13"-bbrbrrbg],
                     "45/5/3=3",
                     Ps).

test(505_490, Ps == ["1*40/5=8","1*40/8=5","6*4-15=9","6*4-19=5","1*45/5=9","1*45/9=5","5*4-11=9","5*4-14=6","5*4-16=4","5*4-19=1","4*45=180","4*4-11=5","4*4-15=1","5*4-15=5"]) :-
    puzzle_solve_all(["2*4+5=13"-bggbrrrb],
                     "4*4-11=5",
                     Ps).
test(505_490, Ps == ["6*4-19=5","5*4-11=9","5*4-14=6","5*4-16=4","5*4-19=1","4*4-11=5"]) :-
    puzzle_solve_all(["2*4+5=13"-bggbrrrb,
                      "1*40/5=8"-rggbbrgb],
                     "4*4-11=5",
                     Ps).

test(505_491, Ps == ["134/2=67","324/4=81","124/4=31"]) :-
    puzzle_solve_all(["2*4+5=13"-rbgbbgrr],
                     "124/4=31",
                     Ps).

test(505_492, Ps == ["20/2-4=6","20/2-6=4"]) :-
    puzzle_solve_all(["2*4+5=13"-gbrbbrbb,
                      "28/4-7=0"-gbgrgbgr],
                     "20/2-4=6",
                     Ps).

test(505_493, Ps == ["3*9-22=5","3*9-25=2","9*3-22=5","9*3-25=2"]) :-
    puzzle_solve_all(["2*4+5=13"-rgbbrrbr,
                      "5*6-23=7"-rgbggrgb],
                     "3*9-25=2",
                     Ps).

test(505_494, Ps == ["7*87=609","8*9-66=6","9*8-66=6","7*98=686","8*76=608","8*87=696","8*96=768","8*97=776","7*97=679","8*86=688"]) :-
    puzzle_solve_all(["2*4+5=13"-bgbbbrbb],
                     "7*98=686",
                     Ps).

test(505_495, Ps == ["207/69=3","267/89=3","20/2-7=3","27/3-6=3","20-8-9=3","20-9-8=3","228/76=3","270/90=3","276/92=3","288/96=3","22/2-8=3","237/79=3","279/93=3","297/99=3","27/3/3=3"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbbbrbg],
                     "22/2-8=3",
                     Ps).

test(505_495, Ps == ["3*4-10=2","3*4-12=0","6*4-23=1","8*4/32=1","3*4/12=1","3*40=120","3*42=126","3*43=129","4*43=172","8*41=328","3*44=132"]) :-
    puzzle_solve_all(["2*4+5=13"-rggbbrrr],
                     "3*4-10=2",
                     Ps).

test(505_496, Ps == ["27-4*6=3","27-6*4=3","24-3*7=3","24-7*3=3","27*9=243"]) :-
    puzzle_solve_all(["2*4+5=13"-grrbbrbg],
                     "24-7*3=3",
                     Ps).
test(505_496, Ps == ["24-3*7=3","24-7*3=3"]) :-
    puzzle_solve_all(["2*4+5=13"-grrbbrbg,
                      "27-4*6=3"-grgrgbgg],
                     "24-7*3=3",
                     Ps).

test(505_497, Ps == ["2+56/8=9","27/9+5=8","28/7+5=9","2+20/5=6","2+27/9=5","20/5+2=6","20/5+5=9","27/9+2=5","2+25/5=7","25/5+2=7"]) :-
    puzzle_solve_all(["2*4+5=13"-gbbrrrbb],
                     "2+56/8=9",
                     Ps).

test(505_498, Ps == ["8/6*9=12","9/6*8=12","6/2*6=18","8/1*2=16","9/1*2=18","6/1*2=12"]) :-
    puzzle_solve_all(["2*4+5=13"-rrbbbggb],
                     "8/6*9=12",
                     Ps).

test(505_499, Ps == ["1+32/8=5","12/3+5=9","12/6+3=5","15/3+2=7","3+12/6=5","32/8+1=5","5+12/3=9","1+12/3=5","12/3+1=5"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrrrr],
                     "12/3+1=5",
                     Ps).
test(505_499, Ps == ["12/6+3=5","12/3+1=5"]) :-
    puzzle_solve_all(["2*4+5=13"-rbbrrrrr,
                      "1+32/8=5"-grrrrbgg],
                     "12/3+1=5",
                     Ps).

test(505_500, Ps == ["47-5*8=7","47-8*5=7","45-5*8=5","45-8*5=5","48-5*8=8","48-8*5=8"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrbb,
                      "54-6*9=0"-rrgbgbgb],
                     "45-8*5=5",
                     Ps).
test(505_500, Ps == ["45-8*5=5","48-8*5=8"]) :-
    puzzle_solve_all(["2*4+5=13"-brrbrrbb,
                      "54-6*9=0"-rrgbgbgb,
                      "47-5*8=7"-gbgrgrgb],
                     "45-8*5=5",
                     Ps).

test(505_501, Ps == ["86-30=56","86-50=36"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbbrgbr,
                      "390/6=65"-rbrbrgrr],
                     "86-30=56",
                     Ps).

test(505_502, Ps == ["17+29=46","19+27=46","14+62=76","14+12=26","16+26=42"]) :-
    puzzle_solve_all(["2*4+5=13"-rbrrbgrb,
                      "12+48=60"-grgrbgrb],
                     "16+26=42",
                     Ps).

test(505_503, Ps == ["7-10+8=5","7-15+8=0","7-11+5=1"]) :-
    puzzle_solve_all(["2*4+5=13"-bbbrrrrb,
                      "1+56/7=9"-rrrbbrgb],
                     "7-11+5=1",
                     Ps).

test(exception, error(nerdle_assertion(fail,nerdle:verify_guess(['2',*,'3',+,'8',+,'1','4'])))) :-
    puzzle_solve_all(["2*3+8+14"-brbbbrbr], _).

:- end_tests(nerdle50).
