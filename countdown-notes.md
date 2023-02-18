# Countdown

The program `countdown.pl` implements the "Countdown" example
from from chapter 9 of _Programming in Haskell_ by Graham Hutton
(Cambridge University Press, 2016). It is also discussed in
https://www.youtube.com/watch?v=dAeECyntQJg&list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3&index=2
and the source code is available at http://www.cs.nott.ac.uk/~pszgmh/pgp-countdown.hs
and a paper about the code is at https://www.cs.nott.ac.uk/~pszgmh/countdown.pdf

The program was run by:
```
ghc pgp-countdown.hs
./pgp-countdown
```
and the solutions were copied to `/tmp/countdown.out`. The Haskell
program has extra parentheses, so to allow comparing with the Prolog
results, the output is processed by:
```prolog
p :-
    open('/tmp/countdown.out', read, Stream),
    p(Stream),
    close(Stream).

p(Stream) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  true
    ;   term_string(Term, Line),
        writeln(Term),
        p(Stream)
    ).
```
The resulting file and the output of `countdown.pl` can be
compared by using the `sort` and `diff` utilities.

When run with `[1,3,5,10,25,50]` and `999`, got the following timings:
* 0.3 seconds with compiled Haskell (`ghc`)
* 4.2 seconds with interpreted Haskell (`ghci`)
* 43.4 seconds with SWI-Prolog (`swipl -O`)
* 52.6 seconds with SWI-Prolog when using the `num(_)` wrapper and `:-` for `eval/2`
  even as high as 62 seconds, with slight variations in the code
  - using =\= instead of \== did speed-up from 56 seconds to 42 seconds
  - removing a few redundant tests got 40.7 seconds



