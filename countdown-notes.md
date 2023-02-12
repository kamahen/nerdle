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



