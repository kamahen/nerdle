# nerdle
Nerdle game in Prolog

This is a sandbox for playing with strategies for playing
https://nerdlegame.com/

To play:
```
swipl ~/src/nerdle/nerdle.pl
run_puzzle([5,0,*,3,=,1,5,0]).
```

or, to run with a random puzzle:
```
swipl ~/src/nerdle/nerdle.pl
run_puzzle.
```

You will be prompted for a guess. After each guess, you'll get your
guess output in colorized form with:
- black: wrong
- purple: appears elsewhere in the answer
- green: correct

In addition, there is a summary of all the guesses, with the same
meanings for the colors.

If you want an aid for playing the game online, then put the
information you know into the specifics/9 predicate and run
```
solve(S).
```
or
```
forall(solve(S), writeln(S)).
```

