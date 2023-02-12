# nerdle
Nerdle game in Prolog

This is a sandbox for playing with strategies for playing
https://nerdlegame.com/

The interface is still quite crude. There's an "aid" for playing,
which you can use like this (see also `test_nerdle.pl`). It starts
with an initial guess of
```
6+9-3=12
rrrbrgrb  (response)
```
That is: `6`, `+`, `9`, `1` were red; `-`, `2` were black;
`=` was green. (Red means the tile appears in the answer but
at a different location; black means it's wrong and doesn't
appear elsewhere; green means that it's correct):
```
swipl nerdle.pl

?- puzzle_solve_all(["6+9-3=12"-"rrrbrgrb"], Ps).
Ps = ["13+46=59","13+76=89","14+69=83","17+39=56","19+37=56","19+64=83","31+64=95","31+67=98","34+61=95","37+19=56","37+61=98","39+17=56","43+16=59","73+16=89","13+36=49"|...].
```
The output is a list of suggestions for the  next guess, with the "best"
one first. So, using that:
```
?- puzzle_solve_all(["6+9-3=12"-"rrrbrgrb", "13+46=59"-"grggrgbr"], Ps).
Ps = ["14+49=63","19+44=63"].

?- puzzle_solve_all(["6+9-3=12"-"rrrbrgrb", "13+46=59"-"grggrgbr", "14+49=63"-"grggrggg"], Ps).
Ps = ["19+44=63"].
```

To run the tests (which are mainly replaying some other games):
```
swipl -g test_nerdle -t halt test_nerdle.pl
```

## The files

`nerdle.pl` - The main program.

`expr.pl` - Parses a guess and validates it

`test_nerdle.pl` - Tests

`gen_all_puzzles.pl` - Generates the file `all_puzzles_facts.pl`,
using the query
`write_all_puzzles('all_puzzles_facts.pl',all_puzzles_facts)`.

`all_puzzles_facts.pl` - An exhaustive list of all possible
puzzles, with a few "trivial" ones removed (e.g., they have
a multiply-by-zero). This is generated by
`gen_all_puzzles:write_all_puzzles/2`.

## How the code works

### Data structures

The puzzle and guesses are kept in 8-element lists of atoms, which
are also called "labels" or "tiles".
For example:
```
?- string_chars("97-31=66",Puzzle),string_chars("19+31=50",Guess), nerdle:puzzle_guess_result(Puzzle,Guess,Result).```
Puzzle = ['9','7',-,'3','1',=,'6','6'],
Guess = ['1','9',+,'3','1',=,'5','0'],
Result = [黒,紅,黒,緑,緑,緑,黒,黒].
```

The `Result` uses Chinese characters for green, red, black (緑,紅,黒),
partly for my amusement and partly to avoid single-letter typos.
For convenience, `g`, `r`, `b` can be used for input and output.

Normally, it is considered bad style to use a fixed-length list, but
`maplist`, `foldl`, `include`, `exclude`, and `library(pairs)` all use
lists.

### Expression parser

The expression parser uses a simple DCG grammar, in predicate
`expr//1`. This predicate can either recognize an expression or
generate all possible expressions by backtracking (for this, you must
limit the length of the expression or else there's infinite
backtracking. We can't use the built-in `term_string/2` because it
allows things that the Nerdle game doesn't, for example '01+-3`
(leading zeros aren't allowed, nor are negative numbers).

There is also an `eval/2` predicate that takes the term generated
by `expr//1` and evaluates it - we could use the builtin `is/2`
predicate (surrounded by a `catch/3` to handle division by zero),
but the `eval/2` code is only a few lines; and it also allows us
to use `freeze/2` in future if that's needed.

## Showing the result of a guess

The `puzzle_guess_result/3` predicate takes a puzzle and a guess, and
returns a list of results (green, red, black). The code is
straightforward, although the original version had a few corner-case
bugs.

## Generate and test

Generate and test would be too slow, so we use a test-and-generate
style that takes advantage of predicates delaying (using `freeze/2` or
`dif/2`). When a guess is made, it used to generate constraints for
each position in the puzzle, whether it must be or must not be a
specific label (digit or operator) Additionally, the guess must
contain a certain nummber of each label, or cannot contain a specific
label. In effect, this interleaves the constraints with the generation
of possible solutions.

For example, if the puzzle is `97-31=66`, and the first guess is
`3*8/1=24`, this results `rbbbggbb` (紅黒黒黒緑緑黒黒). Numbering
the positions from 1 to 8, we can infer that:
- position 1 cannot be `3`
- position 2 cannot be `*`
- position 3 cannot be `8`
- position 4 cannot be `/`
- position 5 must be `1`
- position 6 must be `=`
- position 7 must not be `2`
- position 8 must not be `4`
- There must be at least one `1`, `3`, `=` in the answer.
- There must not be any `*`, `/`, `2`, `4`, or `8` in the answer.

A second guess of `19+31=50` has the result `brbgggbb` (黒紅黒緑緑緑黒黒),
from which we can infer:
- position 1 cannot be `3` or `1`
- position 2 cannot be `*` or `9`
- position 3 cannot be `8` or `+`
- position 4 must be `3`
- position 5 must be `1`
- position 6 must be `=`
- position 7 must not be `2` or `5`
- position 8 must not be `4` or `0`
- There must be at least one `3`, `9`, `=` in the answer.
- There must be exactly one `1` in the answer.
- There must not be any `*`, `/`, `1`, `2`, `4`, `5`, `8`, or `0` in the answer.

The "must be" and "cannot be" constraints are executed first (using
`dif/2` for "cannot be"), then the possible puzzles are generated, and
finally the number of counts for each label are checked.

Note that this code assumes that constraints are monotonic - that is,
each subsequent guess must leave "green" guesses in-place and must
also use all the labels that were guessed correctly but in the wrong
location. (This corresponds to "hard mode" in Wordle.) It is possible
that an optimal strategy might use non-monotonic guesses, but for now
the monotonic approach seems to work well.

## Analyzing distribution of puzzles

The file `all_puzzles_facts.pl` contains all the possible puzzles,
less a few uninteresting ones such as multiply by zero.  It was
created by:
```
?- write_all_puzzles('all_puzzles_facts.pl', all_puzzles_facts).
```

TODO: add analysis here - NOTES.md has a start.

# Countdown

See countdown-notes.md
