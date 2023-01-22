# nerdle notes

Here are some notes on finding an optimal strategy for playing
http://nerdlegame.com/ ... they are far from finished, but are a kind
of "work in progress" notebook.


```
findall(C1, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
[1-3473, 2-2151, 3-1917, 4-1894, 9-1852, 6-1779, 5-1760, 8-1758, 7-1731, 0-251]

findall(C2, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
[(*)-3094, (+)-2343, 0-1532, 2-1340, 4-1278, 6-1222, 8-1179, 1-1171, 5-1168, 3-1097, 7-1023, 9-966, (-)-739, (/)-414]

C2='*', findall(C1, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
[8-403, 9-397, 6-390, 7-380, 5-366, 4-357, 3-324, 2-271, 1-206]

C1='8', C2='*', findall(C3, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
[9-51,6-50,3-48,5-48,4-47,7-47,8-47,2-42,1-23]

C1='8', C2='*', C3='9', findall(C4, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
[(-)-20,(+)-10,(/)-10,(*)-1,0-1,1-1,2-1,3-1,4-1,5-1,6-1,7-1,8-1,9-1]
```
Let's try `['8',*,'9','2','=','7','3','6']`
or  `['8',*,'9','/','2','=','3','6']`

Let's look at totals for all labels
```
findall(C, char_in_puzzle(C), Cs), sort_by_frequency(Cs, CsS), writeln(CsS).
   [(=)-18566, 1-15159, 2-12089, 4-10816, 3-10467, 6-10009, 5-9990, 8-9495, (-)-9066, 7-8998, 9-8950, (+)-8256, (*)-6122, 0-5973, (/)-4572]

C5='=', aggregate_all(count, a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Count).
Count =  1318
C6='='  10914
C7='='   6334
```
So, maybe:
```
['4','*','9','-','1','=','3','5']
```
Or (better?):
```
['6','+','9','-','3','=','1','2']
    C7='1', C8='2',C2='+',C6='=', puzzle_fill([C1,C2,C3,C4,C5,C6,C7,C8]),P=[C1,C2,C3,C4,C5,C6,C7,C8], sort(P,PS), length(PS,8).
```
On the other hand, this seems to work well:
```
3*8/1=24
```

Another approach ... first two guess use as many possible digits and operators as possible:
```
a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs=[C1,C2,C3,C4,C5,C6,C7,C8],
a_puzzle(_,D1,D2,D3,D4,D5,D6,D7,D8), Ds=[D1,D2,D3,D4,D5,D6,D7,D8], append(Cs,Ds,CDs),
memberchk('*',CDs), memberchk('-',CDs), memberchk('/',CDs), memberchk('+',CDs),
sort(CDs, CDsSort), length(CDsSort, SortLen),
msort(CDs, CDsMsort), length(CDsMsort, MsortLen),
LenDiff is MsortLen - SortLen, LenDiff =< 1.
```
```
Cs = ['1',*,'2',+,'8',=,'1','0'],
Ds = ['4','5',/,'3',-,'6',=,'9']
```

```
?- a_puzzle(_,C1,C2,C3,C4,C5,C6,C7,C8), Cs=[C1,C2,C3,C4,C5,C6,C7,C8],
   a_puzzle(_,D1,D2,D3,D4,D5,D6,D7,D8), Ds=[D1,D2,D3,D4,D5,D6,D7,D8],
   append(Cs,Ds,CDs),
   memberchk('*',CDs), memberchk('-',CDs), memberchk('/',CDs), memberchk('+',CDs),
   sort(CDs, CDsSort), length(CDsSort, SortLen),
   msort(CDs, CDsMsort), length(CDsMsort, MsortLen),
   LenDiff is MsortLen - SortLen, LenDiff =< 1,
   string_chars(CsS,Cs), string_chars(DsS,Ds), writeln([CsS, DsS]),fail.
[1+5*9=46, 30/2-7=8]
[1+5*9=46, 30/2-8=7]
[1+6*8=49, 7-20/5=3]
[1+7*9=64, 8-30/5=2]
[1+8*6=49, 7-20/5=3]
[1+8*9=73, 40/5-2=6]
[1+8*9=73, 40/5-6=2]
[1+9*5=46, 30/2-7=8]
[1+9*5=46, 30/2-8=7]
[1+9*7=64, 8-30/5=2]
[1+9*8=73, 40/5-2=6]
[1+9*8=73, 40/5-6=2]
   ...
[0+56/7=8, 21-3*4=9]
[0+56/7=8, 21-4*3=9]
[0+56/8=7, 21-3*4=9]
[0+56/8=7, 21-4*3=9]
[0+63/7=9, 4*5-2=18]
[0+63/7=9, 4*5-8=12]
[0+63/7=9, 5*4-2=18]
[0+63/7=9, 5*4-8=12]
[0+63/7=9, 4*5-12=8]
[0+63/7=9, 4*5-18=2]
[0+63/7=9, 5*4-12=8]
[0+63/7=9, 5*4-18=2]
[0+63/9=7, 4*5-2=18]
[0+63/9=7, 4*5-8=12]
[0+63/9=7, 5*4-2=18]
[0+63/9=7, 5*4-8=12]
[0+63/9=7, 4*5-12=8]
[0+63/9=7, 4*5-18=2]
[0+63/9=7, 5*4-12=8]
[0+63/9=7, 5*4-18=2]
```
So, this suggests a strategy of
```
3*8-4=20
```
and then depending on whether `=` matches or not:
```
6/1+9=15
```
or
```
1+56/7=9
```
(and there are many permutations of this, e.g. `5*9-8=37` followed by `8+4/2=10` or `20/4+1=6`;
or `9/3+7=10` followed by `8*7-4=52` or `52-8*6=4`.)

See the code in `two_guesses.pl`.
