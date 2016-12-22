:-use_module(library(clpfd)).

boardTest([[0, o, o, 3, o],
      [o, o, 4, o, 3],
      [o, o, o, 1, o],
      [o, 2, o, 0, o],
      [2, o, o, o, o]]).

board([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, 4, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o]]).

draw :- board(X), displayBoard(X, 9).
drawTest :- boardTest(X), displayBoard(X, 5).

translate(o) :- write(' ').
translate(v) :- write('|').
translate(h) :- write('-').
translate(X) :- write(X).

displayCol([]) :- nl.
displayCol([X|Xs]) :-
    write('  '),
    write(X),
    write(' '),
    displayCol(Xs).

displayLine([]) :-
    write(' | '),
    nl.

displayLine([X|Xs]) :-
    write(' | '),
    translate(X),
    displayLine(Xs).

displaySeparator(0) :- write('+').
displaySeparator(N) :-
    N1 is N-1,
    write('+---'),
    displaySeparator(N1).

displayBoard([], N) :-
    write('   '),
    displaySeparator(N),
    nl.

displayBoard([L|Ls], N) :-
    write('   '),
    displaySeparator(N),
    nl,
    write('  '),
    displayLine(L),
    displayBoard(Ls, N).
