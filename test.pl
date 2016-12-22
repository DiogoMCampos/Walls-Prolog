:-use_module(library(clpfd)).


board([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, 4, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
        [o, o, o, o, o, o, o, o, o]]).



draw:-board(X), displayBoard(X, 9).

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
    (X == 0,
    write(' ');
    write(X)),
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
