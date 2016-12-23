translate(X) :- var(X), write(' ').
translate(n(X)) :- write(X).
translate(1) :- write('|').
translate(0) :- write('-').

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

displayBoard([], Width) :-
    write('   '),
    displaySeparator(Width),
    nl.
displayBoard([L|Ls], Width) :-
    write('   '),
    displaySeparator(Width),
    nl,
    write('  '),
    displayLine(L),
    displayBoard(Ls, Width).

viewBoard(Board):-
    getWidth(Board, Size),
    displayBoard(Board, Size).

getWidth([X|_], Size):-
    length(X, Size).

getValue(n(X), X).
