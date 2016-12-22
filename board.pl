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


% nao funciona
% suposto dar as coords dos numeros
getPiece([], _, _, Ret, Ret).
getPiece([X|Xs], Row, Line, Acc, Ret):-
    NewRow is Row + 1,
    (X == o,
        getPiece(Xs, NewRow, Line, Acc, Ret)
    ;   append(Acc, [Line-Row], Sum),
        getPiece(Xs, NewRow, Line, Sum, Ret)).

getCoords([], _, _, Ret, Ret).
getCoords([X|Xs], Row, Line, Acc, Ret):-
    NewLine is Line + 1,
    getPiece(X, Row, Line, [], Pieces),
    append(Acc, Pieces, Sum),
    getCoords(Xs, Row, NewLine, Sum, Ret).



testA :- boardTest(X), displayBoard(X, 5), getCoords(X, 1, 1, [], H), nl, write(H).
