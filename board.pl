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


getAffectedUp([X|_], PR, Line, _-PR, Ret):-
    ((X == o,
        Ret = [Line-PR])
    ;   Ret = -1).
getAffectedUp([_|Xs], Row, Line, PL-PR, Ret):-
    NewRow is Row + 1,
    getAffectedUp(Xs, NewRow, Line, PL-PR, Ret).

getAffectedUpMain([X|Xs], Row, PL, PL-PR, Up, Total):-
    % getAffectedLine(),
    NewLine is PL + 1.
    % getAffectedDown().
getAffectedUpMain([X|Xs], Row, Line, PL-PR, Acc, Ret):-
    NewLine is Line + 1,
    getAffectedUp(X, 1, Line, PL-PR, List),
    (is_list(List),
        append(Acc, List, Sum),
        getAffectedUpMain(Xs, Row, NewLine, PL-PR, Sum, Ret)
    ;   getAffectedUpMain(Xs, Row, NewLine, PL-PR, [], Ret)).

getAffectedLine([], _, _, _-_, Left, Right, Left, Right).
getAffectedLine([X|Xs], Row, Line, PL-PR, Left, Right, LfTotal, RtTotal):-
    NewRow is Row + 1,
    (Row == PR,
        getAffectedLine(Xs, NewRow, Line, PL-PR, Left, Right, LfTotal, RtTotal)
    ;Row < PR,
        (X == o,
            append(Left, [Line-Row], Sum),
            getAffectedLine(Xs, NewRow, Line, PL-PR, Sum, Right, LfTotal, RtTotal)
        ;   getAffectedLine(Xs, NewRow, Line, PL-PR, [], Right, LfTotal, RtTotal))
    ;Row > PR,
        (X == o,
            append(Right, [Line-Row], Sum),
            getAffectedLine(Xs, NewRow, Line, PL-PR, Left, Sum, LfTotal, RtTotal)
        ;   LfTotal = Left, RtTotal = Right)).

getAffectedBoard([X|Xs], Row, Line, PL-PR, [Lists|Next]):-
    NewLine is Line + 1,
    (Line < PL,
        getAffectedUp(X, Row, Line, PL-PR, Ret)
    ;Line == PL,
        get).

testLine :- boardTest([X,Y|Xs]), displayBoard([X,Y|Xs], 5), getAffectedLine(Y, 1, 2, 2-5, [], [], R, L), nl,write(R), nl, write(L).

testB :- boardTest(X), displayBoard(X, 5), getAffectedUpMain(X, 1, 1, 4-2, [], H), nl,write(H).

testA :- boardTest(X), displayBoard(X, 5), getCoords(X, 1, 1, [], H), nl, write(H).
