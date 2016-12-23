:-use_module(library(clpfd)).
:-use_module(library(lists)).

boardTest([[0, _, _, 3, _],
            [_, _, 4, _, 3],
            [_, _, _, 1, _],
            [_, 2, _, 0, _],
            [2, _, _, _, _]]).

board([[_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, 4, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _]]).

draw :- board(X), displayBoard(X, 9).
drawTest :- boardTest(X), displayBoard(X, 5).

translate(0) :- write('|').
translate(1) :- write('-').
translate(X) :- integer(X), write(X).
translate(_) :- write(' ').



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

getPiece([], _, _, Ret, Ret).
getPiece([X|Xs], Row, Line, Acc, Ret):-
    NewRow is Row + 1,
    (var(X),
        getPiece(Xs, NewRow, Line, Acc, Ret)
    ;   append(Acc, [Line-Row-X], Sum),
        getPiece(Xs, NewRow, Line, Sum, Ret)).

getCoords([], _, _, Ret, Ret).
getCoords([X|Xs], Row, Line, Acc, Ret):-
    NewLine is Line + 1,
    getPiece(X, Row, Line, [], Pieces),
    append(Acc, Pieces, Sum),
    getCoords(Xs, Row, NewLine, Sum, Ret).

getAffectedDown([X|_], PR, Line, _-PR, Ret):-
    ((var(X),
        Ret = [X])
    ;   Ret = -1).
getAffectedDown([_|Xs], Row, Line, PL-PR, Ret):-
    NewRow is Row + 1,
    getAffectedDown(Xs, NewRow, Line, PL-PR, Ret).

getAffectedDownMain([], _, _, _-_, Ret, Ret).
getAffectedDownMain([X|Xs], Row, Line, PL-PR, Acc, Ret):-
    NewLine is Line + 1,
    getAffectedDown(X, 1, Line, PL-PR, List),
    (is_list(List),
        append(Acc, List, Sum),
        getAffectedDownMain(Xs, Row, NewLine, PL-PR, Sum, Ret)
    ;   Ret = Acc).

getAffectedUp([X|_], PR, Line, _-PR, Ret):-
    ((var(X),
        Ret = [X])
    ;   Ret = -1).
getAffectedUp([_|Xs], Row, Line, PL-PR, Ret):-
    NewRow is Row + 1,
    getAffectedUp(Xs, NewRow, Line, PL-PR, Ret).

getAffectedUpMain([X|Xs], Row, PL, PL-PR, Up, Total):-
    getAffectedLine(X, Row, PL, PL-PR, [], [], Left, Right),
    NewLine is PL + 1,
    reverse(Up, ReverseUp),
    reverse(Left, ReverseLeft),
    getAffectedDownMain(Xs, Row, NewLine, PL-PR, [], Down),
    Total = [ReverseUp, ReverseLeft, Right, Down].
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
        (var(X),
            append(Left, [X], Sum),
            getAffectedLine(Xs, NewRow, Line, PL-PR, Sum, Right, LfTotal, RtTotal)
        ;   getAffectedLine(Xs, NewRow, Line, PL-PR, [], Right, LfTotal, RtTotal))
    ;Row > PR,
        (var(X),
            append(Right, [X], Sum),
            getAffectedLine(Xs, NewRow, Line, PL-PR, Left, Sum, LfTotal, RtTotal)
        ;   LfTotal = Left, RtTotal = Right)).

restrict([], _,_,0).
restrict([Square|Next], Expect, Acc, Total):-
    Square #= Expect,
    Total #= Rest + Algo,
    restrict(Next, Expect, Algo, Rest).

% pega as casas e aplica restricoes
constrainAll(Board, Line-Row-Value):-
    getAffectedUpMain(Board, 1, 1, Line-Row, [], [Up, Down, Left, Right]),
    restrict(Up, 0, 0, TotalUp),
    restrict(Left, 1, 0, TotalLeft),
    restrict(Right, 1, 0, TotalRight),
    restrict(Down, 0, 0, TotalDown),
    Value #= TotalUp + TotalLeft + TotalRight + TotalDown.


listAllAffected(Board):-
    getCoords(Board, 1, 1, [], AllNumbers),
    maplist(constrainAll(Board), AllNumbers).


puzzle:-
    boardTest(X),
    include(var, X, Vars),
    domain(Vars, 0, 1),
    listAllAffected(X),
    labeling([], Vars),
    displayBoard(X, 5).

testGetSpaces :- boardTest(X), displayBoard(X, 5), getAffectedUpMain(X, 1, 1, 3-4, [], H), nl, write(H).

testLine :- boardTest([X,Y|Xs]), displayBoard([X,Y|Xs], 5), getAffectedLine(Y, 1, 2, 2-5, [], [], R, L), nl, write(R), nl, write(L).

testB :- boardTest(X), displayBoard(X, 5), getAffectedUpMain(X, 1, 1, 4-2, [], H), nl, write(H).

testA :- boardTest(X), displayBoard(X, 5), getCoords(X, 1, 1, [], H), nl, write(H).
