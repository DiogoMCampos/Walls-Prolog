:-use_module(library(clpfd)).
:-use_module(library(lists)).

:-include('examples.pl').
:-include('board.pl').

getPiece([], _, _, Ret, Ret).
getPiece([X|Xs], Row, Line, Acc, Ret) :-
    NewRow is Row + 1,
    (   var(X),
        getPiece(Xs, NewRow, Line, Acc, Ret)
    ;   getValue(X, Value),
        append(Acc, [Line-Row-Value], Sum),
        getPiece(Xs, NewRow, Line, Sum, Ret)).

getCoords([], _, _, Ret, Ret).
getCoords([X|Xs], Row, Line, Acc, Ret) :-
    NewLine is Line + 1,
    getPiece(X, Row, Line, [], Pieces),
    append(Acc, Pieces, Sum),
    getCoords(Xs, Row, NewLine, Sum, Ret).

getAffectedVert([X|_], PR, PR, [X]) :- var(X); integer(X).
getAffectedVert([_|Xs], Row, PR, Ret) :-
    Row =\= PR,
    NewRow is Row + 1,
    getAffectedVert(Xs, NewRow, PR, Ret).

getAffectedDownMain([], _, _-_, Ret, Ret).
getAffectedDownMain([X|Xs], Line, PL-PR, Acc, Ret) :-
    NewLine is Line + 1,
    (   getAffectedVert(X, 1, PR, List),
        append(Acc, List, Sum),
        getAffectedDownMain(Xs, NewLine, PL-PR, Sum, Ret)
    ;   Ret = Acc).

getAffectedUpMain([X|Xs], PL, PL-PR, Up, Total) :-
    getAffectedLine(X, 1, PR, [], [], Left, Right),
    NewLine is PL + 1,
    reverse(Up, ReverseUp),
    reverse(Left, ReverseLeft),
    getAffectedDownMain(Xs, NewLine, PL-PR, [], Down),
    Total = [ReverseUp, ReverseLeft, Right, Down].
getAffectedUpMain([X|Xs], Line, PL-PR, Acc, Ret) :-
    NewLine is Line + 1,
    (   getAffectedVert(X, 1, PR, List),
        append(Acc, List, Sum),
        getAffectedUpMain(Xs, NewLine, PL-PR, Sum, Ret)
    ;   getAffectedUpMain(Xs, NewLine, PL-PR, [], Ret)).

getAffectedLine([], _, _, Left, Right, Left, Right).
getAffectedLine([_|Xs], PR, PR, Left, Right, LfTotal, RtTotal) :-
    NewRow is PR + 1,
    getAffectedLine(Xs, NewRow, PR, Left, Right, LfTotal, RtTotal).
getAffectedLine([X|Xs], Row, PR, Left, Right, LfTotal, RtTotal) :-
    Row < PR,
    NewRow is Row + 1,
    (   (var(X); integer(X)),
        append(Left, [X], Sum),
        getAffectedLine(Xs, NewRow, PR, Sum, Right, LfTotal, RtTotal)
    ;   getAffectedLine(Xs, NewRow, PR, [], Right, LfTotal, RtTotal)).
getAffectedLine([X|Xs], Row, PR, Left, Right, LfTotal, RtTotal) :-
    NewRow is Row + 1,
    Row > PR,
    (   (var(X); integer(X)),
        append(Right, [X], Sum),
        getAffectedLine(Xs, NewRow, PR, Left, Sum, LfTotal, RtTotal)
    ;   LfTotal = Left, RtTotal = Right).

restrict(Affected, Expected, Other, Total) :-
    automaton(Affected, _, Affected,
          [source(s), sink(s), sink(f)],
          [arc(s, Expected, s, [Count + 1]),
           arc(s, Other, f),
           arc(f, Expected, f),
           arc(f, Other, f)],
           [Count], [0], [Total]).

% pega as casas e aplica restricoes
constrainAll(Board, Line-Row-Value) :-
    getAffectedUpMain(Board, 1, Line-Row, [], [Up, Left, Right, Down]),
    restrict(Up, 1, 0, TotalUp),
    restrict(Left, 0, 1, TotalLeft),
    restrict(Right, 0, 1, TotalRight),
    restrict(Down, 1, 0, TotalDown),
    Value #= TotalUp + TotalLeft + TotalRight + TotalDown.

solvePuzzle(Board) :-
    getCoords(Board, 1, 1, [], AllNumbers),
    maplist(constrainAll(Board), AllNumbers).

puzzle(N) :-
    board(N, X),
    append(X, V),
    include(var, V, Vars),
    domain(Vars, 0, 1),
    solvePuzzle(X),
    reset_timer,
    labeling([], Vars),
    viewBoard(X),nl,
    print_time,
    fd_statistics.

reset_timer :- statistics(walltime,_).
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.
