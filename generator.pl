:- use_module(library(random)).
:- use_module(library(between)).

:-include('walls.pl').

createRow(0, []).
createRow(N, [_|Rest]) :-
   Next is N - 1,
   createRow(Next, Rest).

createEmptyBoard(_, 0, []).
createEmptyBoard(L, N, [Row|Rest]) :-
    Next is N - 1,
    createRow(L, Row),
    createEmptyBoard(L, Next, Rest).

fillBoardAux(E, B, N, ClueSum) :-
    Limit is N + 1,
    random(1, Limit, Line),
    random(1, Limit, Row),
    getAffectedUpMain(E, 1, 1, Line-Row, [], Affected).

fillBoard(E, B, N) :-
    fillBoardAux(E, B, N, 0).

createBoard(N, Board) :-
    N > 1,
    createEmptyBoard(N, N, EmptyBoard),
    fillBoard(EmptyBoard, Board, N).
