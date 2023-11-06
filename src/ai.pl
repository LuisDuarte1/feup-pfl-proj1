:- use_module(library(between)).
:- use_module(library(ordsets)).
:- use_module(library(random)).

filter_valid_move(Board, Player, (Q1,R1)-(Q1,R1)) :- fail.
filter_valid_move(Board, Player, (Q1,R1)-(Q2,R2)) :- 
    get_board_piece(Board, Q1, R1, Piece), Piece > 0,
    belongs_player(Player, Piece),
    get_board_piece(Board, Q2, R2, DestinationPiece), DestinationPiece > -1,
    (DestinationPiece = 0; neg(belongs_player(Player, DestinationPiece))),
    distance_offset(Q1,R1, Q2,R2, Distance),
    normalize_board_piece(Piece, NormalizedPiece),
    Distance =< NormalizedPiece,
    convert_offset_to_axial(Q1,R1,QFrom,RFrom),
    convert_offset_to_axial(Q2,R2,QTo,RTo),
    normalize_board_piece(DestinationPiece, DestinationNormalizedPiece),
    attack_checker(NormalizedPiece, DestinationNormalizedPiece, State),
    State > -1,
    %format("1st: ~d/~d 2nd: ~d/~d\n",[QFrom,RFrom,QTo,RTo]),
    check_path_possible(Board, (QFrom, RFrom), (QTo, RTo))
    .

% in offset form
% test func: ['src/main'], init_board(_Board), find_all_valid_moves(_Board,0,Moves).
%+Board +Player -Moves
find_all_valid_moves(Board, Player, Moves) :- 
    findall((Q,R), (between(0, 10, Q), between(0,6,R)), _ValidCoordinates),
    list_to_ord_set(_ValidCoordinates, _ValidCoordinatesSet),
    ord_setproduct(_ValidCoordinatesSet, _ValidCoordinatesSet, AllMoves),
    include(call(filter_valid_move, Board, Player), AllMoves, Moves).


%+Board +Player -NewBoard
random_move_ai(Board, Player, NewBoard) :-
    find_all_valid_moves(Board, Player, Moves),
    random_member((Q1,R1)-(Q2,R2), Moves),
    convert_offset_to_axial(Q1,R1,QFrom, RFrom),
    convert_offset_to_axial(Q2,R2,QTo, RTo),
    move_piece(Board, QFrom, RFrom, QTo, RTo, NewBoard, Player)
    .


filter_golden_move((Q1,R1)-(5,1)).
filter_golden_move((Q1,R1)-(5,5)).
filter_golden_move((Q1,R1)-(Q2,R2)) :- fail.


value(0, Board, Value):-
    check_win_condition(Board, Player, State),
    State =:= 0,
    Value is -100.

value(1, Board, Value):-
    check_win_condition(Board, Player, State),
    State =:= 1,
    Value is 100.

value(Player,Board, Value):-
    check_win_condition(Board, Player, State),
    State =:= -1,
    find_all_valid_moves(Board, Player, OwnPlayerMoves),
    include(filter_golden_move, OwnPlayerMoves, OwnGoldenMoves),
    invert_player(Player, EnemyPlayer),
    find_all_valid_moves(Board, EnemyPlayer, EnemyPlayerMoves),
    include(filter_golden_move, EnemyPlayerMoves, EnemyGoldenMoves),
    proper_length(OwnGoldenMoves, OwnMoves),
    proper_length(EnemyGoldenMoves, EnemyMoves),
    Value is OwnMoves - EnemyMoves.

make_greedy_move(Player, Board,(Q1,R1)-(Q2,R2),NewBoard) :-
    convert_offset_to_axial(Q1,R1,QFrom, RFrom),
    convert_offset_to_axial(Q2,R2,QTo, RTo),
    move_piece(Board, QFrom, RFrom, QTo, RTo, NewBoard, Player).

cmp_evalboards((_, Eval1), (_, Eval2)) :- Eval1 =< Eval2.

greedy_ai(Board, Player, NewBoard) :- 
    find_all_valid_moves(Board, Player, Moves),
    maplist(call(make_greedy_move, Player, Board), Moves, NewBoards),
    maplist(call(value, Player), NewBoards, Evals),
    zip(NewBoards, Evals, EvalBoards),
    max_member(cmp_evalboards, (NewBoard,_), EvalBoards).

