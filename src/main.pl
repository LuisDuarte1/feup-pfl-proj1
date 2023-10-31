% import board facts and display rules
:- ['board.pl'].

% List access
nth_list([H| List], 0, Out) :- Out=H.
nth_list([H| List], Index, Out) :- 
    NewIndex is Index-1,
    nth_list(List, NewIndex, Out).

% List mutation

replace_list(0, NewValue, [_ | List], [NewValue | List]).
replace_list(Index, NewValue, [Head | List], [Head | NewList]) :- 
    Index > 0, 
    NewIndex is Index-1,
    replace_list(NewIndex, NewValue, List, NewList).

/*
 board:
    -1 -> not a valid space
    0 -> empty space
    1 || 6 -> circle
    3 || 8 -> triangle
    4 || 9 -> square
    5 || 10 -> pentagon

    1-5 -> red
    6-10-> blue
*/

% -Board
init_board(Board) :- append([],[
        [-1,-1,-1,-1, 0, 0,-1,-1,-1,-1,-1], 
        [-1, 1, 0, 1, 0, 0, 0, 6, 0, 6,-1], 
        [ 0, 4, 3, 0, 0, 0, 0, 8, 9, 0,-1], 
        [ 1, 3, 5, 4, 1, 0, 6, 9,10, 8, 6], 
        [ 0, 4, 3, 0, 0, 0, 0, 8, 9, 0,-1], 
        [-1, 1, 0, 1, 0, 0, 0, 6, 0, 6,-1], 
        [-1,-1,-1,-1, 0, 0,-1,-1,-1,-1,-1]
        ], Board).



% +Board +Q +R -Piece
get_board_piece(Board, Q, R, Piece) :- nth_list(Board, R, Row), nth_list(Row, Q, Piece).


% +Piece, -NormalizedPiece
normalize_board_piece(Piece, NormalizedPiece) :- Piece > 5,
                                                NormalizedPiece is Piece - 5.
normalize_board_piece(Piece, NormalizedPiece) :- Piece =< 5,
                                                NormalizedPiece is Piece.


% we use the axial cordinate system offseted with the rows offseted to 
% the middle so that the hexagon input is more intuitive

% +Q, +R, -AxialQ, -AxialR
convert_offset_to_axial(Q,R, AxialQ, AxialR) :- AxialR is -R+3,
                                                AxialQ is Q - (AxialR - (AxialR /\ 1)) // 2.


% +AxialQ, +AxialR, Q, R
convert_axial_to_offset(AxialQ, AxialR, Q, R) :-    R is -AxialR+3,
                                                    Q is AxialQ + (AxialR - (AxialR /\ 1)) // 2.


% according to the attack table, described in the report:

% s1 - attacker can attack and not be eaten = 1
% s2 - attacker can attack but is also eaten = 0
% s3 - attacker cannot attack = -1

% +attackerValue, +defenderValue, -state

% circle attack
attack_checker(1,1, 1).
attack_checker(1,3, 1).
attack_checker(1,4, 1).
attack_checker(1,5, 1).

% triangle attack
attack_checker(3,1, 0).
attack_checker(3,3, 1).
attack_checker(3,4, 1).
attack_checker(3,5, 1).

% square attack
attack_checker(4,1, -1).
attack_checker(4,3, 0).
attack_checker(4,4, 1).
attack_checker(4,5, 1).

% pentagon attack
attack_checker(5,1, -1).
attack_checker(5,3, -1).
attack_checker(5,4, -1).
attack_checker(5,5, 1).

% generic attack checker for empty slot, we assume that it "eats" nothing and it's not eaten on the process
attack_checker(X,0,1).

% distance function
% +AxialQ1 +AxialR1 +AxialQ2 +AxialR2 -Distance
distance_axial(AxialQ1, AxialR1, AxialQ2, AxialR2, Distance) :- AxialS1 is -AxialQ1-AxialR1,
                                                                AxialS2 is -AxialQ2-AxialR2,
                                                                Distance is (abs(AxialQ1 - AxialQ2) + abs(AxialR1 - AxialR2) + abs(AxialS1 - AxialS2))//2.
% +Q1 +R1 +Q2 +R2 -Distance
distance_offset(Q1,R1,Q2,R2,Distance) :-    convert_offset_to_axial(Q1,R1,AxialQ1,AxialR1),
                                            convert_offset_to_axial(Q2,R2,AxialQ2,AxialR2),
                                            distance_axial(AxialQ1,AxialR1,AxialQ2,AxialR2,Distance).


% commit_piece takes two coordinates, the attack state and board and acts accordingly
% this assumes offset coordinates.
% eats both, so we set both points to 0
% +Board +QFrom +RFrom, +QTo, +RTo, +State, -NewBoard
commit_piece(Board, QFrom, RFrom, QTo, RTo, 0, NewBoard) :-
    % in the source hexagon we replace it 0
    nth_list(Board, RFrom, FromRow),
    replace_list(QFrom, 0, FromRow, FromNewRow),
    replace_list(RFrom, FromNewRow, Board, InterBoard),
    % in the destination hexagon we replace it to 0
    nth_list(Board, RTo, ToRow),
    replace_list(QTo, 0, ToRow, ToNewRow),
    replace_list(RTo, ToNewRow, InterBoard, NewBoard).

% eats only the destination coordinate, so we set the source to 0 and the destination to the piece value
commit_piece(Board, QFrom, RFrom, QTo, RTo, 1, NewBoard) :-
    get_board_piece(Board, QFrom, RFrom, Piece),
    % in the source hexagon we replace it 0
    nth_list(Board, RFrom, FromRow),
    replace_list(QFrom, 0, FromRow, FromNewRow),
    replace_list(RFrom, FromNewRow, Board, InterBoard),
    % in the destination hexagon we replace it with the piece value
    nth_list(Board, RTo, ToRow),
    replace_list(QTo, Piece, ToRow, ToNewRow),
    replace_list(RTo, ToNewRow, InterBoard, NewBoard).


% move_piece will check all rules above to see if it's able the piece and make the move if possible
% this assumes that Q and R are in Axial form. ReturnCode is 0 on success or -1 on failure
% +Board +Qfrom +Rfrom +Qto +Rto -NewBoard
move_piece(Board, QFrom, RFrom, QTo, RTo, NewBoard) :-    
    convert_axial_to_offset(QFrom, RFrom, OffsetQFrom, OffsetRFrom),
    convert_axial_to_offset(QTo, RTo, OffsetQTo, OffsetRTo),
    get_board_piece(Board, OffsetQFrom, OffsetRFrom, Piece),
    Piece > 0,
    get_board_piece(Board, OffsetQTo, OffsetRTo, DestinationPiece),
    DestinationPiece > -1,
    (abs(Piece-DestinationPiece) >= 5; DestinationPiece = 0),
    normalize_board_piece(Piece, NormalizedPiece),
    normalize_board_piece(DestinationPiece, DestinationNormalizedPiece),
    attack_checker(NormalizedPiece, DestinationNormalizedPiece, State),
    State > -1,
    commit_piece(Board, OffsetQFrom, OffsetRFrom, OffsetQTo, OffsetRTo, State, NewBoard).