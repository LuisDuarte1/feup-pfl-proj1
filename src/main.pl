
% List access
nth_list([H| List], 0, Out) :- Out=H.
nth_list([H| List], Index, Out) :- 
    NewIndex is Index-1,
    nth_list(List, NewIndex, Out).

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