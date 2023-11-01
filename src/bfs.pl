
:- use_module(library(lists)).
calculate_adjacent((Q1,R1),(Qb, Rb), (Qo, Ro)) :- Qo is Qb+Q1, Ro is Rb+R1.

% in axial form
check_tile_does_exist(Board, (Q1,R1)) :- 
    Q1 >= 0, Q1 =< 10, R1 >= -3, R1 =< 3,  get_board_piece_axial(Board,Q1,R1,Piece), Piece \= -1.

% in axial form
get_adjacent(Board, Q1, R1, AdjacentList) :-
    Offsets = [(-1,1), (0,1),(-1,0),(1,0),(0,-1), (1,-1)],
    maplist(call(calculate_adjacent,(Q1,R1)), Offsets, IntermediateList),
    include(call(check_tile_does_exist, Board), IntermediateList, AdjacentList).


