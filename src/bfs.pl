
:- use_module(library(lists)).
calculate_adjacent((Q1,R1),(Qb, Rb), (Qo, Ro)) :- Qo is Qb+Q1, Ro is Rb+R1.
calculate_adjacent_dist((Q1,R1,Distance),(Qb, Rb), (Qo, Ro)) :- Qo is Qb+Q1, Ro is Rb+R1.


% in axial form
check_tile_does_exist(Board, (Q1,R1)) :- 
    Q1 >= 0, Q1 =< 10, R1 >= -3, R1 =< 3,  get_board_piece_axial(Board,Q1,R1,Piece), Piece \= -1.

% in axial form
get_adjacent(Board, Q1, R1, AdjacentList) :-
    Offsets = [(-1,1), (0,1),(-1,0),(1,0),(0,-1), (1,-1)],
    maplist(call(calculate_adjacent_dist,(Q1,R1,0)), Offsets, IntermediateList),
    include(call(check_tile_does_exist, Board), IntermediateList, AdjacentList).

%in axial form
filter_adjacent(Board, Visited, (Q2,R2), (Q2,R2)).
filter_adjacent(Board, Visited, (Q2,R2), (Qi,Ri)) :- 
    get_board_piece_axial(Board, Qi, Ri, Piece), Piece = 0, neg(memberchk((Qi,Ri), Visited)). 

%in axial form
check_path_possible(Board, (Q1,R1), (Q2,R2)) :-
    get_board_piece_axial(Board, Q1, R1, Piece),  
    normalize_board_piece(Piece, NormalizedPiece),
    check_path_possible_bfs(Board, (Q2,R2), [], [(Q1,R1,NormalizedPiece)]).


add_distance_to_tuple(Distance, (Q1,R1), ReturnTuple) :- ReturnTuple = (Q1,R1,Distance).


flatten([], []) :- !.
flatten([X|Xs], FlatX) :-
    !,
    flatten(X, NewX),
    flatten(Xs, NewXs),
    append(NewX, NewXs, FlatX).
flatten(X, [X]).

%in axial form

traverse_node(Board, Visited, (Q1,R1,Distance), NewList) :-
    Distance > 0,
    neg(memberchk((Q1,R1), Visited)),
    get_adjacent(Board, Q1,R1, AdjacentList),
    NewDistance is Distance-1,
    include(call(filter_adjacent, Board, Visited, (Q2,R2)), AdjacentList, IntQueue),
    maplist(call(add_distance_to_tuple, NewDistance), IntQueue, NewList)
    .

got_destination_node((Q2,R2), (Q2,R2,_)).

check_path_possible_bfs(_, _, _, []) :- fail.
check_path_possible_bfs(_, (Q2,R2), _, List) :- 
    memberchk((Q2,R2,_), List), !.
check_path_possible_bfs(Board, (Q2,R2), Visited, List) :-
    proper_length(List, Length),
    Length > 0,
    maplist(call(traverse_node, Board, Visited), List, Results),
    flatten(Results, ResultsFlat),
    append(List, Visited, NewVisted),
    check_path_possible_bfs(Board, (Q2,R2), NewVisted, ResultsFlat)
    .
