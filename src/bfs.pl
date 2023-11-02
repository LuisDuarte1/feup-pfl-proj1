
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

%in axial form
filter_adjacent(Board, (Q2,R2), (Q2,R2)).
filter_adjacent(Board, (Q2,R2), (Qi,Ri)) :- get_board_piece_axial(Board, Qi, Ri, Piece), Piece = 0. 

%in axial form
check_path_possible(Board, (Q1,R1), (Q2,R2)) :-
    distance_axial(Q1, R1, Q2, R2, Distance),
    check_path_possible_bfs(Board, (Q2,R2), Distance,[], [(Q1,R1)]).


%in axial form
check_path_possible_bfs(_, (Q2,R2), _, _, [(Q2,R2) | _]) :- !.
check_path_possible_bfs(_, (Q2,R2), 0, _, [(Q1,R1) | _]) :- fail.
check_path_possible_bfs(Board, (Q2,R2),Distance, Visited, [(Q1,R1) | RestQueue]) :-
    neg(memberchk((Q1,R1), Visited)),
    get_adjacent(Board, Q1,R1, AdjacentList),
    NewDistance is Distance-1,
    append([(Q1,R1)], Visited, NewVisited),
    include(call(filter_adjacent, Board, (Q2,R2)), AdjacentList, TailQueue),
    append(RestQueue, TailQueue, NewQueue),
    check_path_possible_bfs(Board, (Q2,R2), NewDistance, NewVisited, NewQueue).
