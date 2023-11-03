
% ui needs a font that support symbols for legacy computing (eg.: Iosevka)
% test func: ['src/main'], init_board(_Board), draw_board(_Board).

upper_forward_diagonal_line :- put_code(129952).
upper_backward_diagonal_line :- put_code(129953).
straight_line :- put_code(129912).
right_both_diagonal_line :- put_code(129956).
left_both_diagonal_line :- put_code(129957). 
down_forward_diagonal_line :- put_code(129955).
down_backward_diagonal_line :- put_code(129954).


bell :- put_code(0x07).

clear_screen :- write('\33\[2J').

set_cursor_pos(X,Y) :- format('\33\[~d;~dH', [Y,X]).

save_cursor_pos :- write('\33\[s').
load_cursor_pos :- write('\33\[u').


reset_color :- write('\33\[39m').
set_red_color :- write('\33\[31m').
set_blue_color :- write('\33\[34m').

% circle
draw_piece((X,Y), 1) :- set_red_color, set_cursor_pos(X,Y), put_code(9679), reset_color.
draw_piece((X,Y), 6) :- set_blue_color, set_cursor_pos(X,Y), put_code(9679), reset_color.

% triangle
draw_piece((X,Y), 3) :- set_red_color, set_cursor_pos(X,Y), put_code(9650), reset_color.
draw_piece((X,Y), 8) :- set_blue_color, set_cursor_pos(X,Y), put_code(9650), reset_color.

% square
draw_piece((X,Y), 4) :- set_red_color, set_cursor_pos(X,Y), put_code(9632), reset_color.
draw_piece((X,Y), 9) :- set_blue_color, set_cursor_pos(X,Y), put_code(9632), reset_color.

% pentagon
draw_piece((X,Y), 5) :- set_red_color, set_cursor_pos(X,Y), put_code(11039), reset_color.
draw_piece((X,Y), 10) :- set_blue_color, set_cursor_pos(X,Y), put_code(11039), reset_color.

draw_piece((X,Y), Piece).

%in offset form
get_center_coordinate((Q,R), (X,Y)) :- R mod 2 =:= 0,
        X is 29 - (4*R + 3) + 3,
        Y is 2*Q + 2 + 3.

get_center_coordinate((Q,R), (X,Y)) :- R mod 2 =:= 1,
        X is 29 - (4*R + 3) + 3,
        Y is 2*Q + 1 + 3.


diff_coordinates((Qi,Ri), (Q,R), (Qo,Ro)) :- Qo is Q - Qi, Ro is R - Ri.


corner_connection((X,Y), (1,0)) :- set_cursor_pos(X+2, Y+1), right_both_diagonal_line, set_cursor_pos(X-2, Y+1), left_both_diagonal_line.
corner_connection((X,Y), (-1,0)) :- set_cursor_pos(X+2, Y-1), right_both_diagonal_line, set_cursor_pos(X-2, Y-1), left_both_diagonal_line.
corner_connection((X,Y), (1,-1)) :- set_cursor_pos(X-2, Y), right_both_diagonal_line.
corner_connection((X,Y), (-1,1)) :- set_cursor_pos(X+2, Y-1), right_both_diagonal_line.
corner_connection((X,Y), (0,1)) :- set_cursor_pos(X+2, Y+1), right_both_diagonal_line.
corner_connection((X,Y), (0,-1)) :- set_cursor_pos(X-2, Y), right_both_diagonal_line.
corner_connection((X,Y), (Qoff,Roff)).

draw_hexagon(Board, (Q,R)) :- 
        get_board_piece(Board, Q,R, Piece),
        Piece > -1,
        get_center_coordinate((Q,R), (X,Y)),
        draw_piece((X,Y), Piece),
        % draw straight lines - up
        set_cursor_pos(X,Y-1),
        straight_line,
        set_cursor_pos(X+1,Y-1),
        straight_line,
        set_cursor_pos(X-1,Y-1),
        straight_line,
        % draw straight lines - down
        set_cursor_pos(X,Y+1),
        straight_line,
        set_cursor_pos(X+1,Y+1),
        straight_line,
        set_cursor_pos(X-1,Y+1),
        straight_line,
        % corners
        set_cursor_pos(X+2,Y-1),
        down_backward_diagonal_line,
        set_cursor_pos(X-2,Y-1),
        down_forward_diagonal_line,
        set_cursor_pos(X+2,Y+1),
        upper_forward_diagonal_line,
        set_cursor_pos(X-2,Y+1),
        upper_backward_diagonal_line,
        % middle vertices
        set_cursor_pos(X+2,Y),
        left_both_diagonal_line,
        set_cursor_pos(X-2,Y),
        right_both_diagonal_line,
        % see if corner needs a connection
        convert_offset_to_axial(Q,R,AxialQ,AxialR),
        get_adjacent(Board, AxialQ, AxialR, AdjacentList),
        maplist(call(diff_coordinates, (AxialQ,AxialR)), AdjacentList, DiffList),
        % format('Q:~d R: ~d : ~p\n', [AxialQ, AxialR, DiffList])
        maplist(call(corner_connection, (X,Y)), DiffList)
        .
draw_hexagon(Board, (Q,R)).

draw_row(Board, R, 11).
draw_row(Board, R, CurrQ) :-
        NewQ is CurrQ+1,
        draw_hexagon(Board, (CurrQ, R)),
        draw_row(Board, R, NewQ).

draw_board_rec(Board, 7).
draw_board_rec(Board, CurrR) :-
        NewR is CurrR+1,
        draw_row(Board, CurrR, 0),
        draw_board_rec(Board, NewR).

draw_board(Board) :- 
        save_cursor_pos,
        clear_screen,
        draw_board_rec(Board, 0),
        load_cursor_pos.