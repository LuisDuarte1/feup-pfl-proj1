

% Player, Red: 0, Blue: 1
% Type: Two-player game: 
% GameState -> (Board, Type, Player)

% +WinCondition, +Player
stop_game(-1, Player).
stop_game(Player, Player) :- 
    write('You won!\n'),
    !,
    fail.
stop_game(Player1, Player2) :- 
    write('You lost :/\n'),
    !,
    fail.

invert_player(0, 1).
invert_player(1, 0).

try_move(Board, Player, NewBoard) :-
    format('Player ~d plays.\n', [Player]),
    write('Select the piece you want to move: '),
    read(IntSourceMoveAtom),
    write('Select the destination tile: '),
    read(DestMoveAtom),
    coord_to_axial_board(IntSourceMoveAtom, Q1, R1),
    coord_to_axial_board(DestMoveAtom, Q2, R2),
    move_piece(Board, Q1, R1, Q2, R2, NewBoard, Player).

try_move(Board, Player, NewBoard) :- 
    write('Invalid move try again...\n'),
    try_move(Board, Player, NewBoard).


% run game -> consult('src/main.pl'), init_board(_Board), run_game((_Board,0,0)).
% +GameState
run_game((Board, 0, Player)) :-
    set_prolog_flag(syntax_errors, dec10),
    draw_board(Board),
    try_move(Board, Player, NewBoard),
    check_win_condition(NewBoard, Player, WinCondition),
    !,
    stop_game(WinCondition, Player),
    invert_player(Player, NewPlayer),
    run_game((NewBoard, 0, NewPlayer))
    .