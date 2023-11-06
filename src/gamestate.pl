

% Player, Red: 0, Blue: 1
% Type: Two-player game: 
% GameState -> (Board, Type, Player)

% +WinCondition, +Player
stop_game(-1, Player).
stop_game(Player, Player) :- 
    write('You won!\n'),
    abort.
stop_game(Player1, Player2) :- 
    write('You lost :/\n'),
    abort.

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

% run game with random ai -> consult('src/main.pl'), init_board(_Board), run_game((_Board,0,0)).
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

% on type 1, we have a random move AI
% player 0 - human player; player 1 - AI
% test game with random ai -> consult('src/main.pl'), init_board(_Board), run_game((_Board,1,0)).
% +GameState
run_game((Board, 1, 0)) :- 
    set_prolog_flag(syntax_errors, dec10),
    draw_board(Board),
    try_move(Board, 0, NewBoard),
    check_win_condition(NewBoard, 0, WinCondition),
    !,
    stop_game(WinCondition, 0),
    run_game((NewBoard, 1, 1)).

run_game((Board, 1, 1)) :-
    random_move_ai(Board, 1, NewBoard),
    check_win_condition(NewBoard, 1, WinCondition),
    stop_game(WinCondition, 1),
    run_game((NewBoard, 1, 0)).


%minimax AI type 2
% +GameState
% test game with minimax ai -> consult('src/main.pl'), init_board(_Board), run_game((_Board,2,0)).
run_game((Board, 2, 0)) :- 
    set_prolog_flag(syntax_errors, dec10),
    draw_board(Board),
    try_move(Board, 0, NewBoard),
    check_win_condition(NewBoard, 0, WinCondition),
    !,
    stop_game(WinCondition, 0),
    run_game((NewBoard, 2, 1)).

run_game((Board, 2, 1)) :-
    greedy_ai(Board, 1, NewBoard),
    check_win_condition(NewBoard, 1, WinCondition),
    stop_game(WinCondition, 1),
    run_game((NewBoard, 2, 0)).