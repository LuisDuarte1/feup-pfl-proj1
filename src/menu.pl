menu_player_choice(1) :- init_board(_Board), run_game((_Board,0,0)).

menu_player_choice(2) :- init_board(_Board), run_game((_Board,1,0)).

menu_player_choice(3) :- init_board(_Board), run_game((_Board,2,0)).

menu_player_choice(4) :- abort.

menu_player_choice(_):-
    write('Invalid choice. Please enter 1, 2, or 3.'),
    menu.

menu :-
        clear_screen,                                                                                                                                                           
        write('                                                                                   \n'),
        write('                                   welcome to tactilog!                                      \n'),                                    
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('\n'),
        write('                                                                                   \n'),
        write('                      Select the number of your option                             \n'),
        write('                                                                                   \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                |           1 - Player VS Player              |                    \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                |          2 -  Player VS RandoLog            |                    \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                |         3 -  Player VS MinimaXpert          |                    \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                 ---------------------------------------------                     \n'),
        write('                |                4 -  Quit                    |                    \n'),
        write('                 ---------------------------------------------                     \n'),
        read(X),
        menu_player_choice(X).
