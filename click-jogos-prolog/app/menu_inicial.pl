:- module(menu_inicial, [inicio/0]).
:- [perguntados].

inicio :-
    exibe_tela_inicial,
    write("Digite uma opção: "),
    read_line_to_string(user_input, Opcao),
    processa_opcao_inicio(Opcao).

exibe_tela_inicial :-
    tela_inicial(Lines),
    maplist(writeln, Lines).

processa_opcao_inicio("1") :- menu.
processa_opcao_inicio("2") :- sair.
processa_opcao_inicio(_) :- 
    write("Opção inválida. Tente novamente.\n"),
    inicio.

sair :- 
    write("Saindo..."), 
    !.

menu :- 
    exibe_menu,
    write("Digite uma opção: "),
    read_line_to_string(user_input, Opcao),
    processa_opcao_menu(Opcao).

exibe_menu :-
    jogos(Lines),
    maplist(writeln, Lines).

% processa_opcao_menu("1") :- forca.
processa_opcao_menu("2") :- perguntados.
% processa_opcao_menu("3") :- jogoDaVelha.
processa_opcao_menu(_) :- 
    write("Opção inválida. Tente novamente.\n"),
    menu.

tela_inicial([
    "===================================================",
    "   ___ _ _      _       __                         ",
    "  / __\\ (_) ___| | __   \\ \\  ___   __ _  ___  ___ ",
    " / /  | | |/ __| |/ /    \\ \\/ _ \\ / _` |/ _ \\/ __|",
    "/ /___| | | (__|   <  /\\_/ / (_) | (_| | (_) \\__ \\",
    "\\____/|_|_|\\___|_|\\_\\ \\___/ \\___/ \\__, |\\___/|___/",
    "                                  |___/            ",
    "===================================================",
    "          MENU (1)        |        SAIR (2)        ",
    "===================================================",
    "                                                   "
]).

jogos([
    "                                                   ",
    "===================================================",
    "                ESCOLHA UM JOGO                    ",
    "===================================================",
    "                    FORCA (1)                      ",
    "                 PERGUNTADOS (2)                   ",
    "                JOGO DA VELHA (3)                  ",
    "===================================================",
    "                                                   "
]).