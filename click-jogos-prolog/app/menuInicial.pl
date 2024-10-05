:- [perguntados].

exibe_tela_inicial :-
    tela_inicial(Lines),
    maplist(writeln, Lines).

exibe_menu :-
    jogos(Lines),
    maplist(writeln, Lines).
    
inicio :- 
    exibe_tela_inicial,
    write("Digite uma opcao: "),
    read_line_to_string(user_input, Opcao),
    processaOpcaoInicio(Opcao).

processaOpcaoInicio("1") :- menu.
processaOpcaoInicio("2") :- sair.
processaOpcaoInicio(_) :- 
    write("Opcao invalida. Tente novamente.\n"),
    inicio.

sair :- 
    write("Saindo..."), 
    !.

menu :- 
    exibe_menu,
    write("Digite uma opcao: "),
    read_line_to_string(user_input, Opcao),
    processaOpcaoMenu(Opcao).

% processaOpcaoMenu("1") :- forca.
% processaOpcaoMenu("3") :- jogoDaVelha.
processaOpcaoMenu("2") :- perguntados.
processaOpcaoMenu(_) :- 
    write("Opcao invalida. Tente novamente.\n"),
    menu.

% Definição da tela inicial como lista de strings
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

% Definição da tela de escolha de jogos como lista de strings
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

:-inicio.