:- dynamic tabuleiro/1.
:- use_module(library(system)).

jogo_da_velha :-
    limpa_terminal,
    writeln("============================================================================"),
    writeln("      _                         _        __     __   _ _                    "),
    writeln("     | | ___   __ _  ___     __| | __ _  \\ \\   / /__| | |__   __ _        "),
    writeln("  _  | |/ _ \\ / _` |/ _ \\   / _` |/ _` |  \\ \\ / / _ \\ | '_ \\ / _` |   "),
    writeln(" | |_| | (_) | (_| | (_) | | (_| | (_| |   \\ V /  __/ | | | | (_| |        "),
    writeln("  \\___/ \\___/ \\__, |\\___/   \\__,_|\\__,_|    \\_/ \\___|_|_| |_|\\__,_|"),
    writeln("              |___/                                                         "),
    writeln("============================================================================"),
    writeln("                            SEJA BEM VINDO!                                 "),
    writeln("                                                                            "),
    writeln("                              (1) JOGAR                                     "),
    writeln("                           (2) SAIR DO JOGO                                 "),
    writeln("                                                                            "),
    read_line_to_string(user_input, Opcao),
    (Opcao = "1" ->
        iniciar_jogo
    ; Opcao = "2" ->
        writeln("Saindo do jogo..."),
        sleep(0.5),
        halt
    ; 
        writeln("Opção inválida!"),
        sleep(0.5),
        jogo_da_velha
    ).

limpa_terminal :-
    writeln('\e[H\e[2J').

:- use_module(library(lists)).

vazio([
    "                 ",
    "                 ",
    "                 ",
    "                 ",
    "                 ",
    "                 "
]).

bola([
    "     OOOOOOO     ",
    "   O         O   ",
    "  O           O  ",
    "  O           O  ",
    "   O         O   ",
    "     OOOOOOO     "
]).

xis([
    "    \\\\    //     ",
    "     \\\\  //      ",
    "      \\\\//       ",
    "      //\\\\       ",
    "     //  \\\\      ",
    "    //    \\\\     "
]).


tabuleiro_inicial([V,V,V,V,V,V,V,V,V]) :- vazio(V).

pega_partes_do_tabuleiro(_, [], []).
pega_partes_do_tabuleiro(N, Lista, [Parte|Partes]) :-
    length(Parte, N),
    append(Parte, Resto, Lista),
    pega_partes_do_tabuleiro(N, Resto, Partes).

printa_partes([]).
printa_partes([Parte]) :-
    maplist(writeln, Parte).
printa_partes([Parte|Resto]) :-
    maplist(writeln, Parte),
    writeln('-------------------'),
    printa_partes(Resto).

transpor([[]|_], []) :- !.
transpor(Lista, [Linha|Linhas]) :-
    maplist(nth1(1), Lista, Linha),
    maplist(resto, Lista, Resto),
    transpor(Resto, Linhas).

resto([_|Xs], Xs).

atualiza_tabuleiro(Tabuleiro, Jogador, Posicao, TabuleiroAtualizado) :-
    nth1(Posicao, Tabuleiro, _, TabuleiroRestante),
    ( Jogador = 'X' -> xis(X) ; bola(X) ),
    nth1(Posicao, TabuleiroAtualizado, X, TabuleiroRestante).

checa_vitoria(Tabuleiro, Jogador) :-
    ( Jogador = 'X' -> xis(X) ; bola(X) ),
    padroes_vitoria(Padroes),
    member(Padrao, Padroes),
    maplist(nth1_pos(Tabuleiro), Padrao, Blocos),
    maplist(=(X), Blocos).

nth1_pos(Tabuleiro, Posicao, Bloco) :-
    nth1(Posicao, Tabuleiro, Bloco).

padroes_vitoria([
    [1, 2, 3], [4, 5, 6], [7, 8, 9],
    [1, 4, 7], [2, 5, 8], [3, 6, 9],
    [1, 5, 9], [3, 5, 7]
]).

reinicia_jogo :-
    writeln("Reiniciando o jogo em 3"),
    sleep(1),
    writeln("2"),
    sleep(1),
    writeln("1"),
    sleep(1),
    iniciar_jogo.

checa_empate(Tabuleiro) :-
    maplist(\=(vazio), Tabuleiro).

pega_input(Tabuleiro, Jogador, Posicao) :-
    format("Vez do jogador ~w. Insira seu movimento (1-9): ", [Jogador]),
    read(Pos),
    ( integer(Pos), between(1, 9, Pos), \+ posicao_ocupada(Tabuleiro, Pos) -> Posicao = Pos
    ; writeln("Entrada inválida!"),
      pega_input(Tabuleiro, Jogador, Posicao)
    ).

posicao_ocupada(Tabuleiro, Posicao) :-
    nth1(Posicao, Tabuleiro, P),
    vazio(V),
    P \= V.

game_loop(Tabuleiro, Jogador) :-
    pega_partes_do_tabuleiro(3, Tabuleiro, Partes),
    printa_partes(Partes),
    pega_input(Tabuleiro, Jogador, Posicao),
    atualiza_tabuleiro(Tabuleiro, Jogador, Posicao, TabuleiroAtualizado),
    ( checa_vitoria(TabuleiroAtualizado, Jogador) ->
        format("Jogador ~w vence!\n", [Jogador]),
        reinicia_jogo
    ; checa_empate(TabuleiroAtualizado) ->
        writeln("Temos um empate!"),
        reinicia_jogo
    ; JogadorProximo = (Jogador = 'X' -> 'O' ; 'X'),
      game_loop(TabuleiroAtualizado, JogadorProximo)
    ).

iniciar_jogo :-
    tabuleiro_inicial(Tabuleiro),
    game_loop(Tabuleiro, 'X').

:-jogo_da_velha.