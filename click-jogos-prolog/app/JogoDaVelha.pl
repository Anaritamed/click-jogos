:- dynamic tabuleiro/1.

% Importação de bibliotecas simulada através de definições de predicados
:- use_module(library(lists)).

% Representação de tabuleiro vazio
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

% Tabuleiro inicial com 9 espaços vazios
tabuleiro_inicial([V,V,V,V,V,V,V,V,V]) :- vazio(V).

% Função para pegar partes do tabuleiro
pega_partes_do_tabuleiro(_, [], []).
pega_partes_do_tabuleiro(N, Lista, [Parte|Partes]) :-
    length(Parte, N),
    append(Parte, Resto, Lista),
    pega_partes_do_tabuleiro(N, Resto, Partes).

% Imprime partes do tabuleiro
printa_partes([]).
printa_partes([Parte]) :-
    maplist(writeln, Parte).
printa_partes([Parte|Resto]) :-
    maplist(writeln, Parte),
    writeln('-------------------'),
    printa_partes(Resto).

% Função para transpor a matriz
transpor([[]|_], []) :- !.
transpor(Lista, [Linha|Linhas]) :-
    maplist(nth1(1), Lista, Linha),
    maplist(resto, Lista, Resto),
    transpor(Resto, Linhas).

resto([_|Xs], Xs).

% Atualiza o tabuleiro com o jogador e posição
atualiza_tabuleiro(Tabuleiro, Jogador, Posicao, TabuleiroAtualizado) :-
    nth1(Posicao, Tabuleiro, _, TabuleiroRestante),
    ( Jogador = 'X' -> xis(X) ; bola(X) ),
    nth1(Posicao, TabuleiroAtualizado, X, TabuleiroRestante).

% Checa se o jogador venceu
checa_vitoria(Tabuleiro, Jogador) :-
    ( Jogador = 'X' -> xis(X) ; bola(X) ),
    padroes_vitoria(Padroes),
    member(Padrao, Padroes),
    maplist(nth1_pos(Tabuleiro), Padrao, Blocos),
    maplist(=(X), Blocos).

nth1_pos(Tabuleiro, Posicao, Bloco) :-
    nth1(Posicao, Tabuleiro, Bloco).

% Padrões de vitória no jogo
padroes_vitoria([
    [1, 2, 3], [4, 5, 6], [7, 8, 9],
    [1, 4, 7], [2, 5, 8], [3, 6, 9],
    [1, 5, 9], [3, 5, 7]
]).

% Reinicia o jogo
reinicia_jogo :-
    writeln("Reiniciando o jogo em 3"),
    sleep(1),
    writeln("2"),
    sleep(1),
    writeln("1"),
    sleep(1),
    iniciar_jogo.

% Checa se o tabuleiro está completo (empate)
checa_empate(Tabuleiro) :-
    maplist(\=(vazio), Tabuleiro).

% Lê a entrada do jogador
pega_input(Tabuleiro, Jogador, Posicao) :-
    format("Vez do jogador ~w. Insira seu movimento (1-9): ", [Jogador]),
    read(Pos),
    ( integer(Pos), between(1, 9, Pos), \+ posicao_ocupada(Tabuleiro, Pos) -> Posicao = Pos
    ; writeln("Entrada inválida!"),
      pega_input(Tabuleiro, Jogador, Posicao)
    ).

% Verifica se a posição já está ocupada
posicao_ocupada(Tabuleiro, Posicao) :-
    nth1(Posicao, Tabuleiro, P),
    vazio(V),
    P \= V.

% Função principal de controle do jogo
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

% Inicializa o jogo
iniciar_jogo :-
    tabuleiro_inicial(Tabuleiro),
    game_loop(Tabuleiro, 'X').