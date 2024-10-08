limpar_tela :-
    write('\e[H\e[2J').

iniciar_jogo :-
    retractall(tabuleiro(_)),
    assert(tabuleiro(['_', '_', '_', '_', '_', '_', '_', '_', '_'])),
    exibir_boas_vindas,
    escolher_opcao.

exibir_boas_vindas :-
    writeln("============================================================================"),
    writeln("      _                                _         __     __   _ _              "),
    writeln("     | | ___   __ _  ___      __| | __ _  \\ \\   / /__| | |__   __ _         "),
    writeln("  _  | |/ _ \\ / _` |/ _ \\    / _` |/ _` |  \\ \\ / / _ \\ | '_ \\ / _` |   "),
    writeln(" | |_| | (_) | (_| | (_) |  | (_| | (_| |   \\ V /  __/ | | | | (_| |         "),
    writeln("  \\___/ \\___/ \\__, |\\___/    \\__,_|\\__,_|    \\_/ \\___|_|_| |_|\\__,_| "),
    writeln("               |___/                                                   "),
    writeln("============================================================================"),
    writeln("                           SEJA BEM VINDO!                                 "),
    writeln("                                                                             ").

escolher_opcao :-
    writeln('1. JOGAR'),
    writeln('2. SAIR DO JOGO'),
    read(Escolha),
    (
        Escolha = 1 -> iniciar_jogada;
        Escolha = 2 -> writeln('Saindo do jogo...'), halt;
        writeln('Opção inválida!'), escolher_opcao
    ).

exibir_tabuleiro :-
    tabuleiro(T),
    formatar_tabuleiro(T, Formatado),
    exibir_linhas(Formatado).

formatar_tabuleiro(Tabuleiro, Formatado) :-
    dividir_em_partes(3, Tabuleiro, Formatado).

dividir_em_partes(_, [], []).
dividir_em_partes(N, Lista, [Parte|Resto]) :-
    length(Parte, N),
    append(Parte, Cauda, Lista),
    dividir_em_partes(N, Cauda, Resto).

exibir_linhas([]).
exibir_linhas([Linha|Resto]) :-
    writeln(Linha),
    exibir_linhas(Resto).

iniciar_jogada :-
    tabuleiro(Tabuleiro),
    loop_jogada(Tabuleiro, 'X').

loop_jogada(Tabuleiro, Jogador) :-
    limpar_tela,
    exibir_tabuleiro,
    writef('\nJogador %w, insira sua jogada (1-9): ', [Jogador]),
    read(Pos),
    (jogada_valida(Tabuleiro, Pos) ->
        atualizar_tabuleiro(Tabuleiro, Jogador, Pos, TabuleiroAtualizado),
        (
            verificar_vencedor(TabuleiroAtualizado, Jogador) -> limpar_tela, exibir_tabuleiro, writef('Jogador %w vence!\n', [Jogador]), reiniciar_jogo;
            verificar_empate(TabuleiroAtualizado) -> limpar_tela, exibir_tabuleiro, writeln('Empate!'), reiniciar_jogo;
            proximo_jogador(Jogador, ProximoJogador), loop_jogada(TabuleiroAtualizado, ProximoJogador)
        );
        writeln('Movimento inválido! Tente novamente.'), loop_jogada(Tabuleiro, Jogador)
    ).

jogada_valida(Tabuleiro, Pos) :-
    integer(Pos),
    Pos >= 1, Pos =< 9,
    nth1(Pos, Tabuleiro, Celula),
    Celula == '_', !.
jogada_valida(_, _) :-
    fail.

atualizar_tabuleiro(Tabuleiro, Jogador, Pos, TabuleiroAtualizado) :-
    nth1(Pos, Tabuleiro, _, Resto),
    nth1(Pos, TabuleiroAtualizado, Jogador, Resto),
    retract(tabuleiro(_)),
    assert(tabuleiro(TabuleiroAtualizado)).

verificar_vencedor(Tabuleiro, Jogador) :-
    combinacoes_vencedoras(Combinacoes),
    member(Combinacao, Combinacoes),
    verificar_combinacao(Tabuleiro, Combinacao, Jogador).

verificar_combinacao(Tabuleiro, [A, B, C], Jogador) :-
    nth1(A, Tabuleiro, Jogador),
    nth1(B, Tabuleiro, Jogador),
    nth1(C, Tabuleiro, Jogador).

combinacoes_vencedoras([
    [1, 2, 3], [4, 5, 6], [7, 8, 9],  % Linhas
    [1, 4, 7], [2, 5, 8], [3, 6, 9],  % Colunas
    [1, 5, 9], [3, 5, 7]              % Diagonais
]).

verificar_empate(Tabuleiro) :-
    \+ member('_', Tabuleiro).

proximo_jogador('X', 'O').
proximo_jogador('O', 'X').

reiniciar_jogo :-
    writeln('Reiniciando o jogo em 3...'), sleep(1),
    writeln('Reiniciando o jogo em 2...'), sleep(1),
    writeln('Reiniciando o jogo em 1...'), sleep(1),
    limpar_tela,
    iniciar_jogo.

vazio(['    ']).