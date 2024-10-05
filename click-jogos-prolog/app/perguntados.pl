:- module(perguntados, [perguntados/0]).
:- use_module(library(readutil)).

perguntados :- 
    exibe_menu_perguntados,
    write("Digite uma opcao: "),
    read_line_to_string(user_input, Opcao),
    processaOpcaoMenu(Opcao).

processaOpcaoMenu("1") :- inicioJogo.
processaOpcaoMenu("2") :- sair.
processaOpcaoMenu(_) :- 
    write("Opcao invalida. Tente novamente.\n"),
    perguntados.

inicioJogo :- 
    exibe_regras_do_jogo,
    writeln("Digite o nome do jogador 1: "),
    read_line_to_string(user_input, Jogador1),
    writeln("Digite o nome do jogador 2: "),
    read_line_to_string(user_input, Jogador2),
    exibe_placar([Jogador1, Jogador2], [0, 0]),
    temaJogo([Jogador1, Jogador2]).

temaJogo(Jogadores) :-
    exibe_escolha_tema,
    write("Digite uma opcao: "),
    read_line_to_string(user_input, Opcao),
    processaTemaJogo(Opcao, Jogadores).

tema_disponivel("1", _, "entretenimento.txt").
tema_disponivel("2", _, "programacao.txt").
tema_disponivel("3", _, "geografia.txt").
tema_disponivel("4", _, "historia.txt").
tema_disponivel("5", _, "ciencias.txt").
tema_disponivel(_, Jogadores, _) :- 
    write("Opcao invalida. Tente novamente.\n"),
    temaJogo(Jogadores).

processaTemaJogo(Opcao, Jogadores) :-
    tema_disponivel(Opcao, Jogadores, Tema),
    jogo(Jogadores, Tema).

jogo(Jogadores, Tema) :-
    working_directory(CWD, CWD),  
    atom_concat(CWD, 'perguntas/', CaminhoBase),
    atom_concat(CaminhoBase, Tema, CaminhoArquivo), 
    open(CaminhoArquivo, read, Arquivo),
    read_lines(Arquivo, Linhas), 
    
    %O problema esta aqui - extrai_perguntas(Linhas, Perguntas) nao esta funcionando e entra em loop

    %extrai_perguntas(Linhas, Perguntas).
    %close(Arquivo),
    %quiz(Perguntas, Jogadores,[0, 0], 0, Resultado),
    %exibe_placar(Jogadores, Resultado),
    %mostra_vencedor(Jogadores, Resultado),
    %exibe_menu_jogar_novamente,
    %write("Digite uma opcao: "),
    %read_line_to_string(user_input, Opcao),
    %processaOpcaoJogarNovamente(Opcao).

processaOpcaoJogarNovamente("1") :- inicioJogo.
processaOpcaoJogarNovamente("2") :- sair.
processaOpcaoJogarNovamente(_) :- 
    write("Opcao invalida. Tente novamente.\n"),
    exibe_menu_jogar_novamente.

extrai_perguntas([], []).
extrai_perguntas([Pergunta, A1, A2, A3, A4, LinhaPontos, LinhaResposta | Linhas], [(Pergunta, [A1, A2, A3, A4], Pontos, Resposta)|Perguntas]) :-
    extrai_pontos(LinhaPontos, Pontos),
    extrai_resposta(LinhaResposta, Resposta),
    extrai_perguntas(Linhas, Perguntas).

extrai_pontos(Linha, Pontos) :-
    split_string(Linha, " ", "", Palavras),
    nth0(2, Palavras, StringPontos),
    number_string(Pontos, StringPontos).

extrai_resposta(Linha, Resposta) :-
    sub_string(Linha, 10, 1, _, Resposta).

take(0, _, []).
take(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, R).

drop(0, L, L).
drop(N, [_|T], R) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, R).   

quiz([], _, Pontuacoes, _) :- 
    Pontuacoes = Pontuacoes.
quiz([(Pergunta, Alternativas, Pontos, RespostaCorreta)|Linhas], Jogadores, Pontuacoes, Rodada) :-
    write('-----------------------------------------------------------------------------------------------------------'), nl,
    write(Pergunta), nl,
    maplist(write_alternativa, Alternativas),
    write('\nValendo '), write(Pontos), write(' pontos!'), nl,
    write('-----------------------------------------------------------------------------------------------------------'), nl,
    jogador_da_vez(Jogadores, Rodada, Jogador),
    write(Jogador), write(', sua resposta: '), nl,
    read_line_to_string(user_input, RespostaInicial),
    valida_resposta_jogador(RespostaInicial, Resposta),
    (   string_lower(Resposta, RespostaCorreta)
    ->  write('\nResposta correta! Você ganhou '), write(Pontos), write(' pontos!'), nl,
        atualiza_pontuacoes(Pontuacoes, Pontos, Rodada, NovasPontuacoes),
        NovaRodada is Rodada + 1,
        quiz(Linhas, Jogadores, NovasPontuacoes, NovaRodada)
    ;   write('\nResposta incorreta!'), nl,
        NovaRodada is Rodada + 1,
        quiz(Linhas, Jogadores, Pontuacoes, NovaRodada)
    ).

write_alternativa(Alternativa) :-
    write(Alternativa), nl.

valida_resposta_jogador(Resposta, Resposta) :-
    member(Resposta, ["a", "b", "c", "d"]), !.
valida_resposta_jogador(_, Resposta) :-
    write('Entrada incorreta! Escolha uma alternativa válida.'), nl,
    read_line_to_string(user_input, NovaResposta),
    valida_resposta_jogador(NovaResposta, Resposta).

jogador_da_vez(Jogadores, Rodada, Jogador) :-
    length(Jogadores, N),
    Index is Rodada mod N,
    nth0(Index, Jogadores, Jogador).

atualiza_pontuacoes(Pontuacoes, Pontos, Rodada, NovasPontuacoes) :-
    (   0 is Rodada mod 2
    ->  nth0(0, Pontuacoes, P1),
        P1Novo is P1 + Pontos,
        nth0(1, Pontuacoes, P2),
        NovasPontuacoes = [P1Novo, P2]
    ;   nth0(0, Pontuacoes, P1),
        nth0(1, Pontuacoes, P2),
        P2Novo is P2 + Pontos,
        NovasPontuacoes = [P1, P2Novo]
    ).

mostra_vencedor([Jogador1, Jogador2], [Pontuacao1, Pontuacao2]) :-
    (   Pontuacao1 > Pontuacao2
    ->  mensagem_vencedor(Jogador1)
    ;   Pontuacao2 > Pontuacao1
    ->  mensagem_vencedor(Jogador2)
    ;   write('\nO jogo empatou!'), nl
    ).

mensagem_vencedor(Jogador) :-
    write('\nParabéns! O vencedor é '), write(Jogador), write('!'), nl.

exibe_menu_perguntados :-
    menuPerguntados(Lines),
    maplist(writeln, Lines).

exibe_regras_do_jogo :-
    regrasDoJogo(Lines),
    maplist(writeln, Lines).

exibe_placar([Jogador1, Jogador2], [Pontuacao1, Pontuacao2]) :-
    placar([Jogador1, Jogador2], [Pontuacao1, Pontuacao2]).

exibe_escolha_tema :-
    escolhaTema(Lines),
    maplist(writeln, Lines).

exibe_menu_jogar_novamente :-
    menuJogarNovamente(Lines),
    maplist(writeln, Lines).

sair :- 
    write("Saindo..."), 
    !.

read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),  % Lê uma linha do arquivo
    ( Line == end_of_file -> Lines = []  % Se for o fim do arquivo, retorna lista vazia
    ; Lines = [Line | Rest],              % Caso contrário, adiciona à lista
      read_lines(Stream, Rest)            % Continua lendo
    ).

menuPerguntados([
     "  ____   U _____ u   ____      ____     _   _   _   _     _____      _      ____      U  ___ u  ____     "
    , "U|  _\"\\ u\\| ___\"|/U |  _\"\\ uU /\"___|uU |\"|u| | | \\ |\"|   |_ \" _| U  /\"\\  u |  _\"\\      \\\"/_ \\/ / __\"| u  "
    , "\\| |_) |/ |  _|\"   \\| |_) |/\\| |  _ / \\| |\\| |<|  \\| |>    | |    \\/ _ \\/ /| | | |     | | | |<\\___ \\/   "
    , " |  __/   | |___    |  _ <   | |_| |   | |_| |U| |\\  |u   /| |\\   / ___ \\ U| |_| |\\.-,_| |_| | u___) |   "
    , " |_|      |_____|   |_| \\_\\   \\____|  <<\\___/  |_| \\_|   u |_|U  /_/   \\_\\ |____/ u \\_)-\\___/  |____/>>  "
    , " ||>>_    <<   >>   //   \\\\_  _)(|_  (__) )(   ||   \\\\,_._// \\\\_  \\\\    >>  |||_         \\\\     )(  (__) "
    , "(__)__)  (__) (__) (__)  (__)(__)__)     (__)  (_\")  (_/(__) (__)(__)  (__)(__)_)       (__)   (__)      "
    , "                                                                                                           "
    , "                                     BEM-VINDOS AO PERGUNTADOS!                                            "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                       INICIAR (1) | SAIR (2)                                              "
    , "-----------------------------------------------------------------------------------------------------------"
    ]).

regrasDoJogo([
     "-----------------------------------------------------------------------------------------------------------"
    , "                                        VAMOS INICIAR O JOGO!                                              "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           REGRAS DO JOGO                                                  "
    , "                                                                                                           "
    , "1 - O jogo é uma competição entre dois jogadores.                                                          "
    , "2 - No início do jogo, os jogadores escolhem um tema para o quiz.                                          "
    , "3 - A cada rodada, os jogadores irão responder perguntas sobre o tema escolhido.                           "
    , "4 - A pontuação da pergunta é dada pelo seu nível de dificuldade.                                          "
    , "5 - No fim, ganha o jogador que obter mais pontos! :D                                                      "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                            ANTES DE INICIAR, DIGITE OS NOMES DOS JOGADORES                                "
    ]).

placar([Jogador1, Jogador2], [Pontuacao1, Pontuacao2]) :-
    atom_concat("-----------------------------------------------------------------------------------------------------------\n", 
    "                                               PLACAR                                                      \n", Linha1),
    atom_concat(Linha1, "-----------------------------------------------------------------------------------------------------------\n", Linha2),
    atom_concat("JOGADOR 1: ", Jogador1, J1),
    atom_concat(J1, " - Pontuação: ", J1Pont),
    number_string(Pontuacao1, P1String),
    atom_concat(J1Pont, P1String, Linha3Parte1),
    atom_concat(Linha2, Linha3Parte1, Linha3),
    
    atom_concat("\nJOGADOR 2: ", Jogador2, J2),
    atom_concat(J2, " - Pontuação: ", J2Pont),
    number_string(Pontuacao2, P2String),
    atom_concat(J2Pont, P2String, Linha4Parte1),
    atom_concat(Linha3, Linha4Parte1, Linha4),

    atom_concat(Linha4, "\n-----------------------------------------------------------------------------------------------------------", ResultadoFinal),

    write(ResultadoFinal).

escolhaTema([ 
      "                                 ESCOLHA UM TEMA PARA O QUIZ                                               "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           (1) ENTRETENIMENTO                                              "
    , "                                           (2) PROGRAMAÇÃO                                                 "
    , "                                           (3) GEOGRAFIA                                                   "
    , "                                           (4) HISTÓRIA                                                    "
    , "                                           (5) CIÊNCIAS                                                    "
    , "-----------------------------------------------------------------------------------------------------------"
    ]).

menuJogarNovamente([
     "-----------------------------------------------------------------------------------------------------------"
    , "                                           JOGAR NOVAMENTE?                                                "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           SIM (1) | NÃO (2)                                               "
    , "-----------------------------------------------------------------------------------------------------------"
    ]).