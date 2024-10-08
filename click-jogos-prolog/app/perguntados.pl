:- module(perguntados, [perguntados/0]).
:- use_module(library(readutil)).

perguntados :- 
    exibe_menu_perguntados,
    write("Digite uma opção: "),
    read_line_to_string(user_input, Opcao),
    processa_opcao_menu(Opcao).

processa_opcao_menu("1") :- inicio_jogo.
processa_opcao_menu("2") :- sair.
processa_opcao_menu(_) :- 
    write("Opção inválida. Tente novamente.\n"),
    perguntados.

inicio_jogo :- 
    exibe_regras_do_jogo,
    writeln("Digite o nome do jogador 1: "),
    read_line_to_string(user_input, Jogador1),
    writeln("Digite o nome do jogador 2: "),
    read_line_to_string(user_input, Jogador2),
    exibe_placar([Jogador1, Jogador2], [0, 0]),
    tema_jogo([Jogador1, Jogador2]).

tema_jogo(Jogadores) :-
    exibe_escolha_tema,
    write("Digite uma opção: "),
    read_line_to_string(user_input, Opcao),
    processa_tema_jogo(Opcao, Jogadores).

tema_disponivel("1", _, "entretenimento.txt").
tema_disponivel("2", _, "programacao.txt").
tema_disponivel("3", _, "geografia.txt").
tema_disponivel("4", _, "historia.txt").
tema_disponivel("5", _, "ciencias.txt").
tema_disponivel(_, Jogadores, _) :- 
    write("Opção inválida. Tente novamente.\n"),
    tema_jogo(Jogadores).

processa_tema_jogo(Opcao, Jogadores) :-
    tema_disponivel(Opcao, Jogadores, Tema),
    jogo(Jogadores, Tema).

jogo(Jogadores, Tema) :-
    working_directory(CWD, CWD),  
    atom_concat(CWD, 'perguntas/', CaminhoBase),
    atom_concat(CaminhoBase, Tema, CaminhoArquivo), 
    open(CaminhoArquivo, read, Arquivo, [encoding(utf8)]),
    read_lines(Arquivo, Linhas), 
    pega_perguntas(Linhas, Perguntas),
    quiz(Perguntas, Jogadores,[0, 0], 0, Resultado),
    exibe_placar(Jogadores, Resultado),
    close(Arquivo),
    mostra_vencedor(Jogadores, Resultado),
    exibe_menu_jogar_novamente,
    write("Digite uma opção: "),
    read_line_to_string(user_input, Opcao),
    processa_opcao_jogar_novamente(Opcao).

processa_opcao_jogar_novamente("1") :- inicio_jogo.
processa_opcao_jogar_novamente("2") :- sair.
processa_opcao_jogar_novamente(_) :- 
    write("Opção inválida. Tente novamente.\n"),
    exibe_menu_jogar_novamente,
    write("Digite uma opção: "),
    read_line_to_string(user_input, Opcao),
    processa_opcao_jogar_novamente(Opcao).

pega_perguntas([], []).
pega_perguntas([_, Pergunta, A1, A2, A3, A4, LinhaPontos, LinhaResposta | Linhas], [(Pergunta, [A1, A2, A3, A4], Pontos, Resposta) | Perguntas]) :-
    pega_ponto(LinhaPontos, Pontos),
    pega_resposta(LinhaResposta, Resposta),
    pega_perguntas(Linhas, Perguntas).
pega_perguntas([Pergunta, A1, A2, A3, A4, LinhaPontos, LinhaResposta | Linhas], [(Pergunta, [A1, A2, A3, A4], Pontos, Resposta) | Perguntas]) :-
    pega_ponto(LinhaPontos, Pontos),
    pega_resposta(LinhaResposta, Resposta),
    pega_perguntas(Linhas, Perguntas). 

pega_ponto(LinhaPontos, Pontos) :-
    split_string(LinhaPontos, " ", "", Palavras),
    nth0(2, Palavras, StringPontos),
    number_string(Pontos, StringPontos).  

pega_resposta(LinhaResposta, Resposta) :-
    split_string(LinhaResposta, " ", "", Palavras),
    nth0(1, Palavras, RespostaAlternativa),
    sub_string(RespostaAlternativa, 0, 1, _, Resposta).

quiz([], _, Pontuacoes, _, Resultado) :- 
    Resultado = Pontuacoes.
quiz([(Pergunta, Alternativas, Pontos, RespostaCorreta)|Linhas], Jogadores, Pontuacoes, Rodada, Resultado) :-
    write('-----------------------------------------------------------------------------------------------------------'), nl,
    write(Pergunta), nl,
    maplist(write_alternativa, Alternativas),
    write('\nValendo '), write(Pontos), write(' pontos!'), nl,
    writeln('-----------------------------------------------------------------------------------------------------------'),
    jogador_da_vez(Jogadores, Rodada, Jogador),
    write(Jogador), write(', sua resposta: '),
    read_line_to_string(user_input, RespostaInicial),
    valida_resposta_jogador(RespostaInicial, Resposta),
    (   string_lower(Resposta, RespostaCorreta)
    ->  write('\nResposta correta! Você ganhou '), write(Pontos), write(' pontos!'), nl,
        atualiza_pontuacoes(Pontuacoes, Pontos, Rodada, NovasPontuacoes),
        NovaRodada is Rodada + 1,
        quiz(Linhas, Jogadores, NovasPontuacoes, NovaRodada, Resultado)
    ;   write('\nResposta incorreta!'), nl,
        NovaRodada is Rodada + 1,
        quiz(Linhas, Jogadores, Pontuacoes, NovaRodada, Resultado)
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
    menu_perguntados(Lines),
    maplist(writeln, Lines).

exibe_regras_do_jogo :-
    regras_do_jogo(Lines),
    maplist(writeln, Lines).

exibe_placar([Jogador1, Jogador2], [Pontuacao1, Pontuacao2]) :-
    placar([Jogador1, Jogador2], [Pontuacao1, Pontuacao2]).

exibe_escolha_tema :-
    escolha_tema(Lines),
    maplist(writeln, Lines).

exibe_menu_jogar_novamente :-
    menu_jogar_novamente(Lines),
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

menu_perguntados([
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

regras_do_jogo([
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

    atom_concat(Linha4, "\n-----------------------------------------------------------------------------------------------------------\n", ResultadoFinal),

    write(ResultadoFinal).

escolha_tema([
      "                                      ESCOLHA UM TEMA PARA O QUIZ                                          "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           (1) ENTRETENIMENTO                                              "
    , "                                           (2) PROGRAMAÇÃO                                                 "
    , "                                           (3) GEOGRAFIA                                                   "
    , "                                           (4) HISTÓRIA                                                    "
    , "                                           (5) CIÊNCIAS                                                    "
    , "-----------------------------------------------------------------------------------------------------------"
    ]).

menu_jogar_novamente([
      "-----------------------------------------------------------------------------------------------------------"
    , "                                           JOGAR NOVAMENTE?                                                "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           SIM (1) | NÃO (2)                                               "
    , "-----------------------------------------------------------------------------------------------------------"
    ]).