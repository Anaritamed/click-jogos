:- module(forca, [forca/0]).

:- use_module(utils).
:- use_module(library(lists)).

forca :- 
    inicio.

inicio :-
    limpa_terminal,
    writeln("                                               "),
    writeln("   ███████╗ ██████╗ ██████╗  ██████╗ █████╗    "),
    writeln("   ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗   "),
    writeln("   █████╗  ██║   ██║██████╔╝██║     ███████║   "),
    writeln("   ██╔══╝  ██║   ██║██╔══██╗██║     ██╔══██║   "),
    writeln("   ██║     ╚██████╔╝██║  ██║╚██████╗██║  ██║   "),
    writeln("   ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝   "),
    writeln("               SEJA BEM VINDO!                 "),
    writeln("                                               "),
    writeln("                 (1) JOGAR                     "),
    writeln("              (2) SAIR DO JOGO                 "),
    writeln("                                               "),
    read_line_to_string(user_input, _opcao),
    processa_opcao(_opcao).

processa_opcao("1") :- get_dados_partida.
processa_opcao("2") :- sair.
processa_opcao(_) :- inicio.

get_dados_partida :-
    regras_do_jogo(_, Regras),
    write(Regras),
    write("Digite o seu nome, Jogador 1: \n"),
    read_line_to_string(user_input, Jogador1),
    write("Digite o seu nome, Jogador 2: \n"),
    read_line_to_string(user_input, _),
    format("\nCerto ~s, qual a palavra a ser adivinhada?\n", [Jogador1]),
    get_Entrada(_, Palavra, "Palavra inválida!"),
    write("Qual o tema da palavra?\n"),
    get_Entrada(_, Tema, "Tema inválido!"),
    jogo(Palavra, Tema).

regras_do_jogo(Regras, RegrasEstilizadas) :-
    Regras = "\n📜 Regras do jogo: \n- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona.
               \n- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.
               \n- Caso a palavra contenha uma letra acentuada ou ç, digite exatamente a letra com sua acentuação ou o ç.
               \n- Por exemplo, caso a palavra fosse 'Maçã' a != ã, assim como c != ç 
               \n",
    colore_amarelo(Regras, RegrasAmarelas),
    bold(RegrasAmarelas, RegrasBold),
    RegrasEstilizadas = RegrasBold.

get_Entrada(_, Entrada, MsgErro) :- 
    read_line_to_string(user_input, EntradaTemp),
    trim(EntradaTemp, EntradaTrimmed),
    (eh_valido(_, EntradaTrimmed) -> 
        Entrada = EntradaTrimmed
        ; writeln(MsgErro), get_Entrada(_, Entrada, MsgErro)).

eh_valido(_, Texto) :- 
    string(Texto),
    string_length(Texto, Tamanho),
    Tamanho > 0,
    trim(Texto, TextoTrim),
    TextoTrim \= "". 

eh_valido("letra", Texto) :- 
    string(Texto),
    string_length(Texto, Tamanho),
    Tamanho > 0,
    Tamanho == 1.

% inicializa o jogo
jogo(Palavra, Tema) :-
    string_lower(Palavra, PalavraMin),
    atom_chars(PalavraMin, ListaPalavra),
    length(ListaPalavra, N),
    cria_lista_sublinhados(N, EstadoAtual),
    colore_amarelo(Tema, TemaAmarelo),
    format("Tema: ~w~n", [TemaAmarelo]),
    loop(ListaPalavra, EstadoAtual, [], 0).

% Função principal de loop
loop(Palavra, EstadoAtual, LetrasDigitadas, Erros) :-
    (Erros == 6 ->
        cena_perda(Palavra),
        sleep(3),
        inicio
        ;
        true
    ),
    Erros < 6,
    atualiza_forca(Erros),
    format("Palavra atual: ~w~n", [EstadoAtual]),
    format("Letras digitadas: ~w~n", [LetrasDigitadas]),
    writeln("Digite uma letra:"),
    read_line_to_string(user_input, Letra),
    string_lower(Letra, LetraMin),
    atom_chars(LetraMin, [LetraChar]),

    (member(LetraChar, LetrasDigitadas) ->
        colore_amarelo("Essa letra já foi digitada!", Mensagem),
        writeln(Mensagem),
        loop(Palavra, EstadoAtual, LetrasDigitadas, Erros)
    ;
        atualiza_jogo(Palavra, LetraChar, EstadoAtual, NovoEstado, Erros, NovosErros),
        (NovoEstado == Palavra ->
            cena_vitoria(Palavra),
                sleep(3),
                inicio
        ;
            limpa_terminal,
            loop(Palavra, NovoEstado, [LetraChar|LetrasDigitadas], NovosErros)
        )
    ).

% Atualiza o estado do jogo com a letra correta ou aumenta o contador de erros
atualiza_jogo(Palavra, Letra, EstadoAtual, NovoEstado, Erros, NovosErros) :-
    (ocorrencias(Palavra, Letra, Posicoes) ->
        atualiza_lista_sublinhados(Posicoes, Letra, EstadoAtual, NovoEstado),
        NovosErros is Erros
    ;
        NovoEstado = EstadoAtual,
        NovosErros is Erros + 1
    ).

% Verifica as posições da letra na palavra
ocorrencias(Palavra, Letra, Posicoes) :-
    findall(Index, nth1(Index, Palavra, Letra), Posicoes),
    Posicoes \= [].

% Atualiza a string sublinhada com as letras corretas
atualiza_lista_sublinhados([], _, EstadoAtual, EstadoAtual).
atualiza_lista_sublinhados([Pos|Posicoes], Letra, EstadoAtual, NovoEstado) :-
    nth1(Pos, EstadoAtual, _, EstadoAtualIntermediario),
    nth1(Pos, EstadoAtualFinal, Letra, EstadoAtualIntermediario),
    atualiza_lista_sublinhados(Posicoes, Letra, EstadoAtualFinal, NovoEstado).

% Cria a string de sublinhados
cria_lista_sublinhados(0, []).
cria_lista_sublinhados(N, ['_'|Resto]) :-
    N > 0,
    N1 is N - 1,
    cria_lista_sublinhados(N1, Resto).

print_mensagem([]).
print_mensagem([Linha|Resto]) :-
    writeln(Linha),
    print_mensagem(Resto).

% Atualiza a forca de acordo com o número de erros
atualiza_forca(0) :-
    writeln("      ________    "),
    writeln("     |/       |   "),
    writeln("     |        §   "),
    writeln("     |            "),
    writeln("     |            "),
    writeln("     |            "),
    writeln("   __|            "),
    writeln("  |  |            "),
    writeln("  ====            ").

atualiza_forca(1) :-
    writeln("      ________     "),
    writeln("     |/       |    "),
    writeln("     |        §    "),
    writeln("     |      (*.*)  "),
    writeln("     |             "),
    writeln("     |             "),
    writeln("   __|             "),
    writeln("  |  |             "),
    writeln("  ====             ").

atualiza_forca(2) :-
    writeln("      ________     "),
    writeln("     |/       |    "),
    writeln("     |        §    "),
    writeln("     |      (*.*)  "),
    writeln("     |        |    "),
    writeln("     |       [ ]   "),
    writeln("   __|        |    "),
    writeln("  |  |             "),
    writeln("  ====             ").

atualiza_forca(3) :-
    writeln("      ________     "),
    writeln("     |/       |    "),
    writeln("     |        §    "),
    writeln("     |      (*.*)  "),
    writeln("     |        |    "),
    writeln("     |       [ ]   "),
    writeln("   __|        |    "),
    writeln("  |  |       /     "),
    writeln("  ====             ").

atualiza_forca(4) :-
    writeln("      ________     "),
    writeln("     |/       |    "),
    writeln("     |        §    "),
    writeln("     |      (*.*)  "),
    writeln("     |        |    "),
    writeln("     |       [ ]   "),
    writeln("   __|        |    "),
    writeln("  |  |       / \\  "),
    writeln("  ====             ").

atualiza_forca(5) :-
    writeln("      ________     "),
    writeln("     |/       |    "),
    writeln("     |        §    "),
    writeln("     |      (*.*)  "),
    writeln("     |        |    "),
    writeln("     |      /[ ]   "),
    writeln("   __|        |    "),
    writeln("  |  |       / \\  "),
    writeln("  ====             ").

atualiza_forca(6) :-
    writeln("      ________     "),
    writeln("     |/       |    "),
    writeln("     |        §    "),
    writeln("     |      (*.*)  "),
    writeln("     |        |    "),
    writeln("     |      /[ ]\\ "),
    writeln("   __|        |    "),
    writeln("  |  |       / \\  "),
    writeln("  ====             "),
    writeln("Você perdeu!").

cena_vitoria(Palavra) :-
    limpa_terminal,
    writeln("  ██████╗███████╗██████╗ ████████╗ █████╗     ██████╗ ███████╗███████╗██████╗  ██████╗ ███████╗████████╗ █████╗ ██╗"),
    writeln(" ██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔══██╗    ██╔══██╗██╔════╝██╔════╝██╔══██╗██╔═══██╗██╔════╝╚══██╔══╝██╔══██╗██║"),
    writeln(" ██║     █████╗  ██████╔╝   ██║   ███████║    ██████╔╝█████╗  ███████╗██████╔╝██║   ██║███████╗   ██║   ███████║██║"),
    writeln(" ██║     ██╔══╝  ██╔══██╗   ██║   ██╔══██║    ██╔══██╗██╔══╝  ╚════██║██╔═══╝ ██║   ██║╚════██║   ██║   ██╔══██║╚═╝"),
    writeln(" ╚██████╗███████╗██║  ██║   ██║   ██║  ██║    ██║  ██║███████╗███████║██║     ╚██████╔╝███████║   ██║   ██║  ██║██╗"),
    writeln("  ╚═════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝      ╚═════╝ ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝"),
    write("                               PARABÉNS, VOCÊ VENCEU! A PALAVRA ERA: "), write(Palavra), write("!"), nl.

cena_perda(Palavra) :-
    writeln(" ██████╗  █████╗ ███╗   ███╗███████╗     ██████╗ ██╗   ██╗███████╗██████╗ "),
    writeln("██╔════╝ ██╔══██╗████╗ ████║██╔════╝    ██╔═══██╗██║   ██║██╔════╝██╔══██╗"),
    writeln("██║  ███╗███████║██╔████╔██║█████╗      ██║   ██║██║   ██║█████╗  ██████╔╝"),
    writeln("██║   ██║██╔══██║██║╚██╔╝██║██╔══╝      ██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗"),
    writeln("╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗    ╚██████╔╝ ╚████╔╝ ███████╗██║  ██║"),
    writeln(" ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝     ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝"),
    write("                           A PALAVRA ERA: "), writeln(Palavra).