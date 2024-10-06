:- module(enforcado, [
    forca/0,
    get_campo_valido/3,
    get_Entrada/2,
    campo_valido/1,
    apenas_espacos/1,
    tem_digito/1
]).

:- use_module(utils).

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

get_dados_partida :-
    regras_do_jogo(_, Regras),
    write(Regras),
    write("Digite o seu nome, Jogador 1: \n"),
    read_line_to_string(user_input, Jogador1),
    write("Digite o seu nome, Jogador 2: \n"),
    read_line_to_string(user_input, _),
    format("\nCerto ~s, qual a palavra a ser adivinhada?\n", [Jogador1]),
    get_campo_valido("palavra", "\nPalavra inválida!", _).

regras_do_jogo(Regras, RegrasEstilizadas) :-
    Regras = "\n📜 Regras do jogo: \n- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona.
               \n- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.
               \n- Caso a palavra contenha uma letra acentuada ou ç, digite exatamente a letra com sua acentuação ou o ç.
               \n- Por exemplo, caso a palavra fosse 'Maçã' a != ã, assim como c != ç 
               \n",
    colore_amarelo(Regras, RegrasAmarelas),
    bold(RegrasAmarelas, RegrasBold),
    RegrasEstilizadas = RegrasBold.

get_campo_valido(Campo, MsgErro, Entrada) :- 
    get_Entrada(Campo, EntradaTemp),
    (eh_valido(Campo, Entrada) -> 
        Entrada = EntradaTemp ; 
        colore_amarelo(MsgErro, MsgErroA),
        bold(MsgErroA, MsgErroAB),
        write(MsgErroAB),
        get_campo_valido(Campo, MsgErro, Entrada)
    ).

get_Entrada("palavra", Entrada) :- read_line_to_string(user_input, Entrada).

eh_valido("palavra", Entrada) :- 
    string_chars(Entrada, Chars),
    campo_valido(Chars),
    length(Chars, Tam),
    Tam > 1.

% Predicado que verifica se um campo é válido
campo_valido(Campo) :-
    nonvar(Campo),           % Verifica se Campo não é uma variável não instanciada
    \+ Campo = [],           % Verifica se Campo não é vazio
    \+ apenas_espacos(Campo), % Verifica se Campo não contém apenas espaços em branco
    \+ tem_digito(Campo).    % Verifica se Campo não contém dígitos

% Predicado que verifica se uma string contém apenas espaços em branco
apenas_espacos([]).
apenas_espacos([C|R]) :-
    char_type(C, space),     % Verifica se o caractere é um espaço
    apenas_espacos(R).        % Chama recursivamente para o restante da lista

% Predicado que verifica se uma string contém dígitos
tem_digito([]).
tem_digito([C|_]) :-
    char_type(C, digit).      % Verifica se o caractere é um dígito
tem_digito([_|R]) :- 
    tem_digito(R).            % Chama recursivamente para o restante da lista
