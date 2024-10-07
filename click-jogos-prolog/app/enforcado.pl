:- module(enforcado, [
    forca/0,
    get_Entrada/3,
    eh_valido/2
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
    writeln(Palavra),
    write("Qual o tema da palavra?\n"),
    get_Entrada(_, Tema, "Tema inválido!"),
    writeln(Tema).

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

eh_valido("letra", Tema) :- 
    string(Tema),
    string_length(Tema, Tamanho),
    Tamanho > 0.
