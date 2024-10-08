:- module(enforcado, [
    forca/0,
    get_Entrada/3,
    eh_valido/2,
    jogo/2,
    loop/6,
    letra_validada/9,
    cria_string_sublinhados/2,
    cria_mapa_letras/2,
    atualiza_string_sublinhados/4,
    atualiza_forca/2,
    loop_get_campo_valido/3,
    campo_valido/1,
    cenario_perda/1,
    cenario_vitoria/1
]).

:- use_module(utils).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(pio)). % Para uso de read_line_to_string

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

eh_valido("letra", Tema) :- 
    string(Tema),
    string_length(Tema, Tamanho),
    Tamanho > 0,
    Tamanho == 1.

% Lógica principal do jogo
jogo(Palavra, Tema) :-
    cria_mapa_letras(Palavra, MapaLetras),  % Mapa de letras em minúsculas
    cria_string_sublinhados(Palavra, EstadoAtual), % cria string sublinhada com o tamanho da palavra

    loop(EstadoAtual, 0, [], MapaLetras, Tema, Palavra).

loop(StringSublinhada, Erros, LetrasDigitadas, MapaLetras, Tema, Palavra) :-
    limpa_terminal,
    atualiza_forca(Erros, ForcaAtual),
    write(ForcaAtual), nl,

    format("~sTEMA: ~s~n", ["\27[33m", Tema]),  % Colore o tema
    writeln(StringSublinhada),
    format("\nLetras digitadas: ~s~n", [LetrasDigitadas]),
    write("Digite uma letra: "),
    read_letra(LetraDigitada),

    letra_validada(LetraDigitada, LetrasDigitadas, StringSublinhada, Erros, MapaLetras, Palavra, NovoEstadoStringSublinhados, NovoErros, LetrasDigitadasAtualizada),
    (   NovoErros >= 6 -> 
        cenario_perda(Palavra);
        loop(NovoEstadoStringSublinhados, NovoErros, LetrasDigitadasAtualizada, MapaLetras, Tema, Palavra)
    ).

letra_validada(LetraDigitada, LetrasDigitadas, StringSublinhada, Erros, MapaLetras, Palavra, NovoEstadoStringSublinhados, NovoErros, LetrasDigitadasAtualizada) :-
    downcase_atom(LetraDigitada, Letra),
    (   member(Letra, LetrasDigitadas) -> 
        writeln("\27[33mEssa letra já foi digitada!\27[0m"), % Mensagem se a letra já foi digitada
        sleep(1),
        (Erros =< 5 -> NovoErros = Erros; NovoErros = Erros);
        % Atualiza letras digitadas
        atom_concat(" ", Letra, LetrasDigitadasAtualizada),
        (   get_assoc(Letra, MapaLetras, Indices) -> 
            atualiza_string_sublinhados(Letra, StringSublinhada, Indices, NovoEstadoStringSublinhados),
            (   downcase_atom(NovoEstadoStringSublinhados, Palavra) -> 
                cenario_vitoria(Palavra);
                NovoErros = Erros
            )
        ;   
            NovoErros is Erros + 1
        )
    ).

% Função que cria a string com sublinhados
cria_string_sublinhados(Palavra, EstadoSublinhado) :-
    string_length(Palavra, Tamanho),
    atom_chars(EstadoSublinhado, "_"),
    length(EstadoSublinhado, Tamanho).

% Função que cria um mapa de letra -> posições
cria_mapa_letras(Palavra, Mapa) :-
    string_chars(Palavra, Letras),
    findall((Letra, [I]), (nth0(I, Letras, Letra)), ListaAssociativa),
    group_pairs_by_key(ListaAssociativa, Mapa).

% Função para atualizar a string de sublinhados com a letra correta
atualiza_string_sublinhados(Letra, Sublinhados, Indices, NovoEstado) :-
    atom_chars(Sublinhados, CharsSublinhados),
    findall(NovoChar, (between(0, 20, I), (member(I, Indices) -> NovoChar = Letra ; nth0(I, CharsSublinhados, NovoChar))), NovoChars),
    atom_chars(NovoEstado, NovoChars).

% Desenha a forca atualizada a cada erro (contado) passado
atualiza_forca(0, Forca) :- 
    Forca = 
        "      ________    \n" +
        "     |/       |   \n" +
        "     |        §   \n" +
        "     |            \n" +
        "     |            \n" +
        "     |            \n" +
        "   __|            \n" +
        "  |  |            \n" +
        "  ====            \n".

atualiza_forca(1, Forca) :- 
    Forca = 
        "      ________     \n" +
        "     |/       |    \n" +
        "     |        §    \n" +
        "     |      (*.*)  \n" +
        "     |             \n" +
        "     |             \n" +
        "   __|             \n" +
        "  |  |             \n" +
        "  ====             \n".

atualiza_forca(2, Forca) :- 
    Forca = 
        "      ________     \n" +
        "     |/       |    \n" +
        "     |        §    \n" +
        "     |      (*.*)  \n" +
        "     |        |    \n" +
        "     |       [ ]   \n" +
        "   __|        |    \n" +
        "  |  |             \n" +
        "  ====             \n".

atualiza_forca(3, Forca) :- 
    Forca = 
        "      ________     \n" +
        "     |/       |    \n" +
        "     |        §    \n" +
        "     |      (*.*)  \n" +
        "     |        |    \n" +
        "     |       [ ]   \n" +
        "   __|        |    \n" +
        "  |  |       /     \n" +
        "  ====             \n".

atualiza_forca(4, Forca) :- 
    Forca = 
        "      ________     \n" +
        "     |/       |    \n" +
        "     |        §    \n" +
        "     |      (*.*)  \n" +
        "     |        |    \n" +
        "     |       [ ]   \n" +
        "   __|        |    \n" +
        "  |  |       / \\  \n" +
        "  ====             \n".

atualiza_forca(5, Forca) :- 
    Forca = 
        "      ________     \n" +
        "     |/       |    \n" +
        "     |        §    \n" +
        "     |      (*.*)  \n" +
        "     |        |    \n" +
        "     |      /[ ]   \n" +
        "   __|        |    \n" +
        "  |  |       / \\  \n" +
        "  ====             \n".

atualiza_forca(6, Forca) :- 
    Forca = 
        "      ________     \n" +
        "     |/       |    \n" +
        "     |        §    \n" +
        "     |      (*.*)  \n" +
        "     |        |    \n" +
        "     |      /[ ]\\ \n" +
        "   __|        |    \n" +
        "  |  |       / \\  \n" +
        "  ====             \n".

% Função que requer em loop campo, caso não validado corretamente
loop_get_campo_valido(Campo, Mensagem, Resposta) :-
    read_line_to_string(user_input, Input),
    (   campo_valido(Input) -> 
        Resposta = Input;
        writeln(Mensagem),
        loop_get_campo_valido(Campo, Mensagem, Resposta)
    ).

campo_valido(Campo) :- 
    \+ (Campo == ""), \+ (all_chars_space(Campo)), \+ (any_char_digit(Campo)).

all_chars_space(String) :- 
    string_chars(String, Chars), 
    forall(member(Char, Chars), char_type(Char, space)).

any_char_digit(String) :- 
    string_chars(String, Chars), 
    member(Char, Chars), 
    char_type(Char, digit).

cenario_perda(Palavra) :-
    format("A PALAVRA ERA: ~s!\n", [Palavra]),
    writeln("Você perdeu!").

cenario_vitoria(Palavra) :-
    format("PARABÉNS, VOCÊ VENCEU! A PALAVRA ERA: ~s!\n", [Palavra]).