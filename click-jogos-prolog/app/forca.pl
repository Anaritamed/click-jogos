:- module(forca, [forca/0]).

:- use_module(utils).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(charsio)).

% Função inicial, que realiza o disparo do jogo forca
forca :-
    limpa_terminal,
    home_forca(HomeForca),
    maplist(writeln, HomeForca),
    read_line_to_string(user_input, Opcao),
    menu_forca(Opcao).

% Menu de opções do jogo forca
menu_forca("1") :- processa_dados_partida.
menu_forca("2") :-
    writeln("Saindo..."),
    halt.
menu_forca(_) :-
    colore_amarelo("Opção inválida!", Msg),
    bold(Msg, BoldMsg),
    writeln(BoldMsg),
    forca.

% Processa dados da interação inicial com os usuários
processa_dados_partida :-
    regras_do_jogo(Regras),
    colore_amarelo(Regras, RegrasAmarelas),
    bold(RegrasAmarelas, RegrasBold),
    writeln(RegrasBold),
    writeln("Digite o seu nome Jogador 1: "),
    read_line_to_string(user_input, Jogador1),
    writeln("Digite o seu nome Jogador 2: "),
    read_line_to_string(user_input, Jogador2),
    format("\nCerto ~w, qual a palavra a ser adivinhada?\n", [Jogador1]),
    loop_get_campo_valido("palavra", "Palavra inválida!", Palavra),
    writeln("Qual o tema que está relacionado à palavra a ser adivinhada? "),
    loop_get_campo_valido("tema", "Tema inválido!", Tema),
    jogo(Palavra, Tema).

% Lógica principal do jogo
jogo(Palavra, Tema) :-
    string_lower(Palavra, PalavraMin),
    cria_mapa_letras(PalavraMin, MapaLetras),
    cria_string_sublinhados(Palavra, EstadoAtual),
    loop(EstadoAtual, 0, "", MapaLetras, Palavra, Tema).

loop(StringSublinhada, Erros, LetrasDigitadas, MapaLetras, Palavra, Tema) :-
    limpa_terminal,
    atualiza_forca(Erros, Forca),
    writeln(Forca),
    colore_amarelo("\nTEMA: ", TemaAmarelo),
    bold(TemaAmarelo, TemaBold),
    format("~w~w\n", [TemaBold, Tema]),
    writeln(StringSublinhada),
    format("\nLetras digitadas: ~w\n", [LetrasDigitadas]),
    writeln("\nDigite uma letra:"),
    loop_get_campo_valido("letra", "Letra inválida!", LetraDigitada),
    string_lower(LetraDigitada, LetraMin),
    string_chars(LetraMin, [Letra]),
    (   sub_string(LetrasDigitadas, _, _, _, Letra)
    ->  colore_amarelo("\nEssa letra já foi digitada!\n", Msg),
        writeln(Msg),
        sleep(0.9),
        loop(StringSublinhada, Erros, LetrasDigitadas, MapaLetras, Palavra, Tema)
    ;   string_concat(Letra, " ", LetrasDigitadas, LetrasDigitadasAtualizada),
        (   get_dict(Letra, MapaLetras, Indices)
        ->  atualiza_string_sublinhados(Letra, StringSublinhada, Indices, NovoEstadoStringSublinhados),
            (   string_lower(NovoEstadoStringSublinhados, PalavraMin)
            ->  cenario_vitoria(Palavra, CenarioVitoria),
                maplist(writeln, CenarioVitoria),
                sleep(4),
                forca
            ;   loop(NovoEstadoStringSublinhados, Erros, LetrasDigitadasAtualizada, MapaLetras, Palavra, Tema)
            )
        ;   (   Erros + 1 >= 6
            ->  cenario_perda(Palavra, CenarioPerda),
                maplist(writeln, CenarioPerda),
                sleep(3),
                forca
            ;   loop(StringSublinhada, Erros + 1, LetrasDigitadasAtualizada, MapaLetras, Palavra, Tema)
            )
        )
    ).

% Função que cria a string com sublinhados
cria_string_sublinhados(Palavra, Sublinhados) :-
    string_length(Palavra, Length),
    length(SublinhadosList, Length),
    maplist(=(' '), SublinhadosList),
    string_chars(Sublinhados, SublinhadosList).

% Função que cria um mapa de letra -> posições
cria_mapa_letras(Palavra, MapaLetras) :-
    string_chars(Palavra, Chars),
    findall(Letra-Pos, nth0(Pos, Chars, Letra), Pairs),
    group_pairs_by_key(Pairs, GroupedPairs),
    dict_create(MapaLetras, _, GroupedPairs).

% Função para atualizar a string de sublinhados com a letra correta
atualiza_string_sublinhados(_, [], _, []).
atualiza_string_sublinhados(Letra, [H|T], Indices, [Letra|NovoT]) :-
    member(0, Indices), !,
    maplist(dec, Indices, NovosIndices),
    atualiza_string_sublinhados(Letra, T, NovosIndices, NovoT).
atualiza_string_sublinhados(Letra, [H|T], Indices, [H|NovoT]) :-
    maplist(dec, Indices, NovosIndices),
    atualiza_string_sublinhados(Letra, T, NovosIndices, NovoT).

dec(X, Y) :- Y is X - 1.

% Função que requer em loop campo, caso não validado corretamente
loop_get_campo_valido(Campo, Mensagem, Input) :-
    get_input(Campo, InputTemp),
    (   is_valid(Campo, InputTemp)
    ->  Input = InputTemp
    ;   colore_amarelo(Mensagem, Msg),
        bold(Msg, BoldMsg),
        writeln(BoldMsg),
        loop_get_campo_valido(Campo, Mensagem, Input)
    ).

% Define formato em receber input, caso palavra ele esconde o input com a função hide_input 
get_input("tema", Input) :- read_line_to_string(user_input, Input).
get_input("letra", Input) :- read_line_to_string(user_input, Input).
get_input("palavra", Input) :- hide_input(Input).

% Realiza validação de acordo com o tipo de campo
is_valid("tema", Input) :- campo_valido(Input), string_length(Input, Length), Length > 1.
is_valid("letra", Input) :- campo_valido(Input), string_length(Input, 1).
is_valid("palavra", Input) :- campo_valido(Input), string_length(Input, Length), Length > 1, \+ sub_string(Input, _, _, _, ' ').

% Função para ocultar a entrada do usuário e avançar o prompt a cada letra digitada
hide_input(Input) :-
    set_prolog_flag(tty_control, true),
    get_single_char(Char),
    hide_input_loop(Char, [], Input).

hide_input_loop(10, Acc, Input) :- !,
    reverse(Acc, RevAcc),
    string_chars(Input, RevAcc),
    nl.
hide_input_loop(Char, Acc, Input) :-
    put_char('*'),
    flush_output,
    get_single_char(NextChar),
    hide_input_loop(NextChar, [Char|Acc], Input).

% Desenha a forca atualizada a cada erro (contado) passado
atualiza_forca(0, Forca) :-
    Forca = "      ________    \n     |/       |   \n     |        §   \n     |            \n     |            \n     |            \n   __|            \n  |  |            \n  ====            ".
atualiza_forca(1, Forca) :-
    Forca = "      ________     \n     |/       |    \n     |        §    \n     |      (*.*)  \n     |             \n     |             \n   __|             \n  |  |             \n  ====             ".
atualiza_forca(2, Forca) :-
    Forca = "      ________     \n     |/       |    \n     |        §    \n     |      (*.*)  \n     |        |    \n     |       [ ]   \n   __|        |    \n  |  |             \n  ====             ".
atualiza_forca(3, Forca) :-
    Forca = "      ________     \n     |/       |    \n     |        §    \n     |      (*.*)  \n     |        |    \n     |       [ ]   \n   __|        |    \n  |  |       /     \n  ====             ".
atualiza_forca(4, Forca) :-
    Forca = "      ________     \n     |/       |    \n     |        §    \n     |      (*.*)  \n     |        |    \n     |       [ ]   \n   __|        |    \n  |  |       / \\  \n  ====             ".
atualiza_forca(5, Forca) :-
    Forca = "      ________     \n     |/       |    \n     |        §    \n     |      (*.*)  \n     |        |    \n     |      /[ ]   \n   __|        |    \n  |  |       / \\  \n  ====             ".
atualiza_forca(6, Forca) :-
    Forca = "      ________     \n     |/       |    \n     |        §    \n     |      (*.*)  \n     |        |    \n     |      /[ ]\\ \n   __|        |    \n  |  |       / \\  \n  ====             ".

% Regras do jogo
regras_do_jogo(Regras) :-
    Regras = "\n📜 Regras do jogo: \n- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona.
               \n- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.
               \n- Caso a palavra contenha uma letra acentuada ou ç, digite exatamente a letra com sua acentuação ou o ç.
               \n- Por exemplo, caso a palavra fosse 'Maçã' a != ã, assim como c != ç 
               \n".

% Tela inicial do jogo
home_forca(HomeForca) :-
    HomeForca = [
        "                                               ",
        "   ███████╗ ██████╗ ██████╗  ██████╗ █████╗    ",
        "   ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗   ",
        "   █████╗  ██║   ██║██████╔╝██║     ███████║   ",
        "   ██╔══╝  ██║   ██║██╔══██╗██║     ██╔══██║   ",
        "   ██║     ╚██████╔╝██║  ██║╚██████╗██║  ██║   ",
        "   ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝   ",
        "               SEJA BEM VINDO!                 ",
        "                                               ",
        "                 (1) JOGAR                     ",
        "              (2) SAIR DO JOGO                 ",
        "                                               "
    ].

% Cenário de perda
cenario_perda(Palavra, CenarioPerda) :-
    string_upper(Palavra, PalavraUpper),
    bold(PalavraUpper, PalavraBold),
    CenarioPerda = [
        " ██████╗  █████╗ ███╗   ███╗███████╗     ██████╗ ██╗   ██╗███████╗██████╗ ",
        "██╔════╝ ██╔══██╗████╗ ████║██╔════╝    ██╔═══██╗██║   ██║██╔════╝██╔══██╗",
        "██║  ███╗███████║██╔████╔██║█████╗      ██║   ██║██║   ██║█████╗  ██████╔╝",
        "██║   ██║██╔══██║██║╚██╔╝██║██╔══╝      ██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗",
        "╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗    ╚██████╔╝ ╚████╔╝ ███████╗██║  ██║",
        " ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝     ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝",
        "                           A PALAVRA ERA: " ++ PalavraBold ++ "!"
    ].

% Cenário de vitória
cenario_vitoria(Palavra, CenarioVitoria) :-
    string_upper(Palavra, PalavraUpper),
    bold(PalavraUpper, PalavraBold),
    CenarioVitoria = [
        "  ██████╗███████╗██████╗ ████████╗ █████╗     ██████╗ ███████╗███████╗██████╗  ██████╗ ███████╗████████╗ █████╗ ██╗",
        " ██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔══██╗    ██╔══██╗██╔════╝██╔════╝██╔══██╗██╔═══██╗██╔════╝╚══██╔══╝██╔══██╗██║",
        " ██║     █████╗  ██████╔╝   ██║   ███████║    ██████╔╝█████╗  ███████╗██████╔╝██║   ██║███████╗   ██║   ███████║██║",
        " ██║     ██╔══╝  ██╔══██╗   ██║   ██╔══██║    ██╔══██╗██╔══╝  ╚════██║██╔═══╝ ██║   ██║╚════██║   ██║   ██╔══██║╚═╝",
        " ╚██████╗███████╗██║  ██║   ██║   ██║  ██║    ██║  ██║███████╗███████║██║     ╚██████╔╝███████║   ██║   ██║  ██║██╗",
        "  ╚═════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝      ╚═════╝ ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝",
        "                               PARABÉNS, VOCÊ VENCEU! A PALAVRA ERA: " ++ PalavraBold ++ "!"
    ].