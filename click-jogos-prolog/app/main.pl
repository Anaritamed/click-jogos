:- use_module(menu_inicial).

:- initialization(configura_unicode).

configura_unicode :-
    % Configura a codificação de entrada e saída como UTF-8
    set_stream(user_output, encoding(utf8)),
    set_stream(user_input, encoding(utf8)),
    % Se necessário, ajuste a codificação padrão do Prolog
    set_prolog_flag(encoding, utf8).

:- inicio.