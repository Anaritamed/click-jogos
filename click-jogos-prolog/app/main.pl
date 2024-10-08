:- use_module(menu_inicial).

:- initialization(configura_unicode).

configura_unicode :-
    set_prolog_flag(encoding, utf8),
    open('output.txt', write, Stream, [encoding(utf8)]),
    set_stream(user_output, Stream),
    set_stream(user_input, encoding(utf8)).

:- inicio.