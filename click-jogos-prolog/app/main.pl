:- use_module(menu_inicial).

:- initialization(configura_unicode).

configura_unicode :-
    set_stream(user_output, encoding(utf8)),
    set_stream(user_input, encoding(utf8)),
    set_stream(user_error, encoding(utf8)).

:- inicio.