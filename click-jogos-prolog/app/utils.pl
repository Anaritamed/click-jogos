:- module(utils, [
    limpa_terminal/0,
    bold/2,
    color_code/2,
    color/3,
    colore_amarelo/2,
    colore_vermelho/2,
    colore_verde/2
]).

:- use_module(library(system)).

% Limpa terminal de acordo com o SO
limpa_terminal :-
    current_prolog_flag(windows, true), !,
    shell('cls').
limpa_terminal :-
    shell('clear').

% Função para aplicar negrito
bold(Str, BoldStr) :-
    string_concat('\033[1m', Str, TempStr),
    string_concat(TempStr, '\033[0m', BoldStr).

% Função para criar código ANSI
color_code(Code, ColorCode) :-
    format(string(ColorCode), '\033[38;5;~dm', [Code]).

% Função para criar uma string colorida
color(Code, Str, ColoredStr) :-
    color_code(Code, ColorCode),
    string_concat(ColorCode, Str, TempStr),
    string_concat(TempStr, '\033[0m', ColoredStr).

% Função para alterar cor de string para amarelo
colore_amarelo(Str, ColoredStr) :-
    color(11, Str, ColoredStr).

% Função para alterar cor de string para vermelho
colore_vermelho(Str, ColoredStr) :-
    color(196, Str, ColoredStr).

% Função para alterar cor de string para verde
colore_verde(Str, ColoredStr) :-
    color(46, Str, ColoredStr).