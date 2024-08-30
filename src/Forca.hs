module Forca where

import Utils (limpaTerminal)

forca :: IO()
forca = do
    limpaTerminal
    putStrLn"                                            "
    putStrLn"   ███████╗ ██████╗ ██████╗  ██████╗ █████╗ "
    putStrLn"   ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗"
    putStrLn"   █████╗  ██║   ██║██████╔╝██║     ███████║"
    putStrLn"   ██╔══╝  ██║   ██║██╔══██╗██║     ██╔══██║"
    putStrLn"   ██║     ╚██████╔╝██║  ██║╚██████╗██║  ██║"
    putStrLn"   ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝"
    putStrLn"               SEJA BEM VINDO!              "
    putStrLn"                                            "
    putStrLn"                 (1) JOGAR                  "
    putStrLn"             (2) RETORNAR AO MENU           "
    putStrLn"                                            "
    opcao <- getLine
    if opcao == "1" then do
        jogo
    else if opcao == "2" then do
        putStrLn "tem que voltar ao menu"
    else do
        putStrLn "Opção inválida!"

jogo :: IO()
jogo = do
    let regras = "Regras do jogo: \n" ++
                 "- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona.\n" ++
                 "- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.\n"
    putStrLn regras
    putStrLn "Digite o seu nome Jogador 1: \n"
    jogador1 <- getLine
    putStrLn "Digite o seu nome Jogador 2: \n"
    jogador2 <- getLine
    putStrLn ("Certo " ++ jogador1 ++ " Qual a palavra da rodada? \n")
    palava <- getLine
    putStrLn "Qual o tema que está relacionado à palavra a ser adivinhada? \n"
    tema <- getLine
    limpaTerminal
    putStrLn "Certo..."