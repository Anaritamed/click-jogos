module Perguntados where

import System.Info (os)
import System.IO (hClose, hGetContents, openFile)
import Utils (limpaTerminal)
import System.Process (callCommand)
import Data.Text.Internal.Read (IParser(P))
import GHC.IO.IOMode

perguntados :: IO()
perguntados = do 
    limpaTerminal
    putStrLn "  ____   U _____ u   ____      ____     _   _   _   _     _____      _      ____      U  ___ u  ____     "
    putStrLn "U|  _\"\\ u\\| ___\"|/U |  _\"\\ uU /\"___|uU |\"|u| | | \\ |\"|   |_ \" _| U  /\"\\  u |  _\"\\      \\\"/_ \\/ / __\"| u  "
    putStrLn "\\| |_) |/ |  _|\"   \\| |_) |/\\| |  _ / \\| |\\| |<|  \\| |>    | |    \\/ _ \\/ /| | | |     | | | |<\\___ \\/   "
    putStrLn " |  __/   | |___    |  _ <   | |_| |   | |_| |U| |\\  |u   /| |\\   / ___ \\ U| |_| |\\.-,_| |_| | u___) |   "
    putStrLn " |_|      |_____|   |_| \\_\\   \\____|  <<\\___/  |_| \\_|   u |_|U  /_/   \\_\\ |____/ u \\_)-\\___/  |____/>>  "
    putStrLn " ||>>_    <<   >>   //   \\\\_  _)(|_  (__) )(   ||   \\\\,_._// \\\\_  \\\\    >>  |||_         \\\\     )(  (__) "
    putStrLn "(__)__)  (__) (__) (__)  (__)(__)__)     (__)  (_\")  (_/(__) (__)(__)  (__)(__)_)       (__)   (__)      "
    putStrLn "                                                                                                           "
    putStrLn "                                     BEM-VINDOS AO PERGUNTADOS!                                            "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                        INICIAR(1) | SAIR(2)                                               "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "Digite uma opção: "
    opcao <- getLine

    if opcao == "1" then do
        inicioJogo
    else if opcao == "2" then do
        putStrLn "Saindo..."
        return ()
    else do
        putStrLn "Opção inválida!"
        perguntados

inicioJogo :: IO()
inicioJogo = do
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                        VAMOS INICIAR O JOGO!                                              "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                            REGRAS DO JOGO                                                 "
    putStrLn "                                                                                                           "
    putStrLn "1 - O jogo é uma competição entre dois jogadores.                                                          "
    putStrLn "2 - No início do jogo, os jogadores escolhem um tema para o quiz.                                          "
    putStrLn "3 - A cada rodada, os jogadores irão responder perguntas sobre o tema escolhido.                           "
    putStrLn "4 - A pontuação da pergunta é dada pelo seu nível de dificuldade.                                          "
    putStrLn "5 - No fim, ganha o jogador que obter mais pontos! :D                                                      "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                                                                                           "
    putStrLn "                            ANTES DE INICIAR, DIGITE OS NOMES DOS JOGADORES                                "
    putStrLn "Nome do jogador 1: "
    jogador1 <- getLine
    putStrLn "Nome do jogador 2: "
    jogador2 <- getLine
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                             PLACAR INICIAL                                                "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn $ "JOGADOR 1: " ++ jogador1 ++ " - Pontuação: 0                                                             " 
    putStrLn $ "JOGADOR 2: " ++ jogador2 ++ " - Pontuação: 0                                                             " 
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                                                                                           "
    putStrLn "                                       QUAL JOGADOR VAI INICIAR?                                           "
    putStrLn "                                     (1) JOGADOR 1 | (2) JOGADOR 2                                         "
    putStrLn "                                                                                                           "
    putStrLn "Digite uma opção: "
    opcao <- getLine

    if opcao == "1" then do
        temaJogo jogador1
    else if opcao == "2" then do
        temaJogo jogador2
    else do
        putStrLn "Opção inválida!"
        inicioJogo

temaJogo :: String -> IO()
temaJogo jogador = do
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn $ "                                         Vamos lá, sua vez " ++ jogador ++ "!                            "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                     ESCOLHA UM TEMA DE SUA PREFERÊNCIA                                    "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                             (1) ENTRETENIMENTO                                            "
    putStrLn "                                             (2) PROGRAMAÇÃO                                               "
    putStrLn "                                             (3) GEOGRAFIA                                                 "
    putStrLn "                                             (4) HISTÓRIA                                                  "
    putStrLn "                                             (5) CIÊNCIAS                                                  "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "Digite uma opção: "
    opcao <- getLine

    if opcao == "1" then do
        jogo jogador "entretenimento.txt"
    else if opcao == "2" then do
        jogo jogador "programacao.txt"
    else if opcao == "3" then do
        jogo jogador "geografia.txt"
    else if opcao == "4" then do
        jogo jogador "historia.txt"
    else if opcao == "5" then do
        jogo jogador "ciencias.txt"
    else do
        putStrLn "Opção inválida!"
        temaJogo jogador
    
jogo :: String -> String -> IO()
jogo jogador tema = do 
    let caminhoArquivo = "app/perguntas/" ++ tema
    arquivo <- openFile caminhoArquivo ReadMode
    conteudo <- hGetContents arquivo

    -- Lógica

    putStrLn conteudo
    hClose arquivo
