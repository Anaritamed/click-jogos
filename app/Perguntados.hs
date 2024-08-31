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
    putStrLn "1 - O jogo é uma competição entre dois jogadores.                                                          "
    putStrLn "2 - Cada jogador terá a oportunidade de responder perguntas de diferentes temas.                           "
    putStrLn "3 - A cada resposta correta, o jogador ganha 1 ponto.                                                      "
    putStrLn "4 - O jogador que iniciar continua respondendo perguntas até errar.                                        "
    putStrLn "5 - O segundo jogador terá a oportunidade de responder perguntas se o primeiro errar ou finalizar o quiz.  "      
    putStrLn "6 - Ganha o jogador que obter mais pontos no fim.                                                          "
    putStrLn "7 - Caso os jogadores empatem, os dois são considerados vencedores.                                        "
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

temaJogo :: String ->  IO()
temaJogo jogador = do
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn $ "                                         Vamos lá, sua vez " ++ jogador ++ "                             "
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

    return ()
    
jogo :: String -> String -> IO()
jogo jogador tema = do 
    let caminhoArquivo = "app/perguntas/" ++ tema
    arquivo <- openFile caminhoArquivo ReadMode
    conteudo <- hGetContents arquivo

    -- Lógica

    putStrLn conteudo
    hClose arquivo

    return ()
