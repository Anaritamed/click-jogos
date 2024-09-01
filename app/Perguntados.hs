module Perguntados where

import System.Info (os)
import Utils (limpaTerminal)
import Data.List (intercalate)
import System.Process (callCommand)
import Data.Text.Internal.Read (IParser(P))
import System.IO (hClose, hGetContents, openFile)
import GHC.IO.IOMode

perguntados :: IO()
perguntados = do 
    limpaTerminal
    putStrLn menuPerguntados
    putStrLn "Digite uma opção: "
    opcao <- getLine

    processarOpcaoInicio opcao

inicioJogo :: IO()
inicioJogo = do
    putStrLn regrasDoJogo

    putStrLn "Nome do jogador 1: "
    jogador1 <- getLine
    putStrLn "Nome do jogador 2: "
    jogador2 <- getLine

    putStrLn $ placar jogador1 jogador2 0 0

    putStrLn escolhaJogador                                                                                                          
    putStrLn "Digite uma opção: "
    opcao <- getLine

    processarOpcaoJogo opcao jogador1 jogador2

temaJogo :: String -> IO()
temaJogo jogador = do
    putStrLn $ escolhaTema jogador
    putStrLn "Digite uma opção: "
    opcao <- getLine

    escolherTemaJogo opcao jogador
    
jogo :: String -> String -> IO()
jogo jogador tema = do 
    let caminhoArquivo = "app/perguntas/" ++ tema
    arquivo <- openFile caminhoArquivo ReadMode
    conteudo <- hGetContents arquivo

    -- Lógica

    putStrLn conteudo
    hClose arquivo

-- Variáveis de texto

menuPerguntados :: String
menuPerguntados = intercalate "\n" 
    [ "  ____   U _____ u   ____      ____     _   _   _   _     _____      _      ____      U  ___ u  ____     "
    , "U|  _\"\\ u\\| ___\"|/U |  _\"\\ uU /\"___|uU |\"|u| | | \\ |\"|   |_ \" _| U  /\"\\  u |  _\"\\      \\\"/_ \\/ / __\"| u  "
    , "\\| |_) |/ |  _|\"   \\| |_) |/\\| |  _ / \\| |\\| |<|  \\| |>    | |    \\/ _ \\/ /| | | |     | | | |<\\___ \\/   "
    , " |  __/   | |___    |  _ <   | |_| |   | |_| |U| |\\  |u   /| |\\   / ___ \\ U| |_| |\\.-,_| |_| | u___) |   "
    , " |_|      |_____|   |_| \\_\\   \\____|  <<\\___/  |_| \\_|   u |_|U  /_/   \\_\\ |____/ u \\_)-\\___/  |____/>>  "
    , " ||>>_    <<   >>   //   \\\\_  _)(|_  (__) )(   ||   \\\\,_._// \\\\_  \\\\    >>  |||_         \\\\     )(  (__) "
    , "(__)__)  (__) (__) (__)  (__)(__)__)     (__)  (_\")  (_/(__) (__)(__)  (__)(__)_)       (__)   (__)      "
    , "                                                                                                           "
    , "                                     BEM-VINDOS AO PERGUNTADOS!                                            "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                        INICIAR(1) | SAIR(2)                                               "
    , "-----------------------------------------------------------------------------------------------------------"
    ]

placar :: String -> String -> Int -> Int -> String
placar jogador1 jogador2 pontuacao1 pontuacao2 = intercalate "\n"
    [ "-----------------------------------------------------------------------------------------------------------"
    , "                                                 PLACAR                                                    "
    , "-----------------------------------------------------------------------------------------------------------"
    , "JOGADOR 1: " ++ jogador1 ++ " - Pontuação: " ++ show pontuacao1
    , "JOGADOR 2: " ++ jogador2 ++ " - Pontuação: " ++ show pontuacao2
    , "-----------------------------------------------------------------------------------------------------------"
    ]

regrasDoJogo :: String
regrasDoJogo = intercalate "\n"
    [ "-----------------------------------------------------------------------------------------------------------"
    , "                                        VAMOS INICIAR O JOGO!                                              "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                            REGRAS DO JOGO                                                 "
    , "                                                                                                           "
    , "1 - O jogo é uma competição entre dois jogadores.                                                          "
    , "2 - No início do jogo, os jogadores escolhem um tema para o quiz.                                          "
    , "3 - A cada rodada, os jogadores irão responder perguntas sobre o tema escolhido.                           "
    , "4 - A pontuação da pergunta é dada pelo seu nível de dificuldade.                                          "
    , "5 - No fim, ganha o jogador que obter mais pontos! :D                                                      "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                                                                                           "
    , "                            ANTES DE INICIAR, DIGITE OS NOMES DOS JOGADORES                                "
    ]

escolhaJogador :: String
escolhaJogador = intercalate "\n"
    [ "                                                                                                           "
    , "                                       QUAL JOGADOR VAI INICIAR?                                           "
    , "                                     (1) JOGADOR 1 | (2) JOGADOR 2                                         "
    , "                                                                                                           "
    ]

escolhaTema :: String -> String
escolhaTema jogador = intercalate "\n"
    [ "-----------------------------------------------------------------------------------------------------------"
    , "                                         Vamos lá, sua vez " ++ jogador ++ "!                            "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                     ESCOLHA UM TEMA DE SUA PREFERÊNCIA                                    "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                             (1) ENTRETENIMENTO                                            "
    , "                                             (2) PROGRAMAÇÃO                                               "
    , "                                             (3) GEOGRAFIA                                                 "
    , "                                             (4) HISTÓRIA                                                  "
    , "                                             (5) CIÊNCIAS                                                  "
    , "-----------------------------------------------------------------------------------------------------------"
    ]

-- Funções para processar opções

processarOpcaoInicio :: String -> IO ()
processarOpcaoInicio opcao
    | opcao == "1" = inicioJogo
    | opcao == "2" = do
        putStrLn "Saindo..."
        return ()
    | otherwise    = do
        putStrLn "Opção inválida!"
        perguntados

processarOpcaoJogo :: String -> String -> String -> IO ()
processarOpcaoJogo opcao jogador1 jogador2
    | opcao == "1" = temaJogo jogador1
    | opcao == "2" = temaJogo jogador2
    | otherwise    = do
        putStrLn "Opção inválida!"
        inicioJogo

escolherTemaJogo :: String -> String -> IO ()
escolherTemaJogo opcao jogador
    | opcao == "1" = jogo jogador "entretenimento.txt"
    | opcao == "2" = jogo jogador "programacao.txt"
    | opcao == "3" = jogo jogador "geografia.txt"
    | opcao == "4" = jogo jogador "historia.txt"
    | opcao == "5" = jogo jogador "ciencias.txt"
    | otherwise    = do
        putStrLn "Opção inválida!"
        temaJogo jogador