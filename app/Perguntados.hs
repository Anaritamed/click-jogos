module Perguntados where

import System.Info (os)
import Utils (limpaTerminal)
import Data.List (intercalate)
import System.Process (callCommand)
import Data.Text.Internal.Read (IParser(P))
import System.IO (hClose, hGetContents, openFile)
import GHC.IO.IOMode
import Data.Char (toLower)

perguntados :: IO()
perguntados = do
    limpaTerminal
    putStrLn menuPerguntados
    putStrLn "Digite uma opção: "
    opcao <- getLine
    processaOpcaoMenu opcao

processaOpcaoMenu :: String -> IO ()
processaOpcaoMenu opcao
    | opcao == "1" = inicioJogo
    | opcao == "2" = do
        putStrLn "Saindo..."
        return ()
    | otherwise    = do
        putStrLn "Opção inválida!"
        perguntados

inicioJogo :: IO()
inicioJogo = do
    putStrLn regrasDoJogo

    putStrLn "Nome do jogador 1: "
    jogador1 <- getLine
    putStrLn "Nome do jogador 2: "
    jogador2 <- getLine

    let jogadores = [jogador1, jogador2]
    putStrLn $ placar jogadores [0, 0]
    temaJogo jogadores

temaJogo :: [String] -> IO()
temaJogo jogadores = do
    putStrLn escolhaTema
    putStrLn "Digite uma opção: "
    opcao <- getLine
    processaTemaJogo opcao jogadores

processaTemaJogo :: String -> [String] -> IO ()
processaTemaJogo opcao jogadores
    | opcao == "1" = jogo jogadores "entretenimento.txt"
    | opcao == "2" = jogo jogadores "programacao.txt"
    | opcao == "3" = jogo jogadores "geografia.txt"
    | opcao == "4" = jogo jogadores "historia.txt"
    | opcao == "5" = jogo jogadores "ciencias.txt"
    | otherwise    = do
        putStrLn "Opção inválida!"
        temaJogo jogadores

jogo :: [String] -> String -> IO()
jogo jogadores tema = do
    let caminhoArquivo = "app/perguntas/" ++ tema
    arquivo <- openFile caminhoArquivo ReadMode
    conteudo <- hGetContents arquivo
    let perguntas = extraiPerguntas $ lines conteudo
    resultado <- quiz perguntas jogadores [0, 0] 1
    putStrLn $ placar jogadores resultado
    hClose arquivo
    -- vencedor
    -- jogar novamente?

extraiPerguntas :: [String] -> [(String, [String], Int, String)]
extraiPerguntas [] = []
extraiPerguntas (pergunta:linhas) =
    let alternativas = take 4 linhas
        pontos = extraiPontos (linhas !! 4)
        resposta = extraiResposta (linhas !! 5)
    in (pergunta, alternativas, pontos, resposta) : extraiPerguntas (drop 7 linhas)

extraiPontos :: String -> Int
extraiPontos linha =
    read (words linha !! 2) :: Int

extraiResposta :: String -> String
extraiResposta linha =
    [linha !! 10]

quiz :: [(String, [String], Int, String)] -> [String] -> [Int] -> Int -> IO [Int]
quiz [] _ pontuacoes rodada = return pontuacoes
quiz ((pergunta, alternativas, pontos, respostaCorreta):linhas) jogadores pontuacoes rodada = do
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn pergunta
    mapM_ putStrLn alternativas
    putStrLn $ "\nValendo " ++ show pontos ++ " pontos!"
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn $ jogadorDaVez jogadores rodada ++ ", sua resposta: "
    resposta <- getLine
    if map toLower resposta == respostaCorreta
        then do
            putStrLn $ "\nResposta correta! Você ganhou " ++ show pontos ++ " pontos!"
            quiz linhas jogadores (aumentaPontuacao pontuacoes pontos rodada) (rodada + 1)
        else do
            putStrLn "\nResposta incorreta!"
            quiz linhas jogadores pontuacoes (rodada + 1)

jogadorDaVez :: [String] -> Int -> String
jogadorDaVez (jogador1:jogador2) rodada =
    if rodada `mod` 2 == 1
        then jogador1
        else head jogador2

aumentaPontuacao :: [Int] -> Int -> Int -> [Int]
aumentaPontuacao (pontuacao1:pontuacao2) pontos rodada =
    if rodada `mod` 2 == 1
        then pontuacao1 + pontos:pontuacao2
        else pontuacao1:[head pontuacao2 + pontos]

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
    , "                                       INICIAR (1) | SAIR (2)                                              "
    , "-----------------------------------------------------------------------------------------------------------"
    ]

regrasDoJogo :: String
regrasDoJogo = intercalate "\n"
    [ "-----------------------------------------------------------------------------------------------------------"
    , "                                        VAMOS INICIAR O JOGO!                                              "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           REGRAS DO JOGO                                                  "
    , "                                                                                                           "
    , "1 - O jogo é uma competição entre dois jogadores.                                                          "
    , "2 - No início do jogo, os jogadores escolhem um tema para o quiz.                                          "
    , "3 - A cada rodada, os jogadores irão responder perguntas sobre o tema escolhido.                           "
    , "4 - A pontuação da pergunta é dada pelo seu nível de dificuldade.                                          "
    , "5 - No fim, ganha o jogador que obter mais pontos! :D                                                      "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                            ANTES DE INICIAR, DIGITE OS NOMES DOS JOGADORES                                "
    ]

placar :: [String] -> [Int] -> String
placar jogadores pontuacoes = intercalate "\n"
    [ "-----------------------------------------------------------------------------------------------------------"
    , "                                               PLACAR                                                      "
    , "-----------------------------------------------------------------------------------------------------------"
    , "JOGADOR 1: " ++ head jogadores ++ " - Pontuação: " ++ show (head pontuacoes)
    , "JOGADOR 2: " ++ jogadores !! 1 ++ " - Pontuação: " ++ show (pontuacoes !! 1)
    , "-----------------------------------------------------------------------------------------------------------"
    ]

escolhaTema :: String
escolhaTema = intercalate "\n"
    [ "                                      ESCOLHA UM TEMA PARA O QUIZ                                          "
    , "-----------------------------------------------------------------------------------------------------------"
    , "                                           (1) ENTRETENIMENTO                                              "
    , "                                           (2) PROGRAMAÇÃO                                                 "
    , "                                           (3) GEOGRAFIA                                                   "
    , "                                           (4) HISTÓRIA                                                    "
    , "                                           (5) CIÊNCIAS                                                    "
    , "-----------------------------------------------------------------------------------------------------------"
    ]
