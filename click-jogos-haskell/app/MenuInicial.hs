module MenuInicial where

import Forca (forca)
import Perguntados (perguntados)
import JogoDaVelha (jogoDaVelha)
import Utils (limpaTerminal)
import Data.List (intercalate)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

inicio :: IO()
inicio = do
    limpaTerminal
    putStrLn telaInicial
    putStrLn "Digite uma opção: "
    opcao <- getLine
    processaOpcaoInicio opcao

processaOpcaoInicio :: String -> IO ()
processaOpcaoInicio opcao = case opcao of
    "1" -> menu
    "2" -> sair
    _   -> do
        putStrLn "Opção inválida!"
        voltaInicio

menu :: IO()
menu = do
    putStrLn jogos
    putStrLn "Digite uma opção: "
    opcao <- getLine
    processaEscolhaJogo opcao

sair :: IO()
sair = do
    putStrLn "Saindo..."
    exitSuccess

voltaInicio :: IO()
voltaInicio = do
    threadDelay (700 * 1000)
    limpaTerminal
    inicio

processaEscolhaJogo :: String -> IO ()
processaEscolhaJogo opcao = case opcao of
    "1" -> forca
    "2" -> perguntados
    "3" -> jogoDaVelha
    _   -> do
        putStrLn "Opção inválida!"
        processaEscolhaJogo =<< getLine

telaInicial :: String
telaInicial = intercalate "\n"
    [ "==================================================="
    , "   ___ _ _      _       __                         "
    , "  / __\\ (_) ___| | __   \\ \\  ___   __ _  ___  ___ "
    , " / /  | | |/ __| |/ /    \\ \\/ _ \\ / _` |/ _ \\/ __|"
    , "/ /___| | | (__|   <  /\\_/ / (_) | (_| | (_) \\__ \\"
    , "\\____/|_|_|\\___|_|\\_\\ \\___/ \\___/ \\__, |\\___/|___/"
    , "                                  |___/            "
    , "==================================================="
    , "          MENU (1)        |        SAIR (2)        "
    , "==================================================="
    , "                                                   "
    ]

jogos :: String
jogos = intercalate "\n"
    [ "                                                   "
    , "==================================================="
    , "                ESCOLHA UM JOGO                    "
    , "==================================================="
    , "                    FORCA (1)                      "
    , "                 PERGUNTADOS (2)                   " 
    , "                JOGO DA VELHA (3)                  "
    , "==================================================="
    , "                                                   "
    ]