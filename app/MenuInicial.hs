module MenuInicial where

import Forca (forca)
import Perguntados (perguntados)
import JogoDaVelha (jogoDaVelha)
import Utils (limpaTerminal)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

renderizaTelaInicial :: IO()
renderizaTelaInicial = do
    putStrLn "==================================================="
    putStrLn "   ___ _ _      _       __                         "
    putStrLn "  / __\\ (_) ___| | __   \\ \\  ___   __ _  ___  ___ "
    putStrLn " / /  | | |/ __| |/ /    \\ \\/ _ \\ / _` |/ _ \\/ __|"
    putStrLn "/ /___| | | (__|   <  /\\_/ / (_) | (_| | (_) \\__ \\"
    putStrLn "\\____/|_|_|\\___|_|\\_\\ \\___/ \\___/ \\__, |\\___/|___/"
    putStrLn "                                  |___/            "
    putStrLn "==================================================="
    putStrLn "          MENU (1)        |        SAIR (2)        "
    putStrLn "==================================================="
    putStrLn "                                                   "
    putStrLn "Digite uma opção: "
    opcao <- getLine
    if opcao == "1" then do
        menu
    else if opcao == "2" then do
        putStrLn "Saindo..."
        exitSuccess
    else do
        putStrLn "Opção inválida!"
        threadDelay (700 * 1000)
        limpaTerminal
        renderizaTelaInicial
    return ()


menu :: IO()
menu = do
    putStrLn "                                                   "
    putStrLn "==================================================="
    putStrLn "                ESCOLHA UM JOGO                    "
    putStrLn "==================================================="
    putStrLn "                    FORCA (1)                      "
    putStrLn "                 PERGUNTADOS (2)                   " 
    putStrLn "                JOGO DA VELHA (3)                  "
    putStrLn "==================================================="
    putStrLn "                                                   "
    putStrLn "Digite uma opção: "
    opcao <- getLine
    handleInteracaoEscolhaJogos opcao

handleInteracaoEscolhaJogos :: String -> IO()
handleInteracaoEscolhaJogos "1" = forca
handleInteracaoEscolhaJogos "2" = perguntados
handleInteracaoEscolhaJogos "3" = jogoDaVelha