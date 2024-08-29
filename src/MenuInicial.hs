module MenuInicial where

import Utils (limpaTerminal)
import System.Exit (exitSuccess)
import Forca (forca)

renderizaTelaInicial :: IO()
renderizaTelaInicial = do
    limpaTerminal
    putStrLn "==================================================="
    putStrLn "   ___ _ _      _       __                         "
    putStrLn "  / __\\ (_) ___| | __   \\ \\  ___   __ _  ___  ___ "
    putStrLn " / /  | | |/ __| |/ /    \\ \\/ _ \\ / _` |/ _ \\/ __|"
    putStrLn "/ /___| | | (__|   <  /\\_/ / (_) | (_| | (_) \\__ \\"
    putStrLn "\\____/|_|_|\\___|_|\\_\\ \\___/ \\___/ \\__, |\\___/|___/"
    putStrLn "                                  |___/            "
    putStrLn "==================================================="
    putStrLn "          MENU(1)        |         SAIR(2)         "
    putStrLn "==================================================="
    putStrLn "                                                   "
    putStrLn "Digite uma opção: "
    opcao <- getLine
    handleInteracaoTelaInicial opcao

handleInteracaoTelaInicial :: String -> IO()
handleInteracaoTelaInicial opcao
    | opcao == "1" = menu
    | opcao == "2" = do
        putStrLn "Saindo..."
        exitSuccess
    | otherwise = do
        putStrLn "Opção inválida!"
        limpaTerminal
        renderizaTelaInicial

menu :: IO()
menu = do
    putStrLn "                                                   "
    putStrLn "==================================================="
    putStrLn "                ESCOLHA UM JOGO                    "
    putStrLn "==================================================="
    putStrLn "                    FORCA (1)                      "
    putStrLn "                 PERGUNTADOS (3)                   " 
    putStrLn "                JOGO DA VELHA (2)                  "
    putStrLn "==================================================="
    putStrLn "                                                   "
    putStrLn "Digite uma opção: "
    opcao <- getLine
    handleInteracaoEscolhaJogos opcao

handleInteracaoEscolhaJogos :: String -> IO()
handleInteracaoEscolhaJogos "1" = forca
handleInteracaoEscolhaJogos "2" = putStrLn "perguntados" --temporário
handleInteracaoEscolhaJogos "3" = putStrLn "Jogo da Velha" --temporário