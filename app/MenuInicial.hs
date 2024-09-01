module MenuInicial where
import System.Exit (exitSuccess)
import Text.XHtml.Frameset (p)
import Perguntados (perguntados)

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
    if opcao == "1" then do
        putStrLn "Iniciando jogo da forca..."
    else if opcao == "2" then do
        perguntados
    else if opcao == "3" then do
        putStrLn "Iniciando jogo da velha..."
    else do
        putStrLn "Opção inválida!"
        menu