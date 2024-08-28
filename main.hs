import System.Exit (exitSuccess)
import Text.XHtml.Frameset (p)

main :: IO()
main = do
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
    if opcao == "1" then do
        menu
    else if opcao == "2" then do
        putStrLn "Saindo..."
        exitSuccess
    else do
        putStrLn "Opção inválida!"
        main
    return ()


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