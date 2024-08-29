module Perguntados where
import System.Info (os)
import System.Process (callCommand)
import Control.Monad.RWS (MonadState(put))
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
        jogo
    else if opcao == "2" then do
        putStrLn "Saindo..."
        return ()
    else do
        putStrLn "Opção inválida!"
        perguntados

jogo :: IO()
jogo = do
    putStrLn "Nome do jogador 1: "
    jogador1 <- getLine
    putStrLn "Nome do jogador 2: "
    jogador2 <- getLine
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                             PLACAR INICIAL                                                "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                 Jogador 1:  Pontuação: 0          |         Jogador 2:  Pontuação: 0                      "
    putStrLn "-----------------------------------------------------------------------------------------------------------"
    putStrLn "                                                                                                           "
    putStrLn "                                       QUAL JOGADOR VAI INICIAR?                                           "
    putStrLn "                                     (1) JOGADOR 1 | (2) JOGADOR 2                                         "
    putStrLn "                                                                                                           "
    putStrLn "Digite uma opção: "
    opcao <- getLine
    if opcao == "1" then do
        putStrLn "Jogador 1 inicia!"
    else if opcao == "2" then do
        putStrLn "Jogador 2 inicia!"
    else do
        putStrLn "Opção inválida!"
        jogo
    return ()
    


-- Limpa texto do terminal, o comando varia de acordo com o sistema operacional, Windows é cls e distribuições Unix é clear.
limpaTerminal :: IO ()
limpaTerminal = do
    if os == "linux" || os == "darwin" || os == "freebsd" || os == "openbsd"
        then callCommand "clear"
        else if os == "windows"
            then callCommand "cls"
            else putStrLn ""