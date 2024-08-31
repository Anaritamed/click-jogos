module JogoDaVelha where
import Control.Monad (replicateM_)

import Data.Map (Map)
import Data.Text (Text)
import Data.Char (toLower)
-- import Utils (limpaTerminal)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (intercalate)
import Distribution.ModuleName (main)


jogoDaVelha :: IO()
jogoDaVelha = do
    putStrLn "============================================================================"
    putStrLn "      _                         _        __     __   _ _                    "
    putStrLn "     | | ___   __ _  ___     __| | __ _  \\ \\   / /__| | |__   __ _        "
    putStrLn "  _  | |/ _ \\ / _` |/ _ \\   / _` |/ _` |  \\ \\ / / _ \\ | '_ \\ / _` |   "
    putStrLn " | |_| | (_) | (_| | (_) | | (_| | (_| |   \\ V /  __/ | | | | (_| |        "
    putStrLn "  \\___/ \\___/ \\__, |\\___/   \\__,_|\\__,_|    \\_/ \\___|_|_| |_|\\__,_|"
    putStrLn "              |___/                                                         "
    putStrLn "============================================================================"
    putStrLn "                 SEJA BEM VINDO!                                            "
    putStrLn"                                                                             "
    putStrLn"                 (1) JOGAR                                                   "
    putStrLn"                 (2) RETORNAR AO MENU                                        "
    putStrLn"                                                                             "
    opcao <- getLine
    if opcao == "1" then do
        main
    else if opcao == "2" then do
        putStrLn "tem que voltar ao menu"
    else do
        putStrLn "Opção inválida!"
    
bola :: [String]
bola = [
    "     OOOOOOO     ",
    "   O         O   ",
    "  O           O  ",
    "  O           O  ",
    "   O         O   ",
    "     OOOOOOO     "
    ]

xis :: [String]
xis = [
    "     \\\\    //    ",
    "      \\\\  //     ",
    "       \\\\//      ",
    "       //\\\\      ",
    "      //  \\\\     ",
    "     //    \\\\    "
    ]

vazio :: [String]
vazio = [
    "                 ",
    "                 ",
    "                 ",
    "                 ",
    "                 ",
    "                 "
    ]


arrayFinal :: [[String]]
arrayFinal = replicate 9 vazio

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main:: IO()
main = printLn"jogo"
