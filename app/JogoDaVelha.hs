module JogoDaVelha where

import Data.Map (Map)
import Data.Text (Text)
import Data.Char (toLower)
import Utils (limpaTerminal)
import qualified Data.Text as T
import qualified Data.Map as Map


jogoDaVelha :: IO()
jogoDaVelha = do
    limpaTerminal
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
    colocaX
    --opcao <- getLine
    --if opcao == "1" then do
        --montaTabuleiro
    --else do
      --  colocaBola
        

montaTabuleiro :: IO ()
montaTabuleiro = do
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"        __________________|__________________|__________________    "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"        __________________|__________________|__________________    "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "
    putStrLn"                          |                  |                      "

colocaBola :: IO ()
colocaBola = do
    putStrLn "      OOOOOOO        "
    putStrLn "    O         O      "
    putStrLn "   O           O     "
    putStrLn "   O           O     "
    putStrLn "    O         O      "
    putStrLn "      OOOOOOO        "

colocaX :: IO ()
colocaX = do
    putStrLn "    \\\\    //     "
    putStrLn "     \\\\  //      "
    putStrLn "      \\\\//       "
    putStrLn "      //\\\\       "
    putStrLn "     //  \\\\      "
    putStrLn "    //    \\\\     "

