module Utils where

import System.Info (os)
import System.IO (hFlush, stdout)
import System.Process (callCommand, readProcess)

-- Limpa texto do terminal, o comando varia de acordo com o sistema operacional, Windows é cls e distribuições Unix é clear.
limpaTerminal :: IO()
limpaTerminal
    | os == "linux" || os == "darwin" || os == "freebsd" || os == "openbsd" = callCommand "clear"
    | os == "windows" = callCommand "cls"
    | otherwise = putStrLn ""

-- Função para criar código ANSI
colorCode :: Int -> String
colorCode n = "\ESC[38;5;" ++ show n ++ "m"

-- Função para criar uma string colorida
colore :: Int -> String -> String
colore code str = colorCode code ++ str ++ "\ESC[0m"

-- Função para aplicar negrito
bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"