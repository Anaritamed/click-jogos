module Utils where

import System.Info (os)
import System.Process (callCommand)

-- Limpa texto do terminal, o comando varia de acordo com o sistema operacional, Windows é cls e distribuições Unix é clear.
limpaTerminal :: IO()
limpaTerminal
    | os == "linux" || os == "darwin" || os == "freebsd" || os == "openbsd" = callCommand "clear"
    | os == "windows" = callCommand "cls"
    | otherwise = putStrLn ""