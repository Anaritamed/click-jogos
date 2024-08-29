module Utils where
import System.Info (os)
import System.Process (callCommand)

-- Limpa texto do terminal, o comando varia de acordo com o sistema operacional, Windows é cls e distribuições Unix é clear.
limpaTerminal :: IO ()
limpaTerminal = do
    if os == "linux" || os == "darwin" || os == "freebsd" || os == "openbsd"
        then callCommand "clear"
        else if os == "windows"
            then callCommand "cls"
            else putStrLn ""