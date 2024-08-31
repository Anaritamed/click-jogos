module JogoDaVelha where
import Control.Monad (replicateM_)
import Data.Map (Map)
import Data.Text (Text)
import Data.Char (toLower)
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

pegaPartesDoTabuleiro :: Int -> [a] -> [[a]]
pegaPartesDoTabuleiro _ [] = []
pegaPartesDoTabuleiro n xs = take n xs : pegaPartesDoTabuleiro n (drop n xs)

printaPartes :: [[[String]]] -> IO ()
printaPartes [] = return ()
printaPartes (x:xs) = do
    mapM_ (putStrLn . intercalate " | ") (formata x)
    putStrLn (replicate (length (head (head x)) * 3 + 2) '-')
    printaPartes xs

formata :: [[a]] -> [[a]]
formata ([]:_) = []
formata x = map head x : formata (map tail x)

atualizaTabuleiro :: [[String]] -> Char -> Int -> [[String]]
atualizaTabuleiro tabuleiro jogador pos = take (pos - 1) tabuleiro ++ [bloco] ++ drop pos tabuleiro
  where
    bloco = case jogador of
        'X' -> xis
        'O' -> bola
        _   -> vazio

checaVitoria :: [[String]] -> Char -> Bool
checaVitoria tabuleiro jogador = any (all (== bloco)) padroesVitoria
  where
    bloco = case jogador of
        'X' -> xis
        'O' -> bola
        _   -> vazio
    padroesVitoria = [
        [head tabuleiro, tabuleiro !! 1, tabuleiro !! 2],
        [tabuleiro !! 3, tabuleiro !! 4, tabuleiro !! 5],
        [tabuleiro !! 6, tabuleiro !! 7, tabuleiro !! 8],
        [head tabuleiro, tabuleiro !! 3, tabuleiro !! 6],
        [tabuleiro !! 1, tabuleiro !! 4, tabuleiro !! 7],
        [tabuleiro !! 2, tabuleiro !! 5, tabuleiro !! 8],
        [head tabuleiro, tabuleiro !! 4, tabuleiro !! 8],
        [tabuleiro !! 2, tabuleiro !! 4, tabuleiro !! 6]
        ]

checaEmpate :: [[String]] -> Bool
checaEmpate = notElem vazio

pegaInput :: [[String]] -> Char -> IO (Char, Int)
pegaInput tabuleiro jogador = do
    putStrLn $ "jogador " ++ [jogador] ++ ", enter your move (format: 1-9):"
    input <- getLine
    let pos = read input :: Int
    return (jogador, pos)

gameLoop :: [[String]] -> Char -> IO ()
gameLoop tabuleiro jogador = do
    let chunks = pegaPartesDoTabuleiro 3 tabuleiro
    printaPartes chunks
    (jogadorAtual, pos) <- pegaInput tabuleiro jogador
    let updatedtabuleiro = atualizaTabuleiro tabuleiro jogadorAtual pos
    if checaVitoria updatedtabuleiro jogadorAtual
        then do
            let updatedChunks = pegaPartesDoTabuleiro 3 updatedtabuleiro
            printaPartes updatedChunks
            putStrLn $ "jogador " ++ [jogadorAtual] ++ " wins!"
        else if checaEmpate updatedtabuleiro
            then do
                let updatedChunks = pegaPartesDoTabuleiro 3 updatedtabuleiro
                printaPartes updatedChunks
                putStrLn "It's a draw!"
            else gameLoop updatedtabuleiro (if jogador == 'X' then 'O' else 'X')
