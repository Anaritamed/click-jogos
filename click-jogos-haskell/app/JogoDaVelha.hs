module JogoDaVelha where

import Control.Monad (replicateM_)
import Data.Map (Map)
import Data.Text (Text)
import Data.Char (isDigit)
import Utils (limpaTerminal, coloreAmarelo, bold, coloreVerde, coloreVermelho)
import System.Exit (exitSuccess)
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Concurrent (threadDelay)

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
    putStrLn "                            SEJA BEM VINDO!                                 "
    putStrLn "                                                                            "
    putStrLn "                              (1) JOGAR                                     "
    putStrLn "                           (2) SAIR DO JOGO                                 "
    putStrLn "                                                                            "
    opcao <- getLine
    if opcao == "1" then do
        iniciarJogo
    else if opcao == "2" then do
        putStrLn "Saindo do jogo..."
        threadDelay (500 * 1000)
        exitSuccess
    else do
        putStrLn "Opção inválida!"
        threadDelay (500 * 1000)
        jogoDaVelha

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
    "    \\\\    //     ",
    "     \\\\  //      ",
    "      \\\\//       ",
    "      //\\\\       ",
    "     //  \\\\      ",
    "    //    \\\\     "
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
printaPartes [x] = mapM_ (putStrLn . intercalate " | ") (formata x)
printaPartes (x:xs) = do
    mapM_ (putStrLn . intercalate " | ") (formata x)
    let tileWidth = length (head (head x))
    let lineLength = tileWidth * 3 + 2 * 2
    putStrLn (replicate lineLength '-')
    printaPartes xs

formata :: [[String]] -> [[String]]
formata = transpose

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

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

reiniciaJogo :: IO()
reiniciaJogo = do
    putStrLn"Reiniciando o jogo em"
    putStrLn"3"
    threadDelay (1 * 1000000)
    putStrLn "2"
    threadDelay (1 * 1000000)
    putStrLn "1"
    threadDelay (1 * 1000000)
    jogoDaVelha

checaEmpate :: [[String]] -> Bool
checaEmpate = notElem vazio

pegaInput :: [[String]] -> Char -> IO (Char, Int)
pegaInput tabuleiro jogador = do
    putStrLn $ bold $ coloreAmarelo $ "\nVez do jogador " ++ [jogador] ++ ". Insira seu movimento (posições 1-9):"
    input <- getLine
    if length input /= 1 || not (isDigit (head input)) then do
        putStrLn $ bold $ coloreVermelho "\nEntrada inválida. Por favor, insira um número entre 1 e 9."
        pegaInput tabuleiro jogador
    else
        let pos = read input :: Int
        in if pos < 1 || pos > 9 then do
            putStrLn $ bold $ coloreVermelho "\nEntrada inválida. Por favor, insira um número entre 1 e 9."
            pegaInput tabuleiro jogador
        else if posicaoOcupada tabuleiro pos then do
            putStrLn $ bold $ coloreVermelho "\nPosição já ocupada. Por favor, escolha outra posição."
            pegaInput tabuleiro jogador
        else
            return (jogador, pos)

posicaoOcupada :: [[String]] -> Int -> Bool
posicaoOcupada tabuleiro pos = let row = (pos - 1) `div` 3
                                   col = (pos - 1) `mod` 3
                               in tabuleiro !! (pos - 1) /= vazio

gameLoop :: [[String]] -> Char -> IO ()
gameLoop tabuleiro jogador = do
    let partes = pegaPartesDoTabuleiro 3 tabuleiro
    printaPartes partes
    (jogadorAtual, pos) <- pegaInput tabuleiro jogador
    limpaTerminal
    let tabuleiroAtualizado = atualizaTabuleiro tabuleiro jogadorAtual pos
    if checaVitoria tabuleiroAtualizado jogadorAtual
        then do
            let partesAtualizadas = pegaPartesDoTabuleiro 3 tabuleiroAtualizado
            printaPartes partesAtualizadas
            putStrLn $ bold $ coloreVerde $ "Jogador " ++ [jogadorAtual] ++ " vence!\n"
            reiniciaJogo
        else if checaEmpate tabuleiroAtualizado
            then do
                let partesAtualizadas = pegaPartesDoTabuleiro 3 tabuleiroAtualizado
                printaPartes partesAtualizadas
                putStrLn $ bold $ coloreVerde "Temos um empate!"
                reiniciaJogo
            else gameLoop tabuleiroAtualizado (if jogador == 'X' then 'O' else 'X')

iniciarJogo :: IO ()
iniciarJogo = gameLoop arrayFinal 'X'
