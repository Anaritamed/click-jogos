module Forca where

import Data.Map (Map)
import Data.Text (Text)
import Data.Char (toLower)
import Utils (limpaTerminal)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)


forca :: IO()
forca = do
    limpaTerminal
    putStrLn "                                               "
    putStrLn "   ███████╗ ██████╗ ██████╗  ██████╗ █████╗    "
    putStrLn "   ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗   "
    putStrLn "   █████╗  ██║   ██║██████╔╝██║     ███████║   "
    putStrLn "   ██╔══╝  ██║   ██║██╔══██╗██║     ██╔══██║   "
    putStrLn "   ██║     ╚██████╔╝██║  ██║╚██████╗██║  ██║   "
    putStrLn "   ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝   "
    putStrLn "               SEJA BEM VINDO!                 "
    putStrLn "                                               "
    putStrLn "                 (1) JOGAR                     "
    putStrLn "             (2) RETORNAR AO MENU              "
    putStrLn "                                               "
    opcao <- getLine
    if opcao == "1" then do
        jogo
    else if opcao == "2" then do
        putStrLn "tem que voltar ao menu"
    else do
        putStrLn "Opção inválida!"
        forca

jogo :: IO ()
jogo = do
    handleInteracaoInicialForca
    palavra <- getLine

    let mapaLetras = criaMapaLetras (map toLower palavra) -- Mapa de letras em minúsculas
    let estadoAtual = criaStringSublinhados palavra -- cria string sublinhada com o tamanho da palavra. Ex: maçã ele criaria : _ _ _ _
    
    putStrLn "\nQual o tema que está relacionado à palavra a ser adivinhada? "
    tema <- getLine

    let loop stringSublinhada erros letrasDigitadas = do
            atualizaForca erros -- printa forca vazia já que erros inicia em zero

            putStrLn $ "\nTEMA: " ++ tema
            putStrLn stringSublinhada
            putStrLn $ "Letras digitadas: " ++ letrasDigitadas
            putStrLn "\nDigite uma letra:"
            letraDigitada <- getLine
            
            if length letraDigitada > 1
                then do
                    putStrLn $ colored 11 "\nAdivinhe com letra e não palavras!"
                    loop stringSublinhada erros letrasDigitadas
                else do
                    let letra = toLower (head letraDigitada) -- Converte a letra digitada para minúscula
                    if letra `elem` letrasDigitadas
                        then do
                            putStrLn $ colored 11 "\nEssa letra já foi digitada!\n"
                            loop stringSublinhada erros letrasDigitadas
                        else do
                            let letrasDigitadasAtualizadas = letra : (" " ++ letrasDigitadas)
                            case Map.lookup letra mapaLetras of
                                Nothing -> do
                                    if (erros + 1) >= 6
                                        then do
                                            handleCenarioPerda
                                            threadDelay (2 * 1000000) -- 2 segundos de delay
                                            forca
                                        else
                                            loop stringSublinhada (erros + 1) letrasDigitadasAtualizadas
                                Just indices -> do
                                    let novoEstado = atualizaStringSublinhados letra stringSublinhada indices
                                    if map toLower novoEstado == map toLower palavra
                                        then
                                            putStrLn $ colored 2 "Parabéns!" ++ "Você acertou: " ++ novoEstado
                                        else
                                            loop novoEstado erros letrasDigitadasAtualizadas -- se ainda não completou a palavra e não errou o limite.
    loop estadoAtual 0 []

-- interação inicial com os jogadores
handleInteracaoInicialForca :: IO()
handleInteracaoInicialForca = do
    let regras = "Regras do jogo: \n" ++
                "- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona.\n" ++
                "- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.\n" ++
                "- Caso a palavra contenha uma letra acentuada ou ç, digite exatamente a letra com sua acentuação ou o ç.\n" ++
                "- Por exemplo, caso a palavra fosse 'Maçã' a ≠ ã, assim como c ≠ ç\n"
    putStrLn regras
    putStrLn "Digite o seu nome Jogador 1: "
    jogador1 <- getLine
    putStrLn "Digite o seu nome Jogador 2: "
    jogador2 <- getLine
    putStrLn ("\nCerto " ++ jogador1 ++ ", Qual a palavra da rodada? ")

-- Função que cria a string com sublinhados
criaStringSublinhados :: String -> String
criaStringSublinhados palavra = replicate (length palavra) '_'

-- Função que cria um mapa de letra -> posições
criaMapaLetras :: String -> Map.Map Char [Int]
criaMapaLetras palavra =
    Map.fromListWith (++) [(letra, [i]) | (i, letra) <- zip [0..] palavra]

-- Função para atualizar a string de sublinhados com a letra correta
atualizaStringSublinhados :: Char -> String -> [Int] -> String
atualizaStringSublinhados letra sublinhados indices =
    [if i `elem` indices then letra else sublinhados !! i | i <- [0..length sublinhados - 1]]

-- desenha a forca atualizada a cada erro (contado) passado
atualizaForca :: Int -> IO()
atualizaForca 0 = do
    putStrLn "      ________    \n"
    putStrLn "     |/       |   \n"
    putStrLn "     |        §   \n"
    putStrLn "     |            \n"
    putStrLn "     |            \n"
    putStrLn "     |            \n"
    putStrLn "   __|            \n"
    putStrLn "  |  |            \n"
    putStrLn "  ====             \n"
atualizaForca 1 = do
    putStrLn "      ________     \n"
    putStrLn "     |/       |    \n"
    putStrLn "     |        §    \n"
    putStrLn "     |      (*.*)  \n"
    putStrLn "     |             \n"
    putStrLn "     |             \n"
    putStrLn "   __|             \n"
    putStrLn "  |  |             \n"
    putStrLn "  ====             \n"
atualizaForca 2 = do
    putStrLn "      ________     \n"
    putStrLn "     |/       |    \n"
    putStrLn "     |        §    \n"
    putStrLn "     |      (*.*)  \n"
    putStrLn "     |        |    \n"
    putStrLn "     |       [ ]   \n"
    putStrLn "   __|        |    \n"
    putStrLn "  |  |             \n"
    putStrLn "  ====             \n"
atualizaForca 3 = do
    putStrLn "      ________     \n"
    putStrLn "     |/       |    \n"
    putStrLn "     |        §    \n"
    putStrLn "     |      (*.*)  \n"
    putStrLn "     |        |    \n"
    putStrLn "     |       [ ]   \n"
    putStrLn "   __|        |    \n"
    putStrLn "  |  |       /     \n"
    putStrLn "  ====             \n"
atualizaForca 4 = do
    putStrLn "      ________     \n"
    putStrLn "     |/       |    \n"
    putStrLn "     |        §    \n"
    putStrLn "     |      (*.*)  \n"
    putStrLn "     |        |    \n"
    putStrLn "     |       [ ]   \n"
    putStrLn "   __|        |    \n"
    putStrLn "  |  |       / \\  \n"
    putStrLn "  ====             \n"
atualizaForca 5 = do
    putStrLn "      ________     \n"
    putStrLn "     |/       |    \n"
    putStrLn "     |        §    \n"
    putStrLn "     |      (*.*)  \n"
    putStrLn "     |        |    \n"
    putStrLn "     |      /[ ]   \n"
    putStrLn "   __|        |    \n"
    putStrLn "  |  |       / \\  \n"
    putStrLn "  ====             \n"
atualizaForca 6 = do
    putStrLn "      ________     \n"
    putStrLn "     |/       |    \n"
    putStrLn "     |        §    \n"
    putStrLn "     |      (*.*)  \n"
    putStrLn "     |        |    \n"
    putStrLn "     |      /[ ]\\ \n"
    putStrLn "   __|        |    \n"
    putStrLn "  |  |       / \\  \n"
    putStrLn "  ====             \n"

handleCenarioPerda :: IO()
handleCenarioPerda = do
    putStrLn " ██████╗  █████╗ ███╗   ███╗███████╗     ██████╗ ██╗   ██╗███████╗██████╗ "
    putStrLn "██╔════╝ ██╔══██╗████╗ ████║██╔════╝    ██╔═══██╗██║   ██║██╔════╝██╔══██╗"
    putStrLn "██║  ███╗███████║██╔████╔██║█████╗      ██║   ██║██║   ██║█████╗  ██████╔╝"
    putStrLn "██║   ██║██╔══██║██║╚██╔╝██║██╔══╝      ██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗"
    putStrLn "╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗    ╚██████╔╝ ╚████╔╝ ███████╗██║  ██║"
    putStrLn " ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝     ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝"

-- Função para criar código ANSI
colorCode :: Int -> String
colorCode n = "\ESC[38;5;" ++ show n ++ "m"

-- Função para criar uma string colorida
colored :: Int -> String -> String
colored code str = colorCode code ++ str ++ "\ESC[0m"