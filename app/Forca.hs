module Forca where

import Data.Map (Map)
import Data.Text (Text)
import Data.Char (toLower)
import Utils (limpaTerminal)
import qualified Data.Text as T
import qualified Data.Map as Map


forca :: IO()
forca = do
    limpaTerminal
    putStrLn"                                            "
    putStrLn"   ███████╗ ██████╗ ██████╗  ██████╗ █████╗ "
    putStrLn"   ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗"
    putStrLn"   █████╗  ██║   ██║██████╔╝██║     ███████║"
    putStrLn"   ██╔══╝  ██║   ██║██╔══██╗██║     ██╔══██║"
    putStrLn"   ██║     ╚██████╔╝██║  ██║╚██████╗██║  ██║"
    putStrLn"   ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝"
    putStrLn"               SEJA BEM VINDO!              "
    putStrLn"                                            "
    putStrLn"                 (1) JOGAR                  "
    putStrLn"             (2) RETORNAR AO MENU           "
    putStrLn"                                            "
    opcao <- getLine
    if opcao == "1" then do
        jogo
    else if opcao == "2" then do
        putStrLn "tem que voltar ao menu"
    else do
        putStrLn "Opção inválida!"

jogo :: IO()
jogo = do
    let regras = "Regras do jogo: \n" ++
                 "- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona.\n" ++
                 "- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.\n"
    putStrLn regras
    putStrLn "Digite o seu nome Jogador 1: "
    jogador1 <- getLine
    putStrLn "\nDigite o seu nome Jogador 2: "
    jogador2 <- getLine
    putStrLn ("\nCerto " ++ jogador1 ++ ", Qual a palavra da rodada? ")
    palavra <- getLine
    let mapaLetras = criaMapaLetras palavra
    let estadoAtual = criaStringSublinhados palavra
    print mapaLetras
    putStrLn "\nQual o tema que está relacionado à palavra a ser adivinhada? "
    tema <- getLine

    -- Inicia o jogo com 0 erros
    let loop estado erros = do
            atualizaForca erros
            putStrLn $ "TEMA: " ++ tema
            putStrLn estado
            putStrLn "Digite uma letra:"
            letra <- getLine
            if length letra > 1
                then do
                    putStrLn "Adivinhe com letra e não palavras!"
                    loop estado erros
                else do
                    let char = toLower (head letra)
                    case Map.lookup char mapaLetras of
                        Nothing -> do
                            let novosErros = erros + 1
                            if novosErros >= 6
                                then do
                                    putStrLn "Game over! Você perdeu." 
                                    forca
                                else loop estado novosErros
                        Just indices -> do
                            let novoEstado = atualizaStringSublinhados char estado indices
                            if novoEstado == palavra
                                then putStrLn $ "Parabéns! Você acertou: " ++ novoEstado
                                else loop novoEstado erros
    loop estadoAtual 0

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

atualizaForca :: Int -> IO()
atualizaForca 0 = do
    putStrLn "      ________   \n"
    putStrLn "     |        |  \n"
    putStrLn "     |        |  \n"
    putStrLn "     |           \n"
    putStrLn "     |           \n"
    putStrLn "     |           \n"
    putStrLn "   __|           \n"
    putStrLn "  |  |           \n"
    putStrLn "---------------  \n"
atualizaForca 1 = do
    putStrLn "      ________    \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "     |      (*u*) \n"
    putStrLn "     |            \n"
    putStrLn "     |            \n"
    putStrLn "   __|            \n"
    putStrLn "  |  |            \n"
    putStrLn "---------------   \n"
atualizaForca 2 = do
    putStrLn "      ________    \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "     |      (*u*) \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "   __|        |   \n"
    putStrLn "  |  |            \n"
    putStrLn "---------------   \n"
atualizaForca 3 = do
    putStrLn "      ________    \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "     |      (*u*) \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "   __|        |   \n"
    putStrLn "  |  |       /    \n"
    putStrLn "---------------   \n"
atualizaForca 4 = do
    putStrLn "      ________    \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "     |      (*u*) \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "   __|        |   \n"
    putStrLn "  |  |       / \\ \n"
    putStrLn "---------------   \n"
atualizaForca 5 = do
    putStrLn "      ________    \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "     |      (*u*) \n"
    putStrLn "     |        |   \n"
    putStrLn "     |       /|   \n"
    putStrLn "   __|        |   \n"
    putStrLn "  |  |       / \\ \n"
    putStrLn "---------------   \n"
atualizaForca 6 = do
    putStrLn "      ________    \n"
    putStrLn "     |        |   \n"
    putStrLn "     |        |   \n"
    putStrLn "     |      (*u*) \n"
    putStrLn "     |        |   \n"
    putStrLn "     |       /|\\ \n"
    putStrLn "   __|        |   \n"
    putStrLn "  |  |       / \\ \n"
    putStrLn "---------------   \n"