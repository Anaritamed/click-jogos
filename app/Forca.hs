module Forca where

import Data.Map (Map)
import Data.Text (Text)
import Data.List (intercalate)
import Data.Char (toLower, toUpper, isSpace, isDigit)
import qualified Data.Map as Map
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.IO (hSetEcho, stdin, stdout, hFlush, getChar)
import Utils (limpaTerminal, coloreAmarelo, coloreVerde, bold)

-- Função inicial, que realiza o disparo do jogo forca
forca :: IO()
forca = do
    limpaTerminal
    putStrLn (unlines homeForca)
    menuForca =<< getLine

-- Menu de opções do jogo forca
menuForca :: String -> IO()
menuForca opcao
    | opcao == "1" = processaDadosPartida
    | opcao == "2" = do
        putStrLn "Saindo..."
        exitSuccess
    | otherwise = do
        putStrLn $ bold (coloreAmarelo "Opção inválida!")
        forca

-- Processa dados da interação inicial com os usuários
processaDadosPartida :: IO()
processaDadosPartida = do
    putStrLn $ bold (coloreAmarelo regrasDoJogo)
    putStrLn "Digite o seu nome Jogador 1: "
    jogador1 <- getLine
    putStrLn "Digite o seu nome Jogador 2: "
    jogador2 <- getLine
    putStrLn ("\nCerto " ++ bold jogador1 ++ " qual a palavra a ser adivinhada?")
    palavra <- loopGetCampoValido "palavra" "Palavra inválida!"
    putStrLn "Qual o tema que está relacionado à palavra a ser adivinhada? "
    tema <- loopGetCampoValido "tema" "Tema inválido!"
    jogo palavra tema

-- Lógica principal do jogo
jogo :: String -> String -> IO ()
jogo palavra tema = do
    let mapaLetras = criaMapaLetras (map toLower palavra) -- Mapa de letras em minúsculas
    let estadoAtual = criaStringSublinhados palavra -- cria string sublinhada com o tamanho da palavra. Ex: maçã ele criaria : _ _ _ _

    let loop stringSublinhada erros letrasDigitadas = do
            limpaTerminal
            putStrLn $ atualizaForca erros -- printa forca vazia já que erros inicia em zero

            putStrLn $ bold (coloreAmarelo "\nTEMA: " ++ tema)
            putStrLn stringSublinhada
            putStrLn $ "\nLetras digitadas: " ++ letrasDigitadas
            putStrLn "\nDigite uma letra:"
            letraDigitada <- loopGetCampoValido "letra" "Letra inválida!"
            
            let letra = toLower (head letraDigitada) -- Converte a letra digitada para minúscula
            if letra `elem` letrasDigitadas
                then do
                    putStrLn $ coloreAmarelo "\nEssa letra já foi digitada!\n"
                    threadDelay (900 * 1000) -- 0.9 segundos de delay
                    limpaTerminal
                    loop stringSublinhada erros letrasDigitadas
                else do
                    let letrasDigitadasAtualizada = letra : (" " ++ letrasDigitadas)
                    case Map.lookup letra mapaLetras of
                        Nothing -> do
                            if (erros + 1) >= 6
                                then do
                                    putStrLn (unlines $ cenarioPerda palavra)
                                    threadDelay (3 * 1000000) -- 2 segundos de delay
                                    forca
                                else
                                    loop stringSublinhada (erros + 1) letrasDigitadasAtualizada
                        Just indices -> do
                            let novoEstadoStringSublinhados = atualizaStringSublinhados letra stringSublinhada indices
                            if map toLower novoEstadoStringSublinhados == map toLower palavra
                                then do
                                    putStrLn (unlines $ cenarioVitoria palavra)
                                    threadDelay (4 * 1000000) -- 4 segundos de delay
                                    forca
                                else do
                                    loop novoEstadoStringSublinhados erros letrasDigitadasAtualizada -- se ainda não completou a palavra e não errou o limite.
    loop estadoAtual 0 []

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

campoValido :: String -> Bool
campoValido campo = not (null campo) && not (all isSpace campo) && not (any isDigit campo)

-- Função que requer em loop campo, caso não validado corretamente
loopGetCampoValido :: String -> String -> IO String
loopGetCampoValido campo mensagem = do
    input <- getInput campo
    if isValid campo input
        then return input
        else do
            putStrLn $ bold (coloreAmarelo mensagem)
            loopGetCampoValido campo mensagem

-- Define formato em receber input, caso palavra ele esconde o input com a função hideInput 
getInput :: String -> IO String
getInput "tema" = getLine
getInput "letra" = getLine
getInput "palavra" = hideInput

-- Realiza validação de acordo com o tipo de campo
isValid :: String -> String -> Bool
isValid "tema" input = campoValido input && length input > 1
isValid "letra" input = campoValido input && length input == 1
isValid "palavra" input = campoValido input && length input > 1 && notElem ' ' input

-- Função para ocultar a entrada do usuário e avançar o prompt a cada letra digitada
hideInput :: IO String
hideInput = do
    hSetEcho stdin False  -- Desativa a exibição da entrada
    input <- loop ""
    hSetEcho stdin True   -- Reativa a exibição da entrada
    putStrLn ""           -- Move para a próxima linha após a entrada
    return input
  where
    loop acc = do
        char <- getChar
        if char == '\n'
            then return (reverse acc)
            else do
                putChar '*'
                hFlush stdout
                loop (char : acc)

-- Desenha a forca atualizada a cada erro (contado) passado
atualizaForca :: Int -> String
atualizaForca 0 = intercalate "\n"
        [ "      ________    "
        , "     |/       |   "
        , "     |        §   "
        , "     |            "
        , "     |            "
        , "     |            "
        , "   __|            "
        , "  |  |            "
        , "  ====            "
        ]

atualizaForca 1 = intercalate "\n"
    [ "      ________     "
    , "     |/       |    "
    , "     |        §    "
    , "     |      (*.*)  "
    , "     |             "
    , "     |             "
    , "   __|             "
    , "  |  |             "
    , "  ====             "
    ]

atualizaForca 2 = intercalate "\n"
    [ "      ________     "
    , "     |/       |    "
    , "     |        §    "
    , "     |      (*.*)  "
    , "     |        |    "
    , "     |       [ ]   "
    , "   __|        |    "
    , "  |  |             "
    , "  ====             "
    ]

atualizaForca 3 = intercalate "\n"
    [ "      ________     "
    , "     |/       |    "
    , "     |        §    "
    , "     |      (*.*)  "
    , "     |        |    "
    , "     |       [ ]   "
    , "   __|        |    "
    , "  |  |       /     "
    , "  ====             "
    ]

atualizaForca 4 = intercalate "\n"
    [ "      ________     "
    , "     |/       |    "
    , "     |        §    "
    , "     |      (*.*)  "
    , "     |        |    "
    , "     |       [ ]   "
    , "   __|        |    "
    , "  |  |       / \\  "
    , "  ====             "
    ]

atualizaForca 5 = intercalate "\n"
    [ "      ________     "
    , "     |/       |    "
    , "     |        §    "
    , "     |      (*.*)  "
    , "     |        |    "
    , "     |      /[ ]   "
    , "   __|        |    "
    , "  |  |       / \\  "
    , "  ====             "
    ]

atualizaForca 6 = intercalate "\n"
    [ "      ________     "
    , "     |/       |    "
    , "     |        §    "
    , "     |      (*.*)  "
    , "     |        |    "
    , "     |      /[ ]\\ "
    , "   __|        |    "
    , "  |  |       / \\  "
    , "  ====             "
    ]

regrasDoJogo :: String
regrasDoJogo = intercalate "\n"
    [ "\n📜 Regras do jogo:                                                                                       "
    , "                                                                                                           "
    , "- O jogador 1 será o jogador que dirá a palavra para ser adivinhada, assim como qual tema ela se relaciona."
    , "- O jogador 2 será o jogador que tentará adivinhar a palavra dada pelo jogador 1.                          "
    , "                                                                                                           "
    , "- Caso a palavra contenha uma letra acentuada ou ç, digite exatamente a letra com sua acentuação ou o ç.   "
    , "- Por exemplo, caso a palavra fosse 'Maçã' a != ã, assim como c != ç                                     \n"
    ]

homeForca :: [String]
homeForca =
           [ "                                               "
            ,"   ███████╗ ██████╗ ██████╗  ██████╗ █████╗    "
            ,"   ██╔════╝██╔═══██╗██╔══██╗██╔════╝██╔══██╗   "
            ,"   █████╗  ██║   ██║██████╔╝██║     ███████║   "
            ,"   ██╔══╝  ██║   ██║██╔══██╗██║     ██╔══██║   "
            ,"   ██║     ╚██████╔╝██║  ██║╚██████╗██║  ██║   "
            ,"   ╚═╝      ╚═════╝ ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝   "
            ,"               SEJA BEM VINDO!                 "
            ,"                                               "
            ,"                 (1) JOGAR                     "
            ,"              (2) SAIR DO JOGO                 "
            ,"                                               "]

cenarioPerda :: String -> [String]
cenarioPerda palavra =
                       [" ██████╗  █████╗ ███╗   ███╗███████╗     ██████╗ ██╗   ██╗███████╗██████╗ "
                       ,"██╔════╝ ██╔══██╗████╗ ████║██╔════╝    ██╔═══██╗██║   ██║██╔════╝██╔══██╗"
                       ,"██║  ███╗███████║██╔████╔██║█████╗      ██║   ██║██║   ██║█████╗  ██████╔╝"
                       ,"██║   ██║██╔══██║██║╚██╔╝██║██╔══╝      ██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗"
                       ,"╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗    ╚██████╔╝ ╚████╔╝ ███████╗██║  ██║"
                       ," ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝     ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝"
                       ,"                           A PALAVRA ERA: " ++ bold (map toUpper palavra) ++ "!"]

cenarioVitoria :: String -> [String]
cenarioVitoria palavra =
    ["  ██████╗███████╗██████╗ ████████╗ █████╗     ██████╗ ███████╗███████╗██████╗  ██████╗ ███████╗████████╗ █████╗ ██╗"
    ," ██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔══██╗    ██╔══██╗██╔════╝██╔════╝██╔══██╗██╔═══██╗██╔════╝╚══██╔══╝██╔══██╗██║"
    ," ██║     █████╗  ██████╔╝   ██║   ███████║    ██████╔╝█████╗  ███████╗██████╔╝██║   ██║███████╗   ██║   ███████║██║"
    ," ██║     ██╔══╝  ██╔══██╗   ██║   ██╔══██║    ██╔══██╗██╔══╝  ╚════██║██╔═══╝ ██║   ██║╚════██║   ██║   ██╔══██║╚═╝"
    ," ╚██████╗███████╗██║  ██║   ██║   ██║  ██║    ██║  ██║███████╗███████║██║     ╚██████╔╝███████║   ██║   ██║  ██║██╗"
    ,"  ╚═════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝      ╚═════╝ ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝"
    ,"                               PARABÉNS, VOCÊ VENCEU! A PALAVRA ERA: " ++ bold (map toUpper palavra) ++ "!"]
