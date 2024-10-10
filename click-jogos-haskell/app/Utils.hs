module Utils where

import System.Info (os)
import System.IO (hFlush, stdout)
import System.Process (callCommand, readProcess)

-- Limpa terminal de acordo com o SO
limpaTerminal :: IO ()
limpaTerminal = do
    case os of
        "mingw32" -> callCommand "cls"  -- Windows
        _         -> callCommand "clear"  -- Linux/MacOS

-- Função para aplicar negrito
bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"

-- Função para criar código ANSI
colorCode :: Int -> String
colorCode n = "\ESC[38;5;" ++ show n ++ "m"

-- Função para criar uma string colorida
color :: Int -> String -> String
color code str = colorCode code ++ str ++ "\ESC[0m"

-- Função para alterar cor de string para amarelo
coloreAmarelo :: String -> String
coloreAmarelo p = do color 11 p

-- Função para alterar cor de string para amarelo
coloreVermelho :: String -> String
coloreVermelho p = do color 196 p

-- Função para alterar cor de string para amarelo
coloreVerde :: String -> String
coloreVerde p = do color 46 p