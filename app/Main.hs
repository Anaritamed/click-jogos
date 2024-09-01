module Main where

import MenuInicial (inicio)
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout, hGetEncoding, mkTextEncoding, hSetEncoding, stderr, stdin, Handle)

main :: IO()
main = do
    -- Permite imprimir caracteres unicode no windows
    mapM_ makeSafe [stdout, stdin, stderr]
    inicio

-- Deixa um handle seguro para imprimir caracteres unicode, se nao houver codificação definida, não faz nada, se tiver, cria uma nova codificação de texto
makeSafe :: Handle -> IO ()
makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding (takeWhile (/= '/') (show ce) ++ "//TRANSLIT") >>=
      hSetEncoding h