module Main where

import Data.List
import Data.Array

type Par = (Int,Int)

-- Separa as rodadas em pares
splitPares :: [Int] -> [Par]
splitPares = unfoldr f
  where
    f [] = Nothing -- ignorar lista vazia
    f (10:xs) = Just ((10,0), xs) -- strike não tem par
    f [x] = Just ((x, -1), []) -- último elemento não tem par
    f (x:y:xs) = Just ((x, y), xs) -- par tradicional

-- Descobre o símbolo padrão do par
getSymbol :: Par -> String
getSymbol (x, y)
  | x == 10 = "X _" -- strike
  | x + y == 10 = show x ++ " /" -- spare
  | otherwise = show x ++ " " ++ show y -- par tradicional

-- Descobre o símbolo do trio final de pares
getEndSymbol :: (Par,Par,Par) -> String
getEndSymbol((x,y),(a,b),(c,d))
  | tipoPar(c,d) == -1 = getSymbol (x,y) ++ " | " ++ getSymbol (a,b) ++ " " ++ show c
  | tipoPar(x,y) == 2 && tipoPar(a,b) == 2 && tipoPar(c,d) == 2 = "X" ++ " " ++ "X" ++ " " ++ "X"
  | tipoPar(x,y) == 2 && tipoPar(a,b) == 2 = "X" ++ " " ++ "X" ++ " " ++ show c
  | tipoPar(x,y) == 2 = "X" ++ " " ++ getSymbol (x,y)
  | otherwise = getSymbol (x,y) ++ " | " ++ getSymbol (a,b) ++ " | " ++ getSymbol (c,d)

-- Calcula os pontos da partida
getPontos :: [Par] -> Int
getPontos [] = 0
getPontos(h:t)
  | length t == 1 && tipoPar h == 2 = 10 + pontosPar (head t)
  | length t == 1 && tipoPar h == 1 = 10 + pontosPri (head t)
  | length t == 2 && tipoPar h == 2 && tipoPar (head t) == 2 = 10 + pontosPri (head t) + pontosPri (head (tail t))
  | tipoPar h == 2 && tipoPar (head t) == 2 = 10 + pontosPri (head t) + pontosPri (head (tail t)) + getPontos t
  | tipoPar h == 2 = 10 + pontosPar (head t) + getPontos t
  | tipoPar h == 1 = 10 + pontosPri (head t) + getPontos t
  | tipoPar h == -1 = pontosPri h
  | otherwise = pontosPar h + getPontos t

-- Retorna o valor em pontos do par
pontosPar :: Par -> Int
pontosPar (x,y) = x + y

-- Retorna o valor em pontos do primeiro elemento do par
pontosPri :: Par -> Int
pontosPri (x,y) = x

-- Retorna um inteiro que representa o tipo do par
tipoPar :: Par -> Int
tipoPar (x,y)
  | y == -1 = -1 -- ponto único
  | x == 10 = 2 -- strike
  | x + y == 10 = 1 -- spare
  | otherwise = 0 -- padrão

-- Converte o array de pares em array de strings
toStringArray :: [Par] -> [String]
toStringArray [] = []
toStringArray(h:t)
  | length t == 2 = [getEndSymbol (h,head t,head (tail t))]
  | otherwise = getSymbol h : toStringArray t

-- Converte o array de strings em uma string
output :: ([String],Int) -> String
output (x,y) = concat (intersperse " | " x) ++ " | " ++ show y

-- main é uma função imperativa
main = do

  -- Lê o input
  input <- getLine

  -- Transforma o input em um array
  let pontos = map read (words input) :: [Int]

  -- Agrupa as rodadas em pares
  let rodadas = splitPares pontos

  -- Converte o array de pares em um array de strings
  let y = toStringArray rodadas

  -- Calcula o número de pontos da partida com base no vetor de rodadas (getPontos)
  -- Pega o array de strings e transforma em string (output)
  -- Junta o array de strings com a pontuação (output)
  -- Imprime a string (putStrLn)
  putStrLn (output((y,getPontos rodadas)))

