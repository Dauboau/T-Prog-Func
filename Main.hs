
module Main where

-- Tipo de dado Rodada
data Rodada = Rodada {
  i :: Int
  , jogada1 :: Int
  , jogada2 :: Int
  , jogada3 :: Int
  , pontos :: Int
}
  deriving (Show)

-- Separa as rodadas em um array
splitRodadas :: ([Int],Int) -> [Rodada]
splitRodadas ([],i) = []
splitRodadas (h:t,i)
  | i == 10 && length t == 2 = [Rodada{i=i,jogada1 = h,jogada2 = head t,jogada3 = head (tail t),pontos = h + head t + head (tail t)}] -- última rodada (tripla)
  | i == 10 = [Rodada{i=i,jogada1 = h,jogada2 = head t,jogada3 = -1,pontos = h + head t}] -- última rodada (dupla)
  | h == 10 = Rodada{i=i,jogada1 = h,jogada2 = -1,jogada3 = -1,pontos = h + head t + head (tail t)}:splitRodadas (t,i+1) -- strike
  | h + head t == 10 = Rodada{i=i,jogada1 = h,jogada2 = head t,jogada3 = -1,pontos = h + head t + head (tail t)}:splitRodadas (tail t,i+1) -- spare
  | otherwise = Rodada{i=i,jogada1 = h,jogada2 = head t,jogada3 = -1,pontos = h + head t}:splitRodadas (tail t,i+1) -- normal

-- Retorna a pontuação da partida
somaPontos :: [Rodada] -> Int
somaPontos = foldr ((+) . pontos) 0

-- Retorna uma string com a pontuação do jogo na formatação adequada
rodadasToString :: [Rodada] -> String
rodadasToString [] = []
rodadasToString (h:t)
  | i h == 10 && jogada1 h == 10 && jogada2 h == 10 && jogada3 h == 10 = "X" ++ " " ++ "X" ++ " " ++ "X" ++ " | " -- última rodada, 3 strikes consecutivos
  | i h == 10 && jogada1 h == 10 && jogada2 h == 10 = "X" ++ " " ++ "X" ++ " " ++ show (jogada3 h) ++ " | " -- última rodada, 2 strikes consecutivos
  | i h == 10 && jogada1 h == 10 && jogada2 h + jogada3 h == 10 = "X" ++ " " ++ show (jogada2 h) ++ " /" ++ " | " -- última rodada, 1 strike seguido de spare
  | i h == 10 && jogada1 h == 10 = "X" ++ " " ++ show (jogada2 h) ++ " " ++ show (jogada3 h) ++ " | " -- última rodada, 1 strike
  | i h == 10 && jogada1 h + jogada2 h == 10 && jogada3 h == 10 = show (jogada1 h) ++ " /" ++ " " ++ "X" ++ " | " -- última rodada, 1 spare seguido de strike
  | i h == 10 && jogada1 h + jogada2 h == 10 = show (jogada1 h) ++ " /" ++ " " ++ show (jogada3 h) ++ " | " -- última rodada, 1 spare
  | jogada1 h == 10 = "X _" ++ " | " ++ rodadasToString t -- strike
  | jogada1 h + jogada2 h == 10 = show (jogada1 h) ++ " /" ++ " | " ++ rodadasToString t -- spare
  | otherwise = show (jogada1 h) ++ " " ++ show (jogada2 h) ++ " | " ++ rodadasToString t -- normal

-- main é uma função imperativa
main = do

  -- Lê o input
  input <- getLine

  -- Transforma o input em um array
  let pontos = map read (words input) :: [Int]

  -- Dividi o array de pontos em rodadas
  let rodadas = splitRodadas (pontos,1)

  -- Gera o output e junta aos pontos
  let output = rodadasToString rodadas ++ show (somaPontos rodadas)

  -- Imprime o output
  putStrLn output