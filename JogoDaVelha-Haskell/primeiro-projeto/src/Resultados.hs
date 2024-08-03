module Resultados where

import Data.List (transpose)
import Tipos ( vazio, Tabuleiro )

{- -----------------------------------------------------------------------------\
    Módulo com verificação dos possíveis resultados do jogo (vitória ou empate)
\--------------------------------------------------------------------------------}

-- Função para verificar se alguém ganhou
ganhou :: Tabuleiro -> Char -> Bool
ganhou tabuleiro jogador = 
    -- Verificar linhas, colunas e diagonais
    any (all (== jogador)) tabuleiro ||
    any (all (== jogador)) (transpose tabuleiro) ||
    all (== jogador) (map (!! 0) tabuleiro) ||
    all (== jogador) (map (!! 2) tabuleiro) ||
    all (== jogador) [tabuleiro !! i !! i | i <- [0..2]] ||
    all (== jogador) [tabuleiro !! i !! (2 - i) | i <- [0..2]]

-- Função para verificar se o jogo empatou
empatou :: Tabuleiro -> Bool
empatou = all (notElem vazio)
