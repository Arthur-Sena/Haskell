module JogoDaVelha.Resultados where

import Data.List (transpose)
import Tipos ( vazio, Tabuleiro )

{- -----------------------------------------------------------------------------\
    Módulo com verificação dos possíveis resultados do jogo (vitória ou empate)
\--------------------------------------------------------------------------------}

ganhou :: Tabuleiro -> Char -> Bool
ganhou tabuleiro jogador = 
    any (all (== jogador)) tabuleiro ||
    any (all (== jogador)) (transpose tabuleiro) ||
    all (== jogador) (map (!! 0) tabuleiro) ||
    all (== jogador) (map (!! 2) tabuleiro) ||
    all (== jogador) [tabuleiro !! i !! i | i <- [0..2]] ||
    all (== jogador) [tabuleiro !! i !! (2 - i) | i <- [0..2]]

empatou :: Tabuleiro -> Bool
empatou = all (notElem vazio)
