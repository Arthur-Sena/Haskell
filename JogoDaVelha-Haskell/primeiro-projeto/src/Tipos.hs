module Tipos where

{---------------------------------------\
    Definição de tipos e constantes
\---------------------------------------}

type Tabuleiro = [[Char]]
type Coord = (Int, Int)

vazio, jogadorX, jogadorO :: Char
vazio = ' '
jogadorX = 'X'
jogadorO = 'O'

tamanhoTabuleiro :: Int
tamanhoTabuleiro = 3