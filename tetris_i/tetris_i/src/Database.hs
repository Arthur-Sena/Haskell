{-|
Module      : Database
Description : Tipos de Dados utilizados para o jogo. 
Copyright   : Bárbara Dias de Sena, 2020
License     : GPL-3
Maintainer  : babi.dias.sena@hotmail.com
Stability   : experimental
Portability : GHC

Contém todos os tipos de dados que compõem o jogo.
-}

module Database where

import Graphics.Gloss


-- | Tamanho do lado de um bloco de peça.
tam :: Float
tam = 20


-- | Limites do jogo.
limiteX, limiteY :: Float
limiteX = 140
limiteY = 280


-- | Uma coordenada (x, y).
type Coord = (Float, Float)


-- | Tipo da peça baseado no Tetraminó.
data Tetramino = I | L | J | T | O | Z | S
  deriving (Eq, Enum)


-- | Cada peça é composta por um tipo (Tetraminó), uma cor, um foco e os blocos que a compõem.
data Peca = Peca { tipo       :: Tetramino
                 , cor        :: Color
                 , foco       :: Coord
                 , blocosPeca :: [Coord] }
  deriving Eq


-- | Um mundo é composto por seus blocos, uma peça ativa, uma lista de próximas peças,
-- pontuação do jogo, um timer para aplicar a gravidade na peça, 
-- a quantidade de fileiras que foram completas na jogada, e uma flag de fim de jogo.
data Mundo = Mundo { blocosPintados    :: [(Coord, Color)]
                   , pecaAtual         :: Peca
                   , proxPecas         :: [Tetramino]
                   , pontuacao         :: Int
                   , timer             :: Int
                   , fileirasCompletas :: Int
                   , gameOver          :: Bool }
  deriving Eq


-- | A direção do movimento pode ser para esquerda, para direita, 
-- para baixo ou rotação da peça.
data Direcao = Dir | Esq | Baixo | Rotaciona


-- | Cria uma peça nova de acordo com o tipo.
criaPeca :: Tetramino -> Peca
criaPeca t = 
  case t of
    I -> Peca I cyan   f [(x, y-tam), f, (x, y+tam), (x,   y+2*tam)]
    L -> Peca L orange f [(x, y+tam), f, (x, y-tam), (x+tam, y-tam)]
    J -> Peca J blue   f [(x, y+tam), f, (x, y-tam), (x-tam, y-tam)]
    T -> Peca T violet f [(x-tam, y), f, (x, y+tam), (x+tam, y    )]
    O -> Peca O yellow f [(x-tam, y), f, (x, y-tam), (x-tam, y-tam)]
    Z -> Peca Z red    f [(x-tam, y), f, (x, y-tam), (x+tam, y-tam)]
    S -> Peca S green  f [(x+tam, y), f, (x, y-tam), (x-tam, y-tam)]
    where f@(x, y) = (0, y')
          y' = if t == T
                  then 300
                  else 320


