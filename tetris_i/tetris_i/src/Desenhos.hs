{-|
Module      : Desenhos
Description : Funções que desenham o jogo na tela. 
Copyright   : Bárbara Dias de Sena, 2020
License     : GPL-3
Maintainer  : babi.dias.sena@hotmail.com
Stability   : experimental
Portability : GHC

Contém todas as funções que desenham o jogo, isto é, todas as funções que retornam uma Picture.
-}

module Desenhos where

import Graphics.Gloss
import Database

-- | Desenha um único bloco na tela.
desenhaBloco :: Color -> Coord -> Picture
desenhaBloco c (x, y) = translate x y $ pictures [ color c $ rectangleSolid tam tam
                                                 , color (greyN 0.3) $ rectangleWire tam tam]


-- | Desenha uma peça na tela.
desenhaPeca :: Peca -> Picture
desenhaPeca (Peca _ c _ bs) = pictures $ map (desenhaBloco c) bs


-- | Desenha uma seta do teclado.
seta :: Picture
seta = pictures $ [color (greyN 0.2) $ rectangleSolid 30 30]
               ++ [color black $ line [(0,-10), (0,10)]]
               ++ [color black $ polygon [(5,0), (0, 10), (-5, 0)]]


-- | Desenha as instruções do jogo na tela.
instrucoes :: Picture
instrucoes = pictures $ [translate (-280) (-25) seta]
                     ++ [translate (-315) (-60) $ rotate (-90) seta]
                     ++ [translate (-280) (-60) $ rotate 180 seta]
                     ++ [translate (-245) (-60) $ rotate 90 seta]
                     ++ [translate (-280) (-180) $ color (greyN 0.2) $ rectangleSolid 180 30]
                     ++ [translate (-355) (-190) (scale 0.14 0.14 $ color black $ Text "Barra de espaco")]
                     ++ [translate (-405) (-55)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-400) (-70)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "esquerda")]
                     ++ [translate (-222) (-55)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-210) (-70)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "direita")]
                     ++ [translate (-310) 10     (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Rotaciona")]
                     ++ [translate (-295) (-5)   (scale 0.1 0.1 $ color (greyN 0.5) $ Text "peca")]
                     ++ [translate (-315) (-90)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-295) (-105) (scale 0.1 0.1 $ color (greyN 0.5) $ Text "baixo")]
                     ++ [translate (-345) (-210) (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para o fundo")]


-- | Desenha o quadro de fim de jogo na tela.
gameOverTela :: Mundo -> Picture
gameOverTela m
  | gameOver m = pictures $ [color (greyN 0.2) $ rectangleSolid 580 300]
                         ++ [color (greyN 0.4) $ rectangleWire 580 300]
                         ++ [translate (-220) 40 (scale 0.55 0.55 $ color red $ Text "Voce perdeu!")]
                         ++ [translate (-200) (-60) (scale 0.3 0.3 $ color white
                                                                   $ Text ("Pontuacao final: " ++ show (pontuacao m)))]
  | otherwise  = blank


-- | Desenha o mundo atual.
desenhaMundo :: Mundo -> Picture
desenhaMundo m = pictures $ map (\(x, y) -> desenhaBloco y x) (blocosPintados m) -- blocos do jogo
                         ++ [color (greyN 0.5) $ rectangleWire 300 580]          -- borda
                         ++ [desenhaPeca proxP]                                  -- proxima peça
                         ++ [translate (-400) 200 ( scale 0.2 0.2                -- pontuação
                                                  $ color (greyN 0.5)
                                                  $ Text pont)]
                         ++ [translate 160 200 ( scale 0.2 0.2                   -- texto "Proxima peça"
                                               $ color (greyN 0.5)
                                               $ Text "Proxima peca:")]
                         ++ [instrucoes]                                         -- instruções
                         ++ [gameOverTela m]                                     -- tela final
  where proxP  = Peca (tipo proxP') (cor proxP') (0,0) (map (\(x,y) -> (x+260, y-190)) (blocosPeca proxP'))
        proxP' = criaPeca $ head $ proxPecas m
        pont   = "Pontuacao: " ++ show (pontuacao m)


