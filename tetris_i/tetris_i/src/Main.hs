{-|
Module      : Main
Description : Implementação de um jogo de Tetris utilizando a biblioteca Gloss.
Copyright   : Bárbara Dias de Sena, 2020
License     : GPL-3
Maintainer  : babi.dias.sena@hotmail.com
Stability   : experimental
Portability : GHC

O intuito do projeto é fazer um jogo de Tetris em Haskell utilizando a biblioteca Gloss.
-}

{-# LANGUAGE TupleSections #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Database
import Desenhos


-- | Lista de Tetraminós gerada aleatóriamente.
listaPecas :: IO [Tetramino]
listaPecas = map (\x -> [I .. S] !! x) <$> fmap (randomRs (0 :: Int, 6 :: Int)) getStdGen


-- | Mundo inicial do jogo que contém apenas a primeira peça.
mundoInicial :: [Tetramino] -> Mundo
mundoInicial []     = undefined
mundoInicial (t:ts) = Mundo (map (, cor p) (blocosPeca p)) p ts 0 3 0 False
  where p = criaPeca t


-- | Se possível, movimenta a peça de acordo com a tecla pressionada, 
-- atualizando os blocos do mundo e da peça ativa.
joga :: Event -> Mundo -> Mundo
joga (EventKey (SpecialKey k) Down _ _) m =
  if gameOver m                                           -- Verifica se o jogo acabou. 
     then m                                               -- Se sim, não faz nada.
     else m {blocosPintados = blocos, pecaAtual = novaP}  -- Se não, realiza o movimento.
      where
        novaP = case k of
                 KeyRight -> if podeMover m Dir       then move Dir       $ pecaAtual m else pecaAtual m
                 KeyLeft  -> if podeMover m Esq       then move Esq       $ pecaAtual m else pecaAtual m
                 KeyDown  -> if podeMover m Baixo     then move Baixo     $ pecaAtual m else pecaAtual m
                 KeyUp    -> if podeMover m Rotaciona then move Rotaciona $ pecaAtual m else pecaAtual m
                 KeySpace -> moveFundo m
                 _        -> pecaAtual m
        -- Blocos do mundo sem a peça atual acrescido da peça transladada.
        blocos = filter (\(x, _) -> x `notElem` blocosPeca (pecaAtual m)) (blocosPintados m)
              ++ map (, cor novaP) (blocosPeca novaP)

joga _ m = m



-- | Verifica se um movimento pode ser feito dado um mundo e uma direção.
podeMover :: Mundo -> Direcao -> Bool
podeMover m d = not (any (`elem` bsM) bsP                               -- Verifica se a peça vai colidir com um bloco
                  || any ((== -280) . snd) (blocosPeca $ pecaAtual m))  -- do mundo ou se a peça chegou ao fundo.
  where
    bsP = blocosPeca (move d (pecaAtual m))                                        -- Blocos da peça atual depois de movida.
    bsM = filter (`notElem` blocosPeca (pecaAtual m)) (map fst (blocosPintados m)) -- Blocos do mundo sem a peça atual.



-- | Move uma peça para uma direção dada, atualizando os blocos da mesma.
move :: Direcao -> Peca -> Peca
move d p@(Peca t c (x, y) bs) =
  case d of
    Dir   -> if any ((==  limiteX) . fst) bs then p                         -- Verifica se chegou no limite da direita.
                else Peca t c (x+tam, y) (map (\(a, b) -> (a+tam, b)) bs)
    Esq   -> if any ((== -limiteX) . fst) bs then p                         -- Verifica se chegou no limite da esquerda.
                else Peca t c (x-tam, y) (map (\(a, b) -> (a-tam, b)) bs)
    Baixo -> if any ((== -limiteY) . snd) bs then p                         -- Verifica se chegou no fundo.
                else Peca t c (x, y-tam) (map (\(a, b) -> (a, b-tam)) bs)
    Rotaciona -> if t == O                  -- Peça do tipo O não precisa rotacionar.
                    then p
                    else Peca t c (novaP !! 1) novaP
                      where
                        novaP = map (\(a, b) -> (a-sinal, b)) pecaRot -- Coordenadas da peça rotacionada.
                        sinal                                         -- Verifica por quanto e por qual lado
                          | any (> 0) volta = maximum volta           -- a peça ultrapassou o limite.
                          | any (< 0) volta = minimum volta
                          | otherwise = 0
                        volta   = map voltaLimite pecaRot             -- Verifica se algum bloco está fora dos limites
                                                                      -- do jogo depois de rotacionado.
                        pecaRot = map rotaciona bs                    -- Rotaciona a peca.
                        rotaciona (a, b) = (x+y-b, y-x+a)             -- Atualiza a coordenada de um bloco rotacionado.



-- | Devolve por quanto que um bloco ultrapassou os limites da tela.
voltaLimite :: Coord -> Float
voltaLimite (a, _)
  | a >  limiteX = a-limiteX
  | a < -limiteX = a+limiteX
  | otherwise    = 0


-- | Movimenta a peça direto para o fundo da tela, movendo pra baixo recursivamente.
moveFundo :: Mundo -> Peca
moveFundo m = if podeMover m Baixo
                 then moveFundo $ m {pecaAtual = move Baixo (pecaAtual m)}
                 else pecaAtual m


-- | Atualiza o mundo.
atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo _ m@(Mundo bs p ps pont t f g)
  | not (podeMover m Baixo) && any (\(_, y) -> y >= limiteY) (blocosPeca p) = m {gameOver = True}  -- Verifica se o jogo acabou.
  | not (podeMover m Baixo) =
    if novoBs /= bs                                                     -- Verifica se completou alguma fileira.
       then Mundo novoBs (criaPeca $ head ps) (tail ps) novaPont t 0 g  -- Se sim, limpa a(s) fileira(s) e atualiza a pontuação.
       else Mundo bs (criaPeca $ head ps) (tail ps) pont t f g          -- Caso contrário, apenas cria a próxima peça.

  | t == 0 = Mundo blocos (move Baixo p) ps pont 3 f g      -- Se o timer chegou ao 0, move a peça 
                                                            -- para baixo e recomeça o timer.
  | otherwise = Mundo bs p ps pont (t-1) f g                -- Senão, decrementa o timer.

  where novoM    = limpaFileira m                                   -- Conta e tira as fileiras completas do mundo.
        novoBs   = blocosPintados novoM                             -- Blocos do mundo sem as fileiras completas.
        novaPont = pont + atualizaPont (fileirasCompletas novoM)    -- Atualiza a pontuação.

        blocos   = filter (\(x, _) -> x `notElem` blocosPeca p) bs                    -- Blocos do mundo com a peça
                ++ map (, cor (move Baixo p)) (blocosPeca (move Baixo p))    -- movida para baixo.


-- | Limpa as fileiras completas do mundo recursivamente (de acordo com os blocos da peça),
-- atualizando o número de fileiras completas.
limpaFileira :: Mundo -> Mundo
limpaFileira m@(Mundo bs p ps pont t f g)
  | null bsPeca = m                     -- Se não há mais blocos da peça para verificar, não faz nada.
  | otherwise =
    if all (`elem` map fst bs) fileira  -- Verifica se uma fileira está completa.

       -- Se sim, chama a função recursivamente com o mundo sem a fileira completa, 
       -- incrementando o numero de fileiras completas.
       then limpaFileira (Mundo (map desce (filter (\(x, _) -> x `notElem` fileira) bs)) p ps pont t (f+1) g)

       -- Caso contrário, chama a função para os próximos blocos da peça.
       else limpaFileira (Mundo bs (p {blocosPeca = tail bsPeca}) ps pont t f g)

         where fileira = [(x, snd $ head bsPeca) | x <- [-limiteX, -limiteX+tam .. limiteX]] -- Coordenadas de uma fileira
                                                                                             -- que contém um bloco da peça.
               bsPeca  = blocosPeca p                               -- Blocos da peça ativa.
               desce   = \((a, b), c) -> if b > snd (head bsPeca)   -- Move para baixo todos os blocos
                                            then ((a, b-tam), c)    -- que estavam acima da fileira que foi limpa.
                                            else ((a, b), c)



-- | Sistema de pontuação do jogo baseada na quantidade de fileiras completas de uma vez.
atualizaPont :: Int -> Int
atualizaPont 0 = 0
atualizaPont 1 = 100
atualizaPont 2 = 250
atualizaPont 3 = 500
atualizaPont 4 = 1000
atualizaPont _ = 0


main :: IO ()
main = do
  ps <- listaPecas
  play
    janela
    black
    10
    (mundoInicial ps)
    desenhaMundo
    joga
    atualizaMundo
    where
      janela = InWindow "Tetris" (820, 580) (50, 50)

