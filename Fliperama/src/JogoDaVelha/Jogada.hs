module JogoDaVelha.Jogada where

import JogoDaVelha.Resultados (ganhou)

import Tipos ( Coord, Tabuleiro, vazio, tamanhoTabuleiro, jogadorX, jogadorO )
import Data.List (intercalate)
import System.Random (randomRIO)

{- --------------------------------------\
    Manipulação e retorno de tabuleiro
\-----------------------------------------}

tabuleiroInicial :: Tabuleiro
tabuleiroInicial = replicate tamanhoTabuleiro (replicate tamanhoTabuleiro vazio)

imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro tabuleiro = do
    putStrLn "Estado atual do tabuleiro:"
    putStrLn $ unlines (map (foldr (\c acc -> if null acc then [c] else c : " | " ++ acc) "") tabuleiro)

{- -----------------------------------------------------------------\
    Funções para obter e savlar jogada no tabuleiro
\--------------------------------------------------------------------}

-- Função para obter a jogada do usuário - looping até ter uma jogada válida do usuário
obterJogadaValida :: Tabuleiro -> IO Coord
obterJogadaValida tabuleiro = do
    putStrLn "Digite a linha (0-2) e a coluna (0-2) para sua jogada, separados por espaço:"
    input <- getLine
    if validarEntrada input
        then do
            let [linhaStr, colunaStr] = words input
            let jogada = (read linhaStr, read colunaStr)
            if dentroDosLimites jogada && posicaoDisponivel jogada tabuleiro
                then return jogada
                else putStrLn "Essa posição já está ocupada ou fora dos limites. Por favor, escolha outra." >> obterJogadaValida tabuleiro
        else do
            putStrLn "Entrada inválida. Por favor, digite dois números entre 0 e 2 separados por um espaço."
            obterJogadaValida tabuleiro

jogadaComputador :: Tabuleiro -> IO Coord
jogadaComputador tabuleiro = do
    let posicoesVazias = [(linha, coluna) | linha <- [0..2], coluna <- [0..2], tabuleiro !! linha !! coluna == vazio]
    let n = length posicoesVazias
    indice <- randomRIO (0, n - 1)
    return $ posicoesVazias !! fromIntegral indice

jogadaInteligenteComputador :: Tabuleiro -> Char -> IO Coord
jogadaInteligenteComputador tabuleiro jogadorComputador = do
    let possiveisJogadas = [(linha, coluna) | linha <- [0..2], coluna <- [0..2], tabuleiro !! linha !! coluna == vazio]
        jogadaVitoria = headMay $ filter (ganhouSeFizer tabuleiro jogadorComputador) possiveisJogadas
        jogadaBloqueio = headMay $ filter (ganhouSeFizer tabuleiro (if jogadorComputador == jogadorX then jogadorO else jogadorX)) possiveisJogadas
        jogadaAleatoria = if null jogadaVitoria && null jogadaBloqueio
                            then headMay possiveisJogadas
                            else Nothing
        headMay :: [a] -> Maybe a
        headMay (x:_) = Just x
        headMay [] = Nothing
        randomJogada :: IO Coord
        randomJogada = do
            let n = length possiveisJogadas
            if n == 0
                then error "Sem jogadas válidas disponíveis"
                else do
                    indice <- randomRIO (0, n - 1)
                    return $ possiveisJogadas !! fromIntegral indice
    case jogadaVitoria of
        Just jogada -> return jogada
        Nothing -> case jogadaBloqueio of
            Just jogada -> return jogada
            Nothing -> case jogadaAleatoria of
                Just jogada -> return jogada
                Nothing -> randomJogada


-- Função para "salvar" a jogada no tabuleiro
-- ***
-- Função "fazFOgada" e "atualizaLista" foram feitas com auxilio de chatGPT, 
-- Primeiramente eu desenvolvi a funcao fazJogada logo no começo do projeto, mas a função estavava muito
-- grande, e numa tentativa de melhorar o código pedi para o chat GPT que basicamente dividiu a função original em
-- 2, simplificando algumas coisas 
-- ***
fazJogada :: Coord -> Tabuleiro -> Char -> Tabuleiro
fazJogada (linha, coluna) tabuleiro jogador
    | tabuleiro !! linha !! coluna == vazio = 
        atualizarLista linha (atualizarLista coluna (const jogador)) tabuleiro
    | otherwise = tabuleiro

atualizarLista :: Int -> (a -> a) -> [a] -> [a]
atualizarLista n f xs =
    take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

{- -----------------------------------------------------------------\
    Funções para validar jogada e formato da entrada
\--------------------------------------------------------------------}

-- Função para verificar se a jogada está dentro do tabuleiro (x e y entre 0 e 2)
dentroDosLimites :: Coord -> Bool
dentroDosLimites (linha, coluna) =
    (linha >= 0 && linha < tamanhoTabuleiro) && (coluna >= 0 && coluna < tamanhoTabuleiro)

validarEntrada :: String -> Bool
validarEntrada input =
    let partes = words input
    in length partes == 2 && all (\parte -> length parte == 1 && (head parte `elem` ['0'..'2'])) partes

posicaoDisponivel :: Coord -> Tabuleiro -> Bool
posicaoDisponivel (linha, coluna) tabuleiro =
    tabuleiro !! linha !! coluna == vazio

ganhouSeFizer :: Tabuleiro -> Char -> Coord -> Bool
ganhouSeFizer tabuleiro jogador jogada =
    let novoTabuleiro = fazJogada jogada tabuleiro jogador
    in ganhou novoTabuleiro jogador