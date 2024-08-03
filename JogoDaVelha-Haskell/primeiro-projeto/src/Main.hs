module Main where

import Resultados ( ganhou, empatou )
import Tipos ( jogadorO, jogadorX, Tabuleiro )
import Jogada

{----------------------------------\
    Menus de Inicio e Fim
\----------------------------------}

-- Menu inicial para o jogo da velha
menuInicial :: IO ()
menuInicial = do
    putStrLn "Bem-vindo ao Jogo da Velha!"
    putStrLn "Por favor, escolha uma opção: (1, 2 ou 3)"
    putStrLn "1 - Jogar contra o computador"
    putStrLn "2 - Jogar com um amigo"
    putStrLn "3 - Sair"
    escolha <- getLine
    case escolha of
        "1" -> nivelDificuldade
        "2" -> jogoComAmigo
        "3" -> putStrLn "Fim!"
        _   -> do
            putStrLn "Opção inválida. Por favor, escolha '1', '2' ou '3'."
            menuInicial

-- Menu após um jogo
menuFinal :: IO ()
menuFinal = do
    putStrLn "Por favor, escolha uma opção: (1, 2 ou 3)"
    putStrLn "1 - Jogar contra o computador"
    putStrLn "2 - Jogar com um amigo"
    putStrLn "3 - Sair"
    escolha <- getLine
    case escolha of
        "1" -> nivelDificuldade
        "2" -> jogoComAmigo
        "3" -> putStrLn "Fim!"
        _   -> do
            putStrLn "Opção inválida. Por favor, escolha '1', '2' ou '3'."
            menuFinal

--Escolha de dificuldade
nivelDificuldade :: IO ()
nivelDificuldade = do
    putStrLn "Por favor, escolha o nível de dificuldade: (1, 2 ou 3)"
    putStrLn "1 - Fácil"
    putStrLn "2 - Difícil"
    putStrLn "3 - Profissional"
    escolha <- getLine
    case escolha of
        "1" -> jogoContraComputadorFacil
        "2" -> jogoContraComputadorDificil
        "3" -> jogoContraComputadorProfissional
        _   -> do
            putStrLn "Opção inválida. Por favor, escolha '1', '2' ou '3'."
            nivelDificuldade

{-------------------------------------------------------------------------------------}

jogoComAmigo :: IO ()
jogoComAmigo = do
    putStrLn "Você escolheu jogar com um amigo."
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loop tabuleiro jogadorX
    putStrLn $ "  "
    menuFinal
  where
    loop :: Tabuleiro -> Char -> IO ()
    loop tabuleiro jogadorAtual = do
        putStrLn $ "É a vez do jogador " ++ [jogadorAtual]
        jogada <- obterJogadaValida tabuleiro
        let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
        imprimeTabuleiro novoTabuleiro
        if ganhou novoTabuleiro jogadorAtual
            then 
              putStrLn $ "O jogador " ++ [jogadorAtual] ++ " ganhou!"
            else if empatou novoTabuleiro
                then putStrLn "O jogo empatou!"
                else loop novoTabuleiro (if jogadorAtual == jogadorX then jogadorO else jogadorX)
        
jogoContraComputadorFacil :: IO ()
jogoContraComputadorFacil = do
    putStrLn "Você escolheu jogar contra o computador. (Fácil)"
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loop tabuleiro jogadorX
    menuFinal
  where
    loop :: Tabuleiro -> Char -> IO ()
    loop tabuleiro jogadorAtual = do
        if jogadorAtual == jogadorX
            then do
                putStrLn $ "É a vez do jogador " ++ [jogadorAtual]
                jogada <- obterJogadaValida tabuleiro
                let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
                imprimeTabuleiro novoTabuleiro
                if ganhou novoTabuleiro jogadorAtual
                    then putStrLn $ "O jogador " ++ [jogadorAtual] ++ " ganhou!"
                    else if empatou novoTabuleiro
                        then putStrLn "O jogo empatou!"
                        else loop novoTabuleiro jogadorO
            else do
                putStrLn "É a vez do computador."
                jogada <- jogadaComputador tabuleiro
                let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
                imprimeTabuleiro novoTabuleiro
                if ganhou novoTabuleiro jogadorAtual
                    then putStrLn $ "O computador ganhou!"
                    else if empatou novoTabuleiro
                        then putStrLn "O jogo empatou!"
                        else loop novoTabuleiro jogadorX

jogoContraComputadorDificil :: IO ()
jogoContraComputadorDificil = do
    putStrLn "Você escolheu jogar contra o computador. (Difícil)"
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loop tabuleiro jogadorX
    menuFinal
  where
    loop :: Tabuleiro -> Char -> IO ()
    loop tabuleiro jogadorAtual = do
        putStrLn $ "É a vez do jogador " ++ [jogadorAtual]
        if jogadorAtual == jogadorX
            then do
                jogada <- obterJogadaValida tabuleiro
                let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
                imprimeTabuleiro novoTabuleiro
                if ganhou novoTabuleiro jogadorAtual
                    then putStrLn $ "O jogador " ++ [jogadorAtual] ++ " ganhou!"
                    else if empatou novoTabuleiro
                        then putStrLn "O jogo empatou!"
                        else loop novoTabuleiro (if jogadorAtual == jogadorX then jogadorO else jogadorX)
            else do
                putStrLn "É a vez do computador."
                jogada <- jogadaInteligenteComputador tabuleiro jogadorAtual
                let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
                imprimeTabuleiro novoTabuleiro
                if ganhou novoTabuleiro jogadorAtual
                    then putStrLn $ "O computador ganhou!"
                    else if empatou novoTabuleiro
                        then putStrLn "O jogo empatou!"
                        else loop novoTabuleiro (if jogadorAtual == jogadorX then jogadorO else jogadorX)

jogoContraComputadorProfissional :: IO ()
jogoContraComputadorProfissional = do
    putStrLn "Você escolheu jogar contra o computador. (Profissional)"
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loop tabuleiro jogadorO  -- Computador começa jogando
    menuFinal
  where
    loop :: Tabuleiro -> Char -> IO ()
    loop tabuleiro jogadorAtual = do
        if jogadorAtual == jogadorX
            then do
                putStrLn $ "É a vez do jogador " ++ [jogadorAtual]
                jogada <- obterJogadaValida tabuleiro
                let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
                imprimeTabuleiro novoTabuleiro
                if ganhou novoTabuleiro jogadorAtual
                    then putStrLn $ "O jogador " ++ [jogadorAtual] ++ " ganhou!"
                    else if empatou novoTabuleiro
                        then putStrLn "O jogo empatou!"
                        else loop novoTabuleiro jogadorO
            else do
                putStrLn "É a vez do computador."
                jogada <- jogadaInteligenteComputador tabuleiro jogadorAtual
                let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
                imprimeTabuleiro novoTabuleiro
                if ganhou novoTabuleiro jogadorAtual
                    then putStrLn $ "O computador ganhou!"
                    else if empatou novoTabuleiro
                        then putStrLn "O jogo empatou!"
                        else loop novoTabuleiro jogadorX


-- Função para iniciar o jogo
main :: IO ()
main = menuInicial