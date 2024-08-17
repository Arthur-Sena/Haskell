module Sudoku.Main where

import Tipos 
import Sudoku.Sudoku 
import Sudoku.Validacao (validEntrada, validTabuleiroCompleto)

{--
    Nível de Díficuldade: o nível de dificuldade varia
    de acordo com a quantidade de casas que começam preenchidas
    no inicio do jogo, por isso a funcao startSUdoku vem acompanhada
    de um inteiro (qntCasasPreenchidasInicialmente)
--}

mainMenu :: IO ()
mainMenu = do
    putStrLn " "
    putStrLn "Escolha uma opção:"
    putStrLn "1. Jogo Fácil"
    putStrLn "2. Jogo Médio"
    putStrLn "3. Jogo Difícil"
    putStrLn "4. Encerrar"
    option <- getLine
    case option of
        "1" -> startSudoku 72 -- Jogo Fácil (85% preenchido)
        "2" -> startSudoku 40 -- Jogo Médio (50% preenchido)
        "3" -> startSudoku 20 -- Jogo Difícil
        "4" -> putStrLn "Jogo encerrado."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            mainMenu

--USEI ESSE MOCK SÓ PRA TESTAR O SOLUCIONADOR DE SUDOKU
--CASO QUEIRA TESTAR TAMBEM É SÓ COLOCAR OS NÚMEROS AI E INICIAR O JOGO NORMALMENTE
--EM QUALQUER NIVEL DE DIFICULDADE, DEPOIS DIGITA 10 0 0 PARA VER O "GABARITO"
sudokuMock :: Sudoku 
sudokuMock = [[0,0,0,0,0,0,0,0,0], --1
              [0,0,0,0,0,0,0,0,0], --2
              [0,0,0,0,0,0,0,0,0], --3
              [0,0,0,0,0,0,0,0,0], --4
              [0,0,0,0,0,0,0,0,0], --5
              [0,0,0,0,0,0,0,0,0], --6
              [0,0,0,0,0,0,0,0,0], --7
              [0,0,0,0,0,0,0,0,0], --8
              [0,0,0,0,0,0,0,0,0]] --9
            -- 1 2 3 4 5 6 7 8 9


startSudoku :: Int -> IO ()
startSudoku qntCasasPreenchidasInicialmente = do
    tabuleiro <- sudokuGenerator qntCasasPreenchidasInicialmente
    sudokuLoop tabuleiro
    --    sudokuLoop sudokuMock

sudokuLoop :: Sudoku -> IO ()
sudokuLoop tabuleiro = do
    showTabuleiro tabuleiro
    if validTabuleiroCompleto tabuleiro
        then putStrLn "Parabéns! Você resolveu o Sudoku!"
        else do
            putStrLn " "
            putStrLn "Digite sua jogada no formato (linha coluna número)"
            putStrLn "Dica: digite 0 0 0 para que o computador faça um jogada"
            putStrLn ", 0 0 1 para sair ou 10 0 0 para revelar a solução:"
            jogada <- getLine
            let (x:y:v:_) = map read (words jogada)
            if x == 0 && y == 0 && v == 0
                then do
                    putStrLn "Computador fazendo uma jogada..."
                    newSudoku <- jogadaComputador tabuleiro
                    sudokuLoop newSudoku
                else if x == 0 && y == 0 && v == 1
                    then putStrLn "Jogo encerrado."
                    else if x == 10
                        then do
                            putStrLn "Revelando solução..."
                            resolucao <- solucionarSudoku tabuleiro
                            case resolucao of
                                Just filledBoard -> showTabuleiro filledBoard
                                Nothing -> putStrLn "Não foi possível revelar a solução."
                            mainMenu
                        else do
                            valid <- validEntrada tabuleiro (x, y, v)
                            if valid
                                then do
                                    let newSudoku = salvarJogada tabuleiro (x-1, y-1, v)
                                    sudokuLoop newSudoku
                                else sudokuLoop tabuleiro

--main :: IO ()
--main = do
--    putStrLn "Bem-vindo ao Sudoku!" 
--    mainMenu

--Melhoria: Limitar o número de ajuda (Quando o pc joga muito acaba chegando num ponto q nao tem solução)