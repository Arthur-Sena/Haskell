module Validacao where

import Tipos 

{-- *****************************************************
    | Função pra validar se a entrada é válida:         |
    |     - Deve ser formada por 3 Inteiros             |
    |     - Os números deve ser >= 0 e <= 10            |
    |     * 0 apenas para casos especias                |
    |       (sair ou obter dica do computador)          |
    |                                                   |        
    | Função pra validar se a jogada é válida:          |
    |     - Posição escolhida não pode estar preenchida |
    |     - Não pode ter o valor da jogada na linha     |
    |     - Não pode ter o valor da jogada na coluna    |
    |     - Não pode ter o valor da jogada no quadrante |
 -- *****************************************************--}

validEntrada :: Sudoku -> Jogada -> IO Bool
validEntrada tabuleiro (line, col, num)
    | num < 1 || num > 9 = do
        putStrLn "Número inválido. Deve ser entre 1 e 9."
        putStrLn " "
        return False
    | tabuleiro !! (line - 1) !! (col - 1) /= 0 = do
        putStrLn "Posição já preenchida."
        putStrLn " "
        return False
    | not (validarJogada tabuleiro num (line - 1, col - 1)) = do
        putStrLn "Jogada errada segundo as regras do Sudoku."
        putStrLn " "
        return False
    | otherwise = return True

--validarJogada :: Sudoku -> Int -> Coord -> Bool
--validarJogada tabuleiro num (linha, col) =
--    notElem num (tabuleiro !! linha) &&
--    notElem num [tabuleiro !! x !! col | x <- [0..8]] &&
--    notElem num [tabuleiro !! x !! y | x <- boxLinhas, y <- boxColunas]
--  where
--    boxInicio x = 3 * (x `div` 3)
--    boxLinhas = [boxInicio linha .. boxInicio linha + 2]
--    boxColunas = [boxInicio col .. boxInicio col + 2]

validarJogada :: Sudoku -> Int -> Coord -> Bool
validarJogada board num (row, col) =
    notElem num (board !! row) &&
    notElem num [board !! r !! col | r <- [0..8]] &&
    notElem num [board !! r !! c | r <- boxRows, c <- boxCols]
  where
    boxStart x = 3 * (x `div` 3)
    boxRows = [boxStart row .. boxStart row + 2]
    boxCols = [boxStart col .. boxStart col + 2]

validTabuleiroCompleto :: Sudoku -> Bool
validTabuleiroCompleto sudoku = all (/= 0) (concat sudoku)