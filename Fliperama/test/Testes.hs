module Main where

import Tipos 
import Sudoku.Validacao 
import JogoDaVelha.Jogada
import JogoDaVelha.Resultados

import Test.QuickCheck

{-- //**********************************************\\
    || QuickCheck: Testes baseados em propriedades   ||
    \\**********************************************//
--}

{-- Mock e geração de dados --}
mockSudoku :: Sudoku
mockSudoku = [[5,3,0, 0,7,0, 0,0,0],
              [6,0,0, 1,9,5, 0,0,0],
              [0,9,8, 0,0,0, 0,6,0],
              [8,0,0, 0,6,0, 0,0,3],
              [4,0,0, 8,0,3, 0,0,1],
              [7,0,0, 0,2,0, 0,0,6],              
              [0,6,0, 0,0,0, 2,8,0],
              [0,0,0, 4,1,9, 0,0,5],
              [0,0,0, 0,8,0, 0,7,9]]

gerarJogada :: Gen Jogada
gerarJogada = (,,) <$> choose (1, 9) <*> choose (1, 9) <*> choose (0, 9)

gerarCoordDentroDosLimites :: Gen Coord
gerarCoordDentroDosLimites = do
    linha <- choose (0, tamanhoTabuleiro - 1)
    coluna <- choose (0, tamanhoTabuleiro - 1)
    return (linha, coluna)

gerarCoordForaDosLimites :: Gen Coord
gerarCoordForaDosLimites = do
    linha <- choose (-(tamanhoTabuleiro + 1), tamanhoTabuleiro * 2)
    coluna <- choose (-(tamanhoTabuleiro + 1), tamanhoTabuleiro * 2)
    return (linha, coluna)

gerarEntradaValida :: Gen String
gerarEntradaValida = do
    linha <- elements ["0", "1", "2"]
    coluna <- elements ["0", "1", "2"]
    return $ linha ++ " " ++ coluna

gerarEntradaInvalida :: Gen String
gerarEntradaInvalida = do
    linha <- elements ["3", "a", " "]
    coluna <- elements ["3", "a", " "]
    return $ linha ++ " " ++ coluna

gerarTabuleiro :: Gen Tabuleiro
gerarTabuleiro = vectorOf tamanhoTabuleiro (vectorOf tamanhoTabuleiro (elements [vazio, 'X', 'O']))

{-- TESTANDO SUDOKU - Validação de tabuleiro--}

prop_sudokuVazioRetornaFalse :: Bool
prop_sudokuVazioRetornaFalse = validTabuleiroCompleto (replicate 9 (replicate 9 0)) == False

prop_sudokuPreenchidoCorretamenteRetornaTrue :: Bool
prop_sudokuPreenchidoCorretamenteRetornaTrue = validTabuleiroCompleto [[1,2,3,4,5,6,7,8,9],
                                           [4,5,6,7,8,9,1,2,3],
                                           [7,8,9,1,2,3,4,5,6],
                                           [2,3,4,5,6,7,8,9,1],
                                           [5,6,7,8,9,1,2,3,4],
                                           [8,9,1,2,3,4,5,6,7],
                                           [3,4,5,6,7,8,9,1,2],
                                           [6,7,8,9,1,2,3,4,5],
                                           [9,1,2,3,4,5,6,7,8]]

prop_sudokuResolvidoEhValidoRetornaTrue :: Bool
prop_sudokuResolvidoEhValidoRetornaTrue =
    validTabuleiroCompleto [[1,2,3,4,5,6,7,8,9],
              [4,5,6,7,8,9,1,2,3],
              [7,8,9,1,2,3,4,5,6],
              [2,3,4,5,6,7,8,9,1],
              [5,6,7,8,9,1,2,3,4],
              [8,9,1,2,3,4,5,6,7],
              [3,4,5,6,7,8,9,1,2],
              [6,7,8,9,1,2,3,4,5],
              [9,1,2,3,4,5,6,7,8]]

prop_sudokuIncompletoNaoEhValidoRetornaFalse :: Bool
prop_sudokuIncompletoNaoEhValidoRetornaFalse =
    not $ validTabuleiroCompleto [[1,2,3,4,5,6,7,8,0],
                    [4,5,6,7,8,9,1,2,3],
                    [7,8,9,1,2,3,4,5,6],
                    [2,3,4,5,6,7,8,9,1],
                    [5,6,7,8,9,1,2,3,4],
                    [8,9,1,2,3,4,5,6,7],
                    [3,4,5,6,7,8,9,1,2],
                    [6,7,8,9,1,2,3,4,5],
                    [9,1,2,3,4,5,6,7,8]]

{-- Validação de Entrada --}

testaCom20Jogadas :: Sudoku -> [Jogada] -> Bool
testaCom20Jogadas tabuleiro jogadas =
    all (prop_validarEntradasAleatorias tabuleiro) jogadas
    
prop_validarEntradasAleatorias :: Sudoku -> Jogada -> Bool
prop_validarEntradasAleatorias tabuleiro (line, col, num) =
    validarEntradaSudoku tabuleiro (line, col, num) == esperaResultado (line, col, num)
  where
    esperaResultado (line, col, num)
        | num < 1 || num > 9 = False
        | tabuleiro !! (line - 1) !! (col - 1) /= 0 = False
        | not (validarJogada tabuleiro num (line - 1, col - 1)) = False
        | otherwise = True

prop_validaEntradaSudoku :: Property
prop_validaEntradaSudoku =
    forAll (vectorOf 20 gerarJogada) $ \jogadas ->
        property $ testaCom20Jogadas mockSudoku jogadas

-- TESTANDO JOGO DA VELHA

prop_ganhouLinha :: Bool
prop_ganhouLinha = ganhou [['X', 'X', 'X'],[' ', ' ', ' '],[' ', ' ', ' ']] 'X'

prop_ganhouColuna :: Bool
prop_ganhouColuna = ganhou [['O', ' ', ' '],['O', ' ', ' '],['O', ' ', ' ']] 'O'

prop_ganhouDiagonalPrincipal :: Bool
prop_ganhouDiagonalPrincipal = ganhou [['X', ' ', ' '],[' ', 'X', ' '],[' ', ' ', 'X']] 'X'

prop_ganhouDiagonalSecundaria :: Bool
prop_ganhouDiagonalSecundaria = ganhou [[' ', ' ', 'O'],[' ', 'O', ' '],['O', ' ', ' ']] 'O'

prop_naoGanhouTabuleiroVazio :: Bool
prop_naoGanhouTabuleiroVazio = not $ ganhou [[' ', ' ', ' '],[' ', ' ', ' '],[' ', ' ', ' ']] 'X'

prop_naoGanhouTabuleiroIncompleto :: Bool
prop_naoGanhouTabuleiroIncompleto = not $ ganhou [['X', 'O', 'X'],['O', 'X', ' '],['O', 'X', 'O']] 'X'

prop_testarValidacaoDeEntradaJogoDaVelha :: Property
prop_testarValidacaoDeEntradaJogoDaVelha = conjoin
    [ 
      forAll gerarCoordDentroDosLimites $ \coord ->
        dentroDosLimites coord === True,
        
      forAll (choose (tamanhoTabuleiro, tamanhoTabuleiro * 2)) $ \linha ->
        forAll (choose (tamanhoTabuleiro, tamanhoTabuleiro * 2)) $ \coluna ->
          dentroDosLimites (linha, coluna) === False,

      forAll (elements ["0 0", "1 1", "2 2"]) $ \entrada ->
        validarEntrada entrada === True,

      forAll (elements ["3 3", "a b", "1"]) $ \entrada ->
        validarEntrada entrada === False,

      forAll gerarTabuleiro $ \tabuleiro ->
        forAll gerarCoordDentroDosLimites $ \coord ->
            let (linha, coluna) = coord
                cell = tabuleiro !! linha !! coluna
                expected = (cell == vazio)
            in posicaoDisponivel coord tabuleiro === expected
    ]

main :: IO ()
main = do
    quickCheck prop_sudokuVazioRetornaFalse
    quickCheck prop_sudokuPreenchidoCorretamenteRetornaTrue
    quickCheck prop_sudokuResolvidoEhValidoRetornaTrue
    quickCheck prop_sudokuIncompletoNaoEhValidoRetornaFalse
    quickCheck prop_validaEntradaSudoku
    quickCheck prop_ganhouLinha
    quickCheck prop_ganhouColuna
    quickCheck prop_ganhouDiagonalPrincipal
    quickCheck prop_ganhouDiagonalSecundaria
    quickCheck prop_naoGanhouTabuleiroVazio
    quickCheck prop_naoGanhouTabuleiroIncompleto
    quickCheck prop_testarValidacaoDeEntradaJogoDaVelha