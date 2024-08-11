module Sudoku where

import Validacao (validarJogada)
import Tipos 
import System.Random (randomRIO)


geradorTabuleiroVazio :: Sudoku
geradorTabuleiroVazio = replicate 9 (replicate 9 0)

sudokuGenerator :: Int -> IO Sudoku
sudokuGenerator qntCasasPreenchidas = do
    Just fullBoard <- preencheTabuleiro geradorTabuleiroVazio
    limpaPosicao fullBoard (81 - qntCasasPreenchidas)
  where
    limpaPosicao tabuleiro 0 = return tabuleiro
    limpaPosicao tabuleiro n = do
        linha <- randomRIO (0, 8)
        col <- randomRIO (0, 8)
        if tabuleiro !! linha !! col == 0
            then limpaPosicao tabuleiro n
            else limpaPosicao (copySudokuEspacoSemValor tabuleiro linha col 0) (n - 1)
    copySudokuEspacoSemValor tabuleiro linha col val =
        take linha tabuleiro ++ 
        [take col (tabuleiro !! linha) 
        ++ [val] 
        ++ drop (col + 1) (tabuleiro !! linha)
        ] ++ drop (linha + 1) tabuleiro

preencheTabuleiro :: Sudoku -> IO (Maybe Sudoku)
preencheTabuleiro tabuleiro = preencheTabuleiro' tabuleiro (0, 0)
  where
    preencheTabuleiro' tabuleiro (9, _) = return (Just tabuleiro)
    preencheTabuleiro' tabuleiro (linha, 9) = preencheTabuleiro' tabuleiro (linha + 1, 0)
    preencheTabuleiro' tabuleiro (linha, col) = do
      nums <- randomRIO (1, 9) >>= \n -> return $ [n] ++ [1..9] ++ [1..9]
      let possibilidades = filter (\n -> validarJogada tabuleiro n (linha, col)) nums
      tentativa possibilidades
      where
        tentativa [] = return Nothing
        tentativa (x:xs) = do
          let newTabuleiro = take linha tabuleiro ++ [take col (tabuleiro !! linha) ++ [x] ++ drop (col + 1) (tabuleiro !! linha)] ++ drop (linha + 1) tabuleiro
          msudoku <- preencheTabuleiro' newTabuleiro (linha, col + 1)
          case msudoku of
            Just b  -> return (Just b)
            Nothing -> tentativa xs

preenchendoTabuleiro' :: Sudoku -> Coord -> Maybe Sudoku
preenchendoTabuleiro' tabuleiro (9, _) = Just tabuleiro
preenchendoTabuleiro' tabuleiro (line, 9) = preenchendoTabuleiro' tabuleiro (line + 1, 0)
preenchendoTabuleiro' tabuleiro (line, col)
    | tabuleiro !! line !! col /= 0 = preenchendoTabuleiro' tabuleiro (line, col + 1)
    | otherwise = tryNum [1..9]
  where
    tryNum [] = Nothing
    tryNum (x:xs)
        | validarJogada tabuleiro x (line, col) =
            case preenchendoTabuleiro' (salvarJogada tabuleiro (line, col, x)) (line, col + 1) of
                Just b -> Just b
                Nothing -> tryNum xs
        | otherwise = tryNum xs

buscaPosicaoVazia :: Sudoku -> Maybe Coord
buscaPosicaoVazia sudoku = findIndex' sudoku 0 0
  where
    findIndex' [] _ _ = Nothing
    findIndex' (x:xs) line col
      | col >= 9 = findIndex' xs (line + 1) 0
      | (x !! col) == 0 = Just (line, col)
      | otherwise = findIndex' (x:xs) line (col + 1)

salvarJogada :: Sudoku -> Jogada -> Sudoku
salvarJogada tabuleiro (line, col, val) =
    take line tabuleiro 
    ++ [
        take col (tabuleiro !! line) 
        ++ [val] 
        ++ drop (col + 1) (tabuleiro !! line)
        ] ++ drop (line + 1) tabuleiro


jogadaComputador :: Sudoku -> IO Sudoku
jogadaComputador sudoku = do
    let Just (line, col) = buscaPosicaoVazia sudoku
    let numValido = [n | n <- [1..9], validarJogada sudoku n (line, col)]
    let num = head numValido -- Pega o primeiro número válido
    return $ salvarJogada sudoku (line, col, num)

solucionarSudoku :: Sudoku -> IO Sudoku
solucionarSudoku sudoku = do
    let mSudoku = preenchendoTabuleiro' sudoku (0, 0)
    case mSudoku of
        Just completo -> return completo
        Nothing -> error "Erro ao tentar completar o tabuleiro"

showTabuleiro :: Sudoku -> IO ()
showTabuleiro tabuleiro = do
    putStrLn "    1 2 3 | 4 5 6 | 7 8 9"
    putStrLn "   -----------------------"
    mapM_ putStrLn $ formatarSudoku 1 tabuleiro
  where
    formatarSudoku _ [] = []
    formatarSudoku n (x:xs) =
        (show n ++ " " ++ concatMap separadorQuadrante (zip [1..] x)) : separadorDeLinha ++ formatarSudoku (n + 1) xs
      where
        separadorDeLinha = if n `mod` 3 == 0 && not (null xs) then ["   ---------------------"] else []
        separadorQuadrante (i, x') = (if (i-1) `mod` 3 == 0 then "| " else "") ++ (if x' == 0 then ". " else show x' ++ " ")
