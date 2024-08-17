module Tipos where


type Tabuleiro = [[Char]] --Tabuleiro Jogo Da Velha
type Sudoku = [[Int]]
type Coord = (Int, Int)
type Jogada = (Int, Int, Int) --Linha, Coluna, Valor

vazio, jogadorX, jogadorO :: Char
vazio = ' '
jogadorX = 'X'
jogadorO = 'O'

tamanhoTabuleiro :: Int
tamanhoTabuleiro = 3

newtype SudokuBoard = SudokuBoard { getBoard :: Sudoku }
    deriving (Show)

instance Semigroup SudokuBoard where
    (SudokuBoard b1) <> (SudokuBoard b2) = SudokuBoard (joinTabuleiro b1 b2)

instance Monoid SudokuBoard where
    mempty = SudokuBoard (replicate 9 (replicate 9 0))

joinTabuleiro :: Sudoku -> Sudoku -> Sudoku
joinTabuleiro b1 b2 = [[if x == 0 then y else x | (x, y) <- zip line1 line2] | (line1, line2) <- zip b1 b2]