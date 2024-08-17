module Main (main) where

import Sudoku.Main (mainMenu)
import JogoDaVelha.Main (menuInicial)

menu :: IO ()
menu = do
  putStrLn " "
  putStrLn "Escolha o número do jogo: "
  putStrLn "1 - Jogo da Velha"
  putStrLn "2 - Sudoku "
  putStrLn "3 - Sair "
  option <- getLine
  case option of
      "1" -> menuInicial -- Menu Inicial do Jogo da Velha
      "2" -> mainMenu -- Menu Inicial do Sudoku
      "3" -> putStrLn "..." 
      _   -> do
          putStrLn "Opção inválida. Tente novamente."
          menu 

main :: IO ()
main = do
  putStrLn "-- Fliperama --"
  menu