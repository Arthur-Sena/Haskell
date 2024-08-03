module Main where

import Tradutor
import System.IO (hFlush, stdout)

main :: IO ()
main = menu

menu :: IO ()
menu = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Detectar idioma e traduzir para português"
    putStrLn "2. Escolher idiomas para tradução"
    putStrLn "3. Sair"
    putStr "Opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> traduzirIdiomaDesconhecido
        "2" -> tradutorDeTexto
        "3" -> putStrLn "FIM"
        _   -> putStrLn "Opção inválida" >> menu

traduzirIdiomaDesconhecido :: IO ()
traduzirIdiomaDesconhecido = do
    putStrLn "Digite o texto:"
    input <- getLine
    let idioma = detectorDeIdioma input
    let traducao = traduzirTexto idioma input
    putStrLn $ "Idioma detectado: " ++ show idioma
    putStrLn $ "Tradução: " ++ traducao
    voltarAoMenu traduzirIdiomaDesconhecido

tradutorDeTexto :: IO ()
tradutorDeTexto = do
    putStrLn "Escolha o idioma de origem (en, es, fr, pt):"
    origem <- getLine
    putStrLn "Escolha o idioma de destino (en, es, fr, pt):"
    destino <- getLine
    putStrLn "Digite o texto:"
    input <- getLine
    let idiomaOrigem = parseLanguage origem
    let idiomaDestino = parseLanguage destino
    let traducao = traduzirIdiomaEscolhido idiomaOrigem idiomaDestino input
    putStrLn $ "Tradução: " ++ traducao
    voltarAoMenu tradutorDeTexto

voltarAoMenu :: IO () -> IO ()
voltarAoMenu funcaoAnterior = do
    putStrLn ""
    putStrLn "Deseja voltar ao menu inicial? (s/n)"
    resposta <- getLine
    if resposta == "s"
        then menu
        else funcaoAnterior