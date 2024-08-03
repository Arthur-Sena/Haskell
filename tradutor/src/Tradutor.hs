module Tradutor where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Char (toLower, isAlpha)
import Dicionario

-- Função para detectar a língua da frase
detectorDeIdioma :: String -> Language
detectorDeIdioma sentence
    | any (`elem` wordsLower) englishWords = English
    | any (`elem` wordsLower) spanishWords = Spanish
    | any (`elem` wordsLower) frenchWords  = French
    | otherwise                            = Unknown
  where
    wordsLower = map (map toLower) (words sentence)
    englishWords = Map.keys englishDict
    spanishWords = Map.keys spanishDict
    frenchWords  = Map.keys frenchDict

dicionarioInvertido :: Dictionary -> Dictionary
dicionarioInvertido dict = Map.fromList [(v, k) | (k, v) <- Map.toList dict]

traduzirPalavra :: Language -> Language -> String -> String
traduzirPalavra idiomaOrigem idiomaDestino word =
    let palavraLimpa = removerPontuacao word
        palavraEmPortugues = case idiomaOrigem of
            English    -> fromMaybe palavraLimpa (Map.lookup (map toLower palavraLimpa) englishDict)
            Spanish    -> fromMaybe palavraLimpa (Map.lookup (map toLower palavraLimpa) spanishDict)
            French     -> fromMaybe palavraLimpa (Map.lookup (map toLower palavraLimpa) frenchDict)
            Portuguese -> fromMaybe palavraLimpa (Map.lookup (map toLower palavraLimpa) (dicionarioInvertido englishDict))
            _          -> palavraLimpa 

        traduzida = case idiomaDestino of
            English    -> fromMaybe palavraEmPortugues (Map.lookup (map toLower palavraEmPortugues) (dicionarioInvertido englishDict))
            Spanish    -> fromMaybe palavraEmPortugues (Map.lookup (map toLower palavraEmPortugues) (dicionarioInvertido spanishDict))
            French     -> fromMaybe palavraEmPortugues (Map.lookup (map toLower palavraEmPortugues) (dicionarioInvertido frenchDict))
            Portuguese -> palavraEmPortugues
            _          -> palavraEmPortugues
    in traduzida

--corrigirGramatica :: Language -> [String] -> [String]
--corrigirGramatica _ [] = []
--corrigirGramatica idioma (x:xs) = corrigirMaiusculas (x:xs)
--  where
--    -- Capitaliza a primeira palavra da frase
--    corrigirMaiusculas [] = []
--    corrigirMaiusculas (w:ws) = (toUpper (head w) : tail w) : corrigirPronomes ws
--
--    -- Corrige a concordância de pronomes
--    corrigirPronomes [] = []
--    corrigirPronomes (w:ws)
--      | idioma == Portuguese = case w of
--          "eu" -> "Eu" : corrigirPronomes ws
--          "você" -> "Você" : corrigirPronomes ws
--          _ -> w : corrigirPronomes ws
--      | otherwise = w : corrigirPronomes ws

traduzirTexto :: Language -> String -> String
traduzirTexto idioma texto =
    let palavrasDoTexto = words texto
        palavrasLimpa = map removerPontuacao palavrasDoTexto
        palavraTraduzida = map (traduzirPalavra idioma Portuguese) palavrasLimpa
        --palavrasCorrigidas = corrigirGramatica palavraTraduzida
    in intercalate " " palavraTraduzida

traduzirIdiomaEscolhido :: Language -> Language -> String -> String
traduzirIdiomaEscolhido idiomaOrigem idiomaDestino texto =
    let palavrasDoTexto = words texto
        palavrasLimpa = map removerPontuacao palavrasDoTexto
        palavraTraduzida = map (traduzirPalavra idiomaOrigem idiomaDestino) palavrasLimpa
        --palavrasCorrigidas = corrigirGramatica palavraTraduzida
    in intercalate " " palavraTraduzida

-- Função para remover pontuação de uma palavra
removerPontuacao :: String -> String
removerPontuacao = filter isAlpha

parseLanguage :: String -> Language
parseLanguage lang = case lang of
    "en" -> English
    "es" -> Spanish
    "fr" -> French
    "pt" -> Portuguese
    _    -> Unknown