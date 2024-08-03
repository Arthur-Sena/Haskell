module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Char (toLower)

-- Tipos para o dicionário e a língua
type Dictionary = Map String String
data Language = English | Spanish | French | Unknown deriving (Show, Eq)

-- Dicionários para cada língua
englishDict, spanishDict, frenchDict :: Dictionary
englishDict = Map.fromList [("hello", "olá"), ("world", "mundo"), ("how", "como"), ("are", "está"), ("you", "você")]
spanishDict = Map.fromList [("hola", "olá"), ("mundo", "mundo"), ("cómo", "como"), ("estás", "está"), ("tú", "você")]
frenchDict  = Map.fromList [("bonjour", "olá"), ("monde", "mundo"), ("comment", "como"), ("ça", "está"), ("vous", "você")]

-- Função para detectar a língua da frase
detectLanguage :: String -> Language
detectLanguage sentence
    | any (`elem` wordsLower) englishWords = English
    | any (`elem` wordsLower) spanishWords = Spanish
    | any (`elem` wordsLower) frenchWords  = French
    | otherwise                            = Unknown
  where
    wordsLower = map (map toLower) (words sentence)
    englishWords = Map.keys englishDict
    spanishWords = Map.keys spanishDict
    frenchWords  = Map.keys frenchDict

-- Função para traduzir uma palavra usando o dicionário apropriado
translateWord :: Language -> String -> String
translateWord lang word = fromMaybe word (Map.lookup (map toLower word) dict)
  where
    dict = case lang of
        English -> englishDict
        Spanish -> spanishDict
        French  -> frenchDict
        _       -> Map.empty

-- Função para traduzir uma frase
translateSentence :: String -> String
translateSentence sentence =
    let lang = detectLanguage sentence
        wordsInSentence = words sentence
        translatedWords = map (translateWord lang) wordsInSentence
    in intercalate " " translatedWords

-- Função principal para testar a tradução
main :: IO ()
main = do
    putStrLn "Digite uma frase em inglês, espanhol ou francês:"
    sentence <- getLine
    let translatedSentence = translateSentence sentence
    putStrLn $ "Tradução: " ++ translatedSentence