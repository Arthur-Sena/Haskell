import Data.List (nub, (\\), transpose, intercalate, intersperse)
{--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs

Se a lista passada como argumento é infinita, use foldr
Se o operador utilizado pode gerar curto-circuito, use foldr
Se a lista é finita e o operador não irá gerar curto-circuito, use foldl
Se faz sentido trabalhar com a lista invertida, use foldl
--}

--Como podemos implementar length utilizando foldr?
length1 :: [a] -> Int
length1 = foldr (\c b -> b + 1) 0

--Reescreva a função reverse utilizando foldr:
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = foldr (\x xs -> xs ++ [x]) [] (x:xs)

--Como ficaria a função length utilizando foldl?
length2 :: [a] -> Int
length2 = foldl (\a _ -> a + 1) 0

--E a função Reverse?
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = foldl (\b a -> a:b) [] (x:xs)


{--
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

Uma função que transforma uma lista do tipo a para o tipo b utilizando uma função f :: a -> b.
Com isso temos uma visão mais clara das transformações feitas em listas:

Exemplo:
> map (+1) [1,2,3]
[2,3,4]

> map even [1,2,3]
[False, True, False]

> map reverse ["ola", "mundo"]
["alo", "odnum"]

filter :: (a -> Bool) -> [a] -> [a]
filter p []                 = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs
--}

--Tipos de dados algebricos

{--
Crie um tipo Fuzzy que pode ter os valores Verdadeiro; Falso; 
ou Pertinencia Double que define um intermediário entre Verdadeiro e Falso.

Crie uma função fuzzifica que recebe um Double e retorna Falso caso o valor seja menor ou igual a 0,
 Verdadeiro se for maior ou igual a 1 e Pertinencia v caso contrário.
--}

data Fuzzy = Verdadeiro | Falso | Pertinencia Double
    deriving Show

fuzzifica :: Double -> Fuzzy
fuzzifica x | x <= 0 = Falso
            | x >= 1 = Verdadeiro
            | otherwise = Pertinencia x

{-- Ex 3.2: Altere a função contem levando em conta que essa é uma árvore de busca,
ou seja, os nós da esquerda são menores ao nó atual, e os nós da direita são maiores.

contem :: Eq a => Tree a -> a -> Bool
contem (Leaf y) x     = x == y
contem (Node l y r) x = x == y || l `contem` x
                               || r `contem` x
--}
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

contem :: Ord a => Tree a -> a -> Bool
contem (Leaf y) x                 = x == y
contem (Node l y r) x | x == y    = True
                      | x < y     = l `contem` x
                      | otherwise = r `contem` x

{--Lista 3 --}
--Exercício 1
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)
{--
=> mult 4 2
=> add 4 (mult 4 1)
=> add 4 ( add 4 (mult 4 1) )
=> add 4 ( add 4 (add 4 (mult 4 0)) )

=> add 4 ( add 4 (add 4 Zero) )
=> add 4 ( add 4 (add 4 Zero) )
=> add 4 ( add 4 ( Succ (add 3 Zero)) )
=> add 4 ( add 4 ( Succ (Succ (add 2 Zero)) )
=> add 4 ( add 4 ( Succ (Succ (Succ (add 1 Zero))) )
=> add 4 ( add 4 ( Succ (Succ (Succ (Zero))) ))

=> add 4 ( add 4 3 ))
=> add 4 ( Succ (add 3 4) )
=> add 4 ( Succ ( Succ (add 2 4) ) )
=> add 4 ( Succ ( Succ ( Succ (add 1 4) ) ) )
=> add 4 ( Succ ( Succ ( Succ ( Succ (add 0 4)) ) ) )
=> add 4 ( Succ ( Succ ( Succ ( Succ (4)) ) ) )
=> add 4 8

--}

--Exercício 2
data Ordering1 = LT1 | EQ1 | GT1 deriving Show

--que decide se um valor do tipo ordering é menor que (LT), igual a (EQ) ou maior que (GT) outro valor. 
compare1 :: Ord a => a -> a -> Ordering1
compare1 x y | x < y  = LT1
            | x == y = EQ1
            | otherwise = GT1

--Usando essa função, redefina
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare1 x y of
                        LT1 -> x `occurs` l
                        EQ1 -> True
                        GT1 -> x `occurs` r


--Ex 3
--que retorna uma lista ordenada com os elementos da árvore percorrida em ordem

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


{--
 Exercício 4
 
 Considere o tipo de árvores binárias
 Uma árvore é dita balanceada se o número de folhas das sub-árvores à 
 esquerda e à direita em cada nó difere em, no máximo, um. As folhas 
 estão trivialmente balanceadas.  
 
 Defina a função que verifica se uma árvore binária está balanceada ou não.
 Dica: primeiro defina uma função que retorna o número de folhas em uma árvore.
--}

data Tree4 a = Leaf4 a | Node4 (Tree4 a) (Tree4 a) deriving (Show)

leaves :: Tree4 a -> Int
leaves (Leaf4 _) = 1
leaves (Node4 l r) = leaves l + leaves r


balanced :: Tree4 a -> Bool
balanced (Leaf4 _) = True
balanced (Node4 l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

{--
 Exercício 5
 
 que converte uma lista não vazia em uma árvore balanceada. 
 Dica: primeiro, defina uma função que divide uma lista em duas partes,
 cujo tamanho difere em no máximo um.
--}

splitList :: [a] -> ([a], [a])
splitList x = (take (divisor x) x, take ((divisor x) + 1) (drop (divisor x) x))
    where
        divisor :: [a] -> Int
        divisor x = div (length x) 2


balance :: [a] -> Tree4 a
balance [x] = Leaf4 x
balance xs = (Node4 ( balance x') (balance y'))
    where (x', y') = splitList xs


showBoard :: [[Maybe Int]] -> String
showBoard board = unlines $ map showRow board
  where
    showRow = concatMap showCell
    showCell Nothing = ". "
    showCell (Just n) = show n ++ " "


{--
formatGrid :: Board -> String
formatGrid grid = unlines (map formatRow grid)

formatRow :: [Int] -> String
formatRow row = intercalate " " (map show left ++ ["|"] ++ map show right)
  where (left, right) = splitAt 3 row--}

{--
formatGrid :: [[Int]] -> String
formatGrid grid =   unlines
                    $ map show top ++ ["_ _ _ _ _ _ _"] ++ map show bot
      where (top, bot) = splitAt 3 (map formatRow grid)

formatRow :: [Int] -> String
formatRow row = intercalate " " (map show left ++ ["|"] ++ (if length right > 3 then map show right else formatRow right))
  where (left, right) = splitAt 3 row
--}


formatGrid :: [[Int]] -> String
formatGrid grid = unlines (intersperse separator (map formatRow grid))
  where
    formatRow row = intercalate " " (map show row)
    separator = replicate (length (head grid) * 2 - 1) '_' ++ "\n"




