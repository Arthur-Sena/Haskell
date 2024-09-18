{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Data.List ( intercalate, nub, sort, delete, )
import Control.Monad.Cont (cont)
import Data.Monoid ()
import Text.Read (readMaybe)
import System.Win32 (xBUTTON1)

{--Functors: https://haskell.pesquisa.ufabc.edu.br/haskell/09.functors/ --}

--3.1 Exercício 1 - Escreva a instância de Functor para o tipo SafeNum.

data SafeNum a = NaN | NegInf| PosInf | SafeNum a deriving Show

instance Functor SafeNum where
    fmap f NegInf = NegInf
    fmap f PosInf = PosInf
    fmap f NaN = NaN


{--- - - - - - - - - - - - - - - LISTA 5 (Quiz 4) - - - - - - - - - - - - - - ---}

{--Exercício 1

Considere o tipo data Resultado = Pontuacao Int | Cola, que representa
o resultado das atividades entregues por um aluno. No final do quadrimestre,
deseja-se somar a pontuação de todas as atividades entregues. No entanto, no
caso de Cola, toda a pontuação obtida até o momento deve ser descartada, pois
implica reprovação automática. Implemente uma instância de monoide para
Resultado que modele esse comportamento.
--}

data Resultado = Pontuacao Int | Cola deriving Show

instance Semigroup Resultado where
    (Pontuacao x) <> (Pontuacao y) = Pontuacao (x + y)
    _ <> _                         = Cola

instance Monoid Resultado where
  mempty = Pontuacao 0

{--Exercício 2

Considere o tipo data Set a = Set [a] deriving Eq, que deve representar
um conjunto arbitrário de qualquer a. Invariante: a lista armazenada pelo
construtor Set deve sempre conter elementos únicos e ordenados.
1. Implemente uma instância de Show para o set, que mostre-o conforme o
seguinte exemplo: show Set [1,2,4] →"{1,2,4}" (considere a função
intercalate1 do Data.List).
2. Implemente uma função fromList :: Ord a => [a] -> Set a que gera
um conjunto a partir de uma lista.
3. Implemente uma função member :: Ord a => a -> Set a -> Bool que
retorna se um elemento pertence àquele conjunto
4. Implemente uma função insert :: Ord a => a -> Set a -> Set a que
adiciona o elemento passado por parâmetro no conjunto passado por pa-
râmetro
5. Implemente uma função delete :: Ord a => a -> Set a -> Set a que
faz o inverso da função acima
--}

data Set a = Set [a] deriving Eq

instance Show a => Show (Set a) where
    show (Set xs) = '{' : intercalate "," (show <$> xs) <> "}"

fromListGabarito :: Ord a => [a] -> Set a
fromListGabarito = Set . sort . nub

fromList :: Ord a => [a] -> Set a
fromList = Set

member :: Ord a => a -> Set a -> Bool
member a (Set x) = a `elem` x

insert :: Ord a => a -> Set a -> Set a
insert x (Set y) = Set (x : y)

delete' :: Ord a => a -> Set a -> Set a
delete' x (Set y) = Set (filter (/= x ) y)

deleteGabarito :: Ord a => a -> Set a -> Set a
deleteGabarito x (Set xs) = Set $ delete x xs

{--Exercício 3

Implemente uma instância de Monoid para Set a, dado que a seja Ord, 
utilizando a operação de união de conjuntos
--}

instance Ord a => Semigroup (Set a) where
    (Set x) <> (Set y) = Set (x ++ y)

instance Ord a => Monoid (Set a) where
    mempty = Set []

{--Exercício 4

Você está abrindo uma lanchonete diferente, pois não existe um cardápio fixo.
Você apenas fornece uma lista de ingredientes possíveis, e os clientes podem
combiná-los como bem entenderem. Considere os tipos data Dieta = Vegano
| Vegetariano | Tradicional, e data Lanche = Lanche (Set String) Int
Dieta. Dessa forma, um Lanche é composto por um conjunto de ingredientes,
um preço em centavos e qual a Dieta adequada para aquele Lanche. Implemente
uma instância de monoide para Dieta, considerando o seguinte que duas dietas
são combinadas usando ”denominador comum”, ou seja, duas dietas diferentes
resultam na menos restritiva:
• Se você colocar queijo (alimento vegetariano, mas não vegano) em um
lanche vegano, ele deixa de ser vegano
• Mas colocar queijo em um lanche tradicional não faz com que ele deixe de
ser tradicional
--}

data Dieta = Vegano | Vegetariano | Tradicional deriving Show

data Lanche = Lanche (Set String) Int Dieta deriving Show

instance Semigroup Dieta where
    Tradicional <> _ = Tradicional
    _ <> Tradicional = Tradicional
    Vegetariano <> _ = Vegetariano
    _ <> Vegetariano = Vegetariano
    Vegano <> Vegano = Vegano

instance Monoid Dieta where
    mempty = Vegano

{--Exercício 5

Implemente uma instância de monoide para o Lanche conforme as seguintes
regras para combinar dois Lanche s:
• A lista de ingredientes deve ser combinada usando união de conjuntos
(pode usar sua implementação de (<>))
• O preço deve ser simplesmente somado
• A Dieta deve seguir o (<>) implementado anteriormente
--}

instance Semigroup Lanche where
    (Lanche (Set x) preco1 dieta1) <> (Lanche (Set y) preco2 dieta2) = Lanche (Set (x ++ y)) (preco1 + preco2) (dieta1 <> dieta2)
    --(Lanche s1 preco1 dieta1) <> (Lanche s2 preco2 dieta2) = Lanche (s1 <> s2) (preco1 + preco2) (dieta1 <> dieta2)

instance Monoid Lanche where
    mempty = Lanche (Set []) 0 Tradicional

{--Exercício 6

Defina a instância de Functor o seguinte tipo de árvores binárias:
--}

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a b c) = Node (fmap f a) (f b) (fmap f c)

{--Exercício 7

Defina a função arvorePossui a que retorna True caso a seja um valor 
presente na árvore e False caso contrário.
--}

arvorePossui :: Eq a => Tree a -> a -> Bool
arvorePossui (Leaf t) a = a == t
arvorePossui (Node r x l) a = arvorePossui r a || (x == a) || arvorePossui l a

{--Exercício 8

Defina a função contaLetras :: Tree String -> Tree Int que recebe uma
árvore onde cada nó contém uma string e devolve uma nova árvore onde 
cada nó contém o comprimento das strings que continham na árvore dada 
como entrada.
--}
contaLetras :: Tree String -> Tree Int
contaLetras (Leaf x) = Leaf $ length x
contaLetras (Node r x l) = Node a b c
    where
        a = contaLetras r
        b = length x
        c = contaLetras l

-- Resposta com fmap:
-- contaLetras :: Tree String -> Tree Int
-- contaLetras = fmap length

{--Exercício 9

Defina uma instância de Foldable para Tree. Dica! Para definir a 
instância de Foldable, basta implementar qualquer um dentre os 
seguintes métodos foldMap ou foldr
--}

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

{--Exercício 10

Implemente a função convertString2Int :: String -> Maybe Int, que 
converte, se possível, uma string para inteiro. Você pode usar as 
funções em Text.Read.
--}

convertString2Int :: String -> Maybe Int
convertString2Int = readMaybe

{--Exercício 11

Implemente a função nothingToZero :: Maybe Int -> Int que dado um Maybe
Int devolve o próprio inteiro caso presente ou 0 caso contrário.
--}

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just x) = x

{--Exercício 12

Usando a instância de Foldable (cuja instância você definiu nos exercícios an-
teriores) e o monoide Sum (veja Data.Monoid para mais informações) defina
a função frutasDaArvore :: Tree String -> Int que recebe uma árvore de
strings e devolve o número total de frutas que a árvore tem. Cada nó da árvore
recebida como parâmetro possui uma string que informa a quantidade de fru-
tas que aquele nó possui. Caso a string não corresponda a um número inteiro,
ignore o nó e continue a contagem.
--}

newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show)

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0


frutasDaArvore :: Tree String -> Int
frutasDaArvore = getSum . foldMap (Sum . nothingToZero . convertString2Int)

{--Exercício 13

Escreva as instâncias de Functor e Applicative para o tipo ZipList, 
no qual a função pura faz uma lista infinita de cópias do argumento, 
e o operador <*> aplica cada função argumento no valor 
correspondente na mesma posição.
--}

newtype ZipList a = Z [a] deriving Show

-- fmap :: (a -> b) -> ZipList a -> ZipList b
instance Functor ZipList where
    fmap g (Z xs) = Z (fmap g xs)

-- pure :: a -> ZipList a
instance Applicative ZipList where
    pure x = Z (repeat x)
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

{--Exercício 14

Dado o tipo Expr, que contém variáveis de um tipo a, defina 
instâncias para esse tipo de Functor, Applicative
--}

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap f (Var x) = Var (f x)
    fmap _ (Val n) = Val n
    fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
    pure = Var
    Var g <*> e = fmap g e

{--Exercício 15

Defina instâncias de Functor, Applicative para os seguintes tipos:
--}

newtype Identity a = Identity a
data Pair a = Pair a a

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    Identity a <*> x = fmap a x

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair x y) <*> (Pair a b) = Pair (x a) (y b)

{--Exercício 16

Run-length encoding, ou RLE para os íntimos, é um método de compactação
sem perdas que é muito utilizado para comprimir dados com muitas sequencias
repetitivas valores. Talvez o seu uso mais comum tenha sido para compressão
de arquivos de imagem simples como ícones. Considere o ADT data RLE a =
Repeat Int a (RLE a) | End deriving (Eq, Show). Caso se trate de uma
sequência vazia utilizamos o construtor End e caso se trate de uma sequência
não vazia, utilizamos o construtor Repeat com o inteiro contendo o número de
elementos repetidos do tipo a seguido de outro Repeat ou um End para indicar
o termino da sequência. Implemente a função rleCons :: Eq a => a -> RLE
a -> RLE a cujo comportamento é análogo à função cons para listas (:).
--}

data RLE a = Repeat Int a (RLE a) | End deriving (Eq, Show)

rleCons :: Eq a => a -> RLE a -> RLE a
rleCons x End = Repeat 1 x End
rleCons x r@(Repeat i y ys)
    | x == y = Repeat (i + 1) y ys
    | otherwise = Repeat 1 x r

{--Exercício 17

Implemente uma instância de Foldable para RLE.
--}

instance Foldable RLE where
    foldMap _ End = mempty
    foldMap f (Repeat n x xs) = mconcat (replicate n (f x)) <> foldMap f xs

{--Exercício 22

Suponha que você está cursando a disciplina de Arquitetura de Computadores
e precisa implementar uma hierarquia de memória para o seu emulador MIPS.
Você desenvolveu a seguinte estrutura de dados:

data Memory a = UnifiedCache a (Memory a) | SplitCache a a (Memory a) | RAM a

Essa estrutura tem a propriedade interessante de que sempre vai terminar
na memória RAM, e podemos ter quantos níveis de cache quisermos. Por exemplo,
se quisermos armazenar os acessos a cada um dos níveis de memória, podemos
fazer: SplitCache 5 2 (UnifiedCache 10 (RAM 5)).
Imagine que queremos calcular o total de acessos feitos a todos os níveis de
memória usando:
foldMap Sum $ SplitCache 5 2 (UnifiedCache 10 (RAM 5))
Implemente instâncias de Functor, Foldable e Traversable para a estru-
tura Memory.
--}

data Memory a = UnifiedCache a (Memory a) | SplitCache a a (Memory a) | RAM a

instance Foldable Memory where
    foldMap :: Monoid m => (a -> m) -> Memory a -> m
    foldMap f (RAM x) = f x
    foldMap f (SplitCache x y memory) = (f x) <> (f y) <> (foldMap f memory)
    foldMap f (UnifiedCache x memory) = f x <> foldMap f memory

instance Functor Memory where
    fmap :: (a -> b) -> Memory a -> Memory b
    fmap f (RAM x) = RAM (f x)
    fmap f (SplitCache x y memory) = SplitCache (f x) (f y) (fmap f memory)
    fmap f (UnifiedCache x memory) = UnifiedCache (f x) (fmap f memory)

instance Traversable Memory where
    traverse :: Applicative f => (a -> f b) -> Memory a -> f (Memory b)
    traverse fx (UnifiedCache x m) = UnifiedCache <$> (fx x) <*> (traverse fx m)
    traverse fx (SplitCache x y m) =
        SplitCache <$> (fx x) <*> (fx y) <*> (traverse fx m)
    traverse fx (RAM x) = RAM <$> fx x

--FOLDABLE = Tipo a -> (foldMap f Tipo a b) == (f a) <> (f b)
--FUNCTOR  = Tipo a -> (fmap f Tipo a) == Tipo (f a)

{--Exercício 20

Obedecendo as leis descritas no início da lista de exercícios, 
escreva  instâncias de Functor e Applicative para o tipo 
data Fantasma a = Fantasma
--}
data Fantasma a = Fantasma

instance Functor Fantasma where
    fmap f _ =  Fantasma

instance Applicative Fantasma where
    pure _ = Fantasma
    _ <*> _ = Fantasma