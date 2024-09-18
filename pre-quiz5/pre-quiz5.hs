{--Exercício 1
Implemente instâncias de Functor, Applicative e Monad para o tipo Caixa
--}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

newtype Caixa a = Caixa a deriving (Eq, Show)

instance Functor Caixa where
    fmap x (Caixa a) = Caixa (x a)

instance Applicative Caixa where
    pure = Caixa
    (Caixa x) <*> (Caixa y) = Caixa (x y)

instance Monad Caixa where
    (Caixa a ) >>= f = f a

{--Exercício 2
Dado o tipo
que contém variáveis de um tipo a, defina instâncias para esse tipo de Functor, Applicative e
Monad. (Dica: você já criou as instâncias de Functor e Applicative na lista anterior).
--}
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap _ (Val i) = Val i
    fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
    pure = Var
    (Var a) <*> f = fmap a f

instance Monad Expr where
    (Var a)     >>= f = f a
    (Val a)     >>= _ = Val a
    (Add a b)   >>= f = Add (a >>= f) (b >>= f)

{--Exercício 3
Defina instâncias de Functor, Applicative e Monad para os seguintes tipos. (Dica: você já
criou as instâncias de Functor e Applicative na lista anterior).
--}
newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity x) <*> f = fmap x f

instance Monad Identity where
    return = Identity
    (Identity a) >>= f = f a

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure x = Pair x x
    (Pair x y) <*> (Pair i j) = Pair (x i) (y j)

instance Monad Pair where
    return = pure
    (Pair x y) >>= f = f x

{--Exercício 4
Obedecendo as leis descritas no início da lista de exercícios, escreva instâncias de Functor,
Applicative e Monad para o tipo data Fantasma a = Fantasma. (Dica: você já criou as
instâncias de Functor e Applicative na lista anterior).
--}
data Fantasma a = Fantasma

instance Functor Fantasma where
    fmap _ _ = Fantasma

instance Applicative Fantasma where
    pure _ = Fantasma
    _ <*> _ = Fantasma

instance Monad Fantasma where
    return = pure
    Fantasma >>= _ = Fantasma

{--Exercício 5
Obedecendo as leis descritas acima, escreva instâncias de Functor, Applicative e Monad
para o tipo 
--}
data Duo a = Duo (Bool -> a)

instance Functor Duo where
    fmap f (Duo x) = Duo (f . x)

instance Applicative Duo where
    pure v = Duo $ const v
    (Duo f) <*> (Duo g) = Duo $ \b -> f b (g b)

runDuo :: Duo a -> Bool -> a
runDuo (Duo f) = f

instance Monad Duo where
    a >>= f = Duo $ \ b -> runDuo (f $ runDuo a b) b

{--Exercício 6
Você está escrevendo uma lib que faz requisições HTTP para servidores. Você definiu o tipo:

Defina instâncias de Functor, Applicative e Monad para este tipo. Caso haja ao menos um
Error envolvido nas funções (<*>) e (>>=), o resultado deverá ser Error. Caso não haja
nenhum Error e haja um Loading a resposta deverá ser Loading
--}

data Request a = Loading | Error | Success a

instance Functor Request where
    fmap _ Loading      = Loading
    fmap _ Error        = Error
    fmap f (Success a)  = Success (f a)

instance Applicative Request where
    pure = Success
    Error       <*> _       = Error
    _           <*> Error   = Error
    Loading     <*> _       = Loading
    _           <*> Loading = Loading
    (Success x) <*> f       = fmap x f

instance Monad Request where
    return = pure
    (Success a) >>= f = f a
    Loading     >>= _ = Loading
    Error       >>= _ = Error

{--Exercício 7
• OK - Crie uma instância de Functor que aplica a função passada em todas as ”posições”.
• Ok - Crie uma instância de Eq manualmente para Bolso, que compara apenas o valor mais
a direita. Ou, em outras palavras: Um 5 == Dois _ 5 == Tres _ _ 5 e Dois 10 5 ==
Tres _ 1 5.
• Crie uma instância de Monad para Bolso, dado que, na hora de definir o (>>=), sempre
o valor ”mais a direita” deve ser enviado para a função. Você precisará também fazer uma
instância de Applicative seguindo a mesma lógica.
--}

data Bolso a = Um a | Dois a a | Tres a a a deriving (Show)

instance Functor Bolso where
    fmap f (Um x)       = Um   (f x)
    fmap f (Dois x y)   = Dois (f x) (f y)
    fmap f (Tres x y z) = Tres (f x) (f y) (f z)

instance Eq a => Eq (Bolso a) where
    Um a == Um b = a == b
    (Dois _ a) == x = Um a == x
    (Tres _ _ a) == x = Um a == x
    x == y = y == x

--bolsoEq :: Eq a => Bolso a -> Bolso a -> Bool
--bolsoEq x y = x3 == y3
--    where 
--        x3 = case x of
--            Um a -> a
--            Dois _ b -> b
--            Tres _ _ c -> c
--        y3 = case y of
--            Um a -> a
--            Dois _ b -> b
--            Tres _ _ c -> c

instance Applicative Bolso where
    pure = Um
    (Um a)       <*> f = fmap a f
    (Dois x y)   <*> f = fmap y f
    (Tres x y z) <*> f = fmap z f

instance Monad Bolso where
    return = Um
    (Um x)       >>= f = f x
    (Dois _ y)   >>= f = f y
    (Tres _ _ z) >>= f = f z

{--Exercício 8
Seu professor de educação física entrega uma lista de pesos e alturas e pede para você calcular
o IMC de cada pessoa. Inicialmente, é dito que o dataset é uma lista de tuplas que representa
uma tabela. Para cada linha há três campos: um nome [Char], uma altura Double e um peso
Double. Porém, ao ter o dataset em mãos, descobre-se que algumas células estão branco.
Além de criar o próprio tipos adicionais conforme necessário, você deve criar uma função
imc que recebe uma lista de tuplas, em que cada posição se refere a um dos campos citados
anteriormente. Porém, em qualquer um deles, pode haver Nothing. A função deve retornar uma
lista com o IMC corresponde para cada pessoa, quando isso for possível. Em outras palavras, a
função precisa ter o tipo:

Observações importantes:
• O IMC é calculado como Peso/Altura2.
• Caso o nome esteja ausente, deve ser possível calcular o IMC de qualquer maneira
--}

type Nome   = String
type Peso   = Double
type Altura = Double
type IMC    = Double

--imc :: [(Maybe Nome, Maybe Peso, Maybe Altura)] -> [Maybe IMC]
--imc [(n p a)] = fmap IMC (p a)
--    where IMC = if (p == Nothing || a == Nothing) then Nothing
--                else p * (a * a)

imc :: [(Maybe Nome, Maybe Peso, Maybe Altura)] -> [Maybe IMC]
imc = fmap calculate where
    calculate (_, mp, ma) =
        if mp == Nothing || ma == Nothing
        then Nothing
        else do
            let Just p = mp
            let Just a = ma
            return (p / (a ** 2))

--Gabarito:
--imc :: [(Maybe Nome, Maybe Peso, Maybe Altura)] -> [Maybe IMC]
--imc = fmap calculate where
--    calculate (_, mp, ma) = do
--        p <- mp
--        a <- ma
--        return $ p/(a**2)

{--Exercício 9
Implemente a função azul. O seu comportamento pode ser inferido da sua 
assinatura dada abaixo.
--}

azul :: Monad m => m (m a) -> m a
azul m = m >>= id

{--Exercício 10
Implemente a função amarelo. O seu comportamento pode ser inferido da sua 
assinatura dada abaixo.
--}

amarelo :: Monad m => (a -> b) -> m a -> m b
amarelo f ma = do
    a <- ma
    return (f a)

--Gabarito
--amarelo :: Monad m => (a -> b) -> m a -> m b
--amarelo = fmap

{--Exercício 11
Implemente a função vermelho. O seu comportamento pode ser inferido da sua 
assinatura dada abaixo
--}

vermelho :: Monad m => (a -> b -> c) -> m a -> m b -> m c
vermelho f ma mb = fmap f ma <*> mb

{--Exercício 12
Implemente a função verde. O seu comportamento pode ser inferido da sua assinatura dada
abaixo.
--}

verde :: Monad m => m a -> m (a -> b) -> m b
verde ma mf = do
    a <- ma
    f <- mf
    return (f a)

verde :: Monad m => m a -> m (a -> b) -> m b
verde ma mf = 
    ma >>= \a -> 
    mf >>= \f -> 
    return (f a)

{--Exercício 13
Implemente a função laranja. O seu comportamento pode ser inferido da sua 
assinatura dada abaixo. Dica, use recursão.
--}

laranja :: Monad m => [m a] -> m [a]
laranja [] = return []
laranja (x:xs) = do 
    a <- x
    xs' <- laranja xs
    return ( a : xs' )

{--Exercício 14
Implemente a função roxo. O seu comportamento pode ser inferido da sua assinatura 
dada abaixo. Dica, use a funçao laranja e amarelo.
--}

roxo :: Monad m => [a] -> (a -> m b) -> m [b]
roxo xs f = laranja $ amarelo f xs

