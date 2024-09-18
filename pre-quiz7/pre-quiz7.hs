import Control.Monad.State
import Control.Monad.RWS
import Data.Maybe
{--
    Estamos simulando um sistema de comércio em um RPG, e queremos 
simular lojas e clientes.
    Considere os tipos Produto e Loja, fornecidos abaixo:

    Um Produto contém apenas um nome e um preco, enquanto uma Loja 
possui o seu estoque (uma lista de Produto) e um caixa, representando
quanto dinheiro aquela loja tem em caixa.
    Considere ainda as funções comprar e vender, cujas 
implementações são dadas abaixo:
--}

data Produto = Produto
    { nome :: String
    , preco :: Int
    } deriving (Show, Eq)

data Loja = Loja
    { caixa :: Int
    , estoque :: [Produto]
    } deriving (Show, Eq)

comprar :: String -> State Loja (Maybe Produto)
comprar nomeProduto = state realizaCompra
    where
        realizaCompra :: Loja -> (Maybe Produto, Loja)
        realizaCompra lj =
            case findAndRemoveProduto $ estoque lj of
                Nothing -> (Nothing, lj)
                Just (p, novoEstoque) -> (Just p, Loja novoCaixa novoEstoque)
                    where novoCaixa = caixa lj + preco p
        findAndRemoveProduto :: [Produto] -> Maybe (Produto, [Produto])
        findAndRemoveProduto ps = go ps []
            where
                go [] _ = Nothing
                go (x:xs) acc
                    | nome x == nomeProduto = Just (x, reverse acc ++ xs)
                    | otherwise = go xs (x : acc)

vender :: String -> Int -> State Loja Int
vender nomeProduto valor = state realizaVenda
    where
        realizaVenda :: Loja -> (Int, Loja)
        realizaVenda lj
            | valor <= caixa lj =
            ( valor
            , Loja (caixa lj - valor) ((Produto nomeProduto valor) : estoque lj))
            | otherwise = (0, lj)

vendeEspada :: Cliente
vendeEspada = do
    valorVendido <- vender "Espada" 10
    return $ valorVendido > 0

compraEscudo :: Cliente
compraEscudo = do
    maybeProduto <- comprar "Escudo"
    return $ isJust maybeProduto

shepard :: Cliente
shepard = do
    valorEspada <- vender "Espada" 10
    valorEscudo <- vender "Escudo" 5
    return $ valorEspada + valorEscudo > 0

type Cliente = State Loja Bool

shepardMeu :: Cliente
shepardMeu = do
    valorVendido1 <- vender "Espada" 10
    valorVendido2 <- vender "Escudo" 5
    return (valorVendido1 > 0 || valorVendido2 > 0)

{--Exercício 1
Implemente o cliente frisk que tenta vender uma "Espada" por 10 moedas e um "Escudo"
por 5, e só sai satisfeito se vender os dois. Note que, mesmo que ele não consiga vender
a Espada, ele ainda deve tentar vender o escudo (mesmo que isso signifique que isso não
mude se ele sairá insatisfeito).
--}

frisk :: Cliente
frisk = do
    frstVenda <- vender "Espada" 10
    scndVenda <- vender "Escudo" 5
    return (frstVenda > 0 && scndVenda > 0)

{--Exercício 2
Implemente o cliente loneWanderer que tenta vender uma "Espada" por 10 moedas. Se
for bem sucedido, tenta comprar um "Escudo". Ele sai satisfeito se conseguir sair de lá
com o Escudo.
--}

loneWanderer :: Cliente
loneWanderer = do
    valorEspada <- vender "Espada" 10
    if valorEspada == 0
    then return False
    else isJust <$> comprar "Escudo"

{--Exercício 3
Implemente o cliente dragonborn que tenta vender o máximo de "Queijo" que conseguir,
por 3 moedas cada. Ele sairá satisfeito de qualquer forma, independente de quantos
queijos vender.
--}

dragonborn :: Cliente
dragonborn = do
    venda <- vender "Queijo" 3
    if venda > 0 
    then dragonborn
    else return True

{--Exercício 4
Implemente o cliente geralt que tenta vender 10 "Espada" por 15 moedas cada. Se ele
conseguir vender ao menos 6 espadas, ele deve tentar comprar um "Escudo". Ele sai
satisfeito se conseguir sair de lá com o Escudo.
--}

geralt :: Cliente
geralt = do
    valorEspadas <- sum <$> (sequenceA $ replicate 10 $ vender "Espada" 15)
    if valorEspadas >= (15 * 6)
    then isJust <$> comprar "Escudo"
    else return False

--------------------------------------------------------

newtype MudaLista a = MudaLista { runMudaLista :: [Int] -> ([Int], a)}

desempilha :: MudaLista Int
desempilha = MudaLista $ \(x:xs) -> (xs, x)

empilha :: Int -> MudaLista ()
empilha x = MudaLista $ \l -> (x : l, ())


instance Functor MudaLista where
    fmap g (MudaLista f) = MudaLista $ \l -> fmap g (f l)