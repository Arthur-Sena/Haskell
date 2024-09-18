# Monoids em Haskell

## O que é um Monoid?

Em Haskell, um `Monoid` é uma estrutura algébrica com uma operação binária associativa e um elemento neutro. Em termos mais simples, um `Monoid` é uma classe de tipos que possui duas propriedades principais:

1. **Associatividade:** A operação binária (`<>`) deve ser associativa, ou seja, a ordem em que você aplica a operação não deve alterar o resultado.

   \[
   (a <> b) <> c = a <> (b <> c)
   \]

2. **Elemento Neutro:** Deve haver um elemento neutro (`mempty`) tal que, quando combinado com qualquer outro elemento utilizando a operação binária, retorne o outro elemento.

   \[
   mempty <> a = a <> mempty = a
   \]

### Exemplo: Monoid para Listas

Em Haskell, listas formam um `Monoid`, onde a operação binária é a concatenação (`++`), e o elemento neutro é a lista vazia (`[]`).

```haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)


# Função `pure` em Haskell

## Introdução

A função `pure` em Haskell é parte da classe de tipos `Applicative`, que é usada para trabalhar com valores contextuais, ou seja, valores que estão em algum tipo de "contexto", como dentro de uma lista, um `Maybe`, ou uma função.

## Definição e Propósito

A função `pure` tem o seguinte tipo:

```haskell
pure :: Applicative f => a -> f a
```

Ela pega um valor normal do tipo `a` e o coloca dentro de um contexto (ou "container") do tipo `f`. O tipo `f` é qualquer tipo que pertence à classe `Applicative`.

## Intuição

- **`pure`** pega um valor e o "injeta" em um contexto mínimo que ainda mantém esse valor.
  - Para `Maybe`, o contexto mínimo é `Just`.
  - Para listas, o contexto mínimo é uma lista com um único elemento.
  - Para funções, o contexto mínimo é uma função constante.

## Exemplos de `pure`

### `Maybe`

No contexto de `Maybe`, `pure` coloca um valor dentro de um `Just`.

```haskell
pure 3 :: Maybe Int
-- Resultado: Just 3
```

### Listas

Para listas, `pure` coloca o valor em uma lista de um único elemento.

```haskell
pure 3 :: [Int]
-- Resultado: [3]
```

### Funções

Para funções, `pure` cria uma função constante que sempre retorna o valor dado.

```haskell
pure 3 :: a -> Int
-- Resultado: \_ -> 3
```

## Relação com `Applicative`

A `pure` é uma das duas funções fundamentais da classe `Applicative`, a outra sendo `(<*>)`. Ela é usada para introduzir um valor em um contexto antes de aplicar funções que também operam nesse contexto.

Por exemplo, se você tem uma função `f` que opera sobre valores simples e quer aplicá-la a valores que estão dentro de um `Maybe`, você pode usar `pure` para transformar a função em uma função que opera dentro do contexto `Maybe`:

```haskell
import Control.Applicative

f :: Int -> Int -> Int
f x y = x + y

-- Aplicando 'f' a valores em contextos Maybe
resultado :: Maybe Int
resultado = pure f <*> Just 2 <*> Just 3
-- Resultado: Just 5
```

## Resumo

- **`pure`** pega um valor e o coloca dentro de um contexto mínimo aplicável.
- Ela é usada em combinação com outras operações `Applicative` para trabalhar com valores dentro de contextos como `Maybe`, listas, ou funções.
- `pure` é essencial para funções que precisam operar dentro de um contexto, permitindo que você comece com um valor simples antes de aplicá-lo de forma mais complexa.
