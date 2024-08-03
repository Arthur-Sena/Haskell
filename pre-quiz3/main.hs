data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero                -- 0 multiplicado por qualquer número é 0
mult _ Zero = Zero                -- qualquer número multiplicado por 0 é 0
mult (Succ m) n = add n (mult m (n-2) -- m vezes n é n adicionado a m vezes n